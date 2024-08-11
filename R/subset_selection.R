#' Create row indices for K folds of a dataset
#'
#' @param data Dataset to split into folds
#' @param K Number of folds. Default is 10. 
#' @return A list with the row indices for K folds of data
#' @export
sample_k_fold_indices = function(data, K = 10){
  
  # Get a vector of row indices for data
  indices = seq.int(nrow(data))
  
  # Create a vector with row indices for folds
  indices_for_folds = split(sample(indices, length(indices)), rep(1:K, ceiling(length(indices)/K)))
  return(indices_for_folds)
  
}

#' Separate a dataset into a training and test set using specified folds of the data
#'
#' @param data Matrix or table to split into folds
#' @param k_folds_indices A list with the indices for different folds of the data. 
#' Each index should occur in only one fold. 
#' @param test_fold Number of the fold from k_folds_indices which should be treated as the test set.
#' @return A list with the training and test set splits of the data. 
#' @export
separate_training_and_test_data = function(data, k_folds_indices, test_fold){
  
  # Check that there are indices for each row in data
  if(!all(sort(unlist(k_folds_indices)) == seq.int(nrow(data)))){
    stop("Indices in k_folds_indices should cover all rows in data and each index should occur in only a single fold")
  }
  
  # Get a vector of row indices for data
  test_indices = k_folds_indices[[test_fold]]
  
  # Separate data into training and test data
  training_data = data[-test_indices, ]
  test_data = data[test_indices, ]
  
  # Return a list with the training and test data sets
  return(list(training = training_data, test = test_data))
  
}

#' Calculate MSE using new data for the best model of an indicated size identified by subset selection
#'
#' @param data Matrix or data.frame containing the response and features as columns.
#' @param response Name of the column containing the response in data. 
#' @param regsubsets A regsubsets object.
#' @param n The model size to evaluate. 
#' @return The calculated MSE.
#' @export
mse_for_subset = function(data, response, regsubsets, n){
  
  # Extract the formula from the regsubsets object
  formula = as.formula(regsubsets$call[[2]])
  
   # Create a model matrix for the data
  model_matrix = model.matrix(formula, data = data)
  
  # Get the coefficients of the model of the indicated size
  if(n == 0){
    coefs = setNames(mean(data[, response]), "(Intercept)")
  } else {
    coefs = coef(regsubsets, n)
  }
  
  # Calculate the predicted values using the coefficients
  predictions = as.numeric(model_matrix[, names(coefs), drop = FALSE] %*% coefs)
  
  # Calculate the residuals and return the MSE
  residuals = data[, response] - predictions
  return(mean(residuals^2))
  
}

#' Perform subset selection using cross-validated errors to evaluate the best model
#'
#' @param data Matrix or data.frame containing the response and features as columns.
#' @param response Name of the column containing the response in data. 
#' @param K Number of folds. Default is 10. 
#' @param method Type of feature selection to perform. Same as for leaps::regsubsets. Default is forward selection.
#' @param nvmax Maximum number of features to consider. Default is all features in data. 
#' @param plot_mean_mse A logical value indicating whether to plot the mean mse for each size model. Default is TRUE. 
#' @param ncores Number of cores to use to process folds separately in parallel. Default is process each fold sequentially.
#' Cannot be greater than K. 
#' @return A data.frame with the CV MSE for best model of the indicated size found for each fold of data.  
#' @export
cross_validated_mse_for_subset_selection = function(data, response, K = 10, method = "forward", nvmax = NULL, plot_mean_mse = T, ncores = 1){
  
  # Check that response is the name of a column in data
  if(!response %in% names(data)){stop(paste(response, "is not the name of a column in data"))}
  
  # Give a warning if ncores is greater than K and set ncores equal to K.
  if(ncores > K){
    warning(sprintf("Provided value for ncores (%s) is greater than K (%s). Will just use %s cores.", ncores, K, K))
    ncores = K
  }
  
  # Set nvmax equal to the number of features in data if it is not provided
  if(is.null(nvmax)){nvmax = ncol(data) - 1}
  
  # Split the data into K folds
  folds = suppressWarnings(sample_k_fold_indices(data, K))
  
  # Create a matrix to store the CV results for each fold for each model size
  cv_results = matrix(NA, nrow = nvmax + 1, ncol = K)
  
  # Make a cluster with the specified number of cores and register it
  if(ncores > 1){
    cl = parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl, ncores)
    on.exit(parallel::stopCluster(cl))
    `%dopar%` = foreach::`%dopar%`
  } else {
    `%dopar%` <- foreach::`%do%`
  }
  
  # Perform subset selection on each fold
  mse_for_cv_models = foreach::foreach(fold = folds, i = seq_along(folds), .packages = "featureSelector") %dopar% {
    
    # Separate data into train and test sets
    training = separate_training_and_test_data(data, k_folds_indices = folds, test_fold = i)$training
    test = separate_training_and_test_data(data, k_folds_indices = folds, test_fold = i)$test
    
    # Perform subset selection with indicated method
    formula = as.formula(paste(response, "~ ."))
    subset_selection_results = leaps::regsubsets(formula, data = training, method = method, nvmax = nvmax)
    
    # Replace formula variable name with the actual formula in the call
    subset_selection_results$call[[2]] = formula
    
    # Calculate the MSE for best model of each size
    mse_for_models = sapply(0:nvmax, function(i) 
      mse_for_subset(data = test, response = response, regsubsets = subset_selection_results, n = i))
    mse_for_models
  }
  
  # Convert the results into a data.frame
  mse_for_cv_models = data.frame(mse_for_cv_models)
  names(mse_for_cv_models) = paste0("fold_", 1:K)
  row.names(mse_for_cv_models) = paste0(0:nvmax, "_features")
  
  # Plot mean values for folds if indicated
  if(plot_mean_mse){
    plot(rowMeans(mse_for_cv_models), xlab = "Number of Features", ylab = "Mean mse")
  }
  
  return(mse_for_cv_models)
  
}

#' Select the best model size from cross-validated errors for subset selection 
#'
#' @param cv_subset_results A data.frame with the results of cross_validated_mse_for_subset_selection.
#' @param one_sd_error_rule A logical value indicating whether to use the one-standard-error rule to select the smallest
#' model where the mean CV MSE is within one standard error of the model with the lowest mean CV MSE. Default is FALSE.
#' @return An integer with the identified best model size. 
#' @export
select_best_model_size_from_cv_subset_results = function(cv_subset_results, one_sd_error_rule = FALSE){
  
  # Calculate the mean MSE for each model size across the folds and identify the best model size
  mean_mse = rowMeans(cv_subset_results)
  best_size = which.min(mean_mse)
  
  # Use one-standard-error rule to select the best model if one_sd_error_rule is TRUE.
  if(one_sd_error_rule){
    
    # Calculate the standard error of the mean CV MSE estimate for each model size
    message("Using one-standard-error rule")
    sd_errors = apply(cv_subset_results, 1, sd)/sqrt(ncol(cv_subset_results))
    
    # Find the smallest model with a mean CV MSE within 1 standard error of the smallest value
    one_sd_model = names(which.min(which(mean_mse - sd_errors <= mean_mse[best_size])))
    best_size = which(names(mean_mse) == one_sd_model)
  }
  
  # Return the best model size
  message(paste("Best model size is", best_size - 1))
  return(unname(best_size) - 1)
  
}

#' Create a lm object from the best identified model of a specified size by subset selection
#'
#' @param regsubsets A regsubsets object.
#' @param n The size of the model.
#' @param data Data used to fit the model. Should be the same as used to create the regsubsets object.
#' @return An lm object.
#' @export
extract_best_model_from_subset_selection = function(regsubsets, n, data){
  
  # Check that n is not greater than the biggest model size considered
  if(n > regsubsets$nvmax - 1){stop("n cannot be greater than the maximum model size of the regsubsets object")}
  
  # Get the name of the response variable 
  response = all.vars(formula(regsubsets$call[[2]]))[1]
  
  # Create a formula for the model
  if(n == 0){
    # Create a formula for the null model if n is 0
    formula = as.formula(paste(response, "~", "1"))
  } else {
    # Get the names of the features in the best model of size n and create a formula from them
    features = names(coef(regsubsets, n))[-1]
    formula = as.formula(paste(response, "~", paste(features, collapse = " + ")))
  }
  
  # Create the lm object and return
  model = lm(formula = formula, data = data)
  return(model)
  
}

# broom::tidy(do.call(anova, lapply(0:6, function(x) extract_best_model_from_subset_selection(regsubsets, x, mtcars))))