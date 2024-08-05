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
#' @param data Matrix or data.frame containing new data the response and features as columns.
#' @param response Name of the column containing the response in data. 
#' @param regsubsets A regsubsets object
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

#' Perform subset selection using cross-validated errors to evaulate the best model.
#'
#' @param data Matrix or data.frame containing the response and features as columns.
#' @param response Name of the column containing the response in data. 
#' @param K Number of folds. Default is 10. 
#' @param method Type of feature selection to perform. Same as for leaps::regsubsets. Default is forward selection.
#' @param nvmax Maximum number of features to consider. Default is all features in data. 
#' @param plot_mean_mse A logical value indicating whether to plot the mean mse for each size model. Default is TRUE. 
#' @return 
#' @export
cross_validated_subset_selection = function(data, response, K = 10, method = "forward", nvmax = NULL, plot_mean_mse = T){
  
  # Check that response is the name of a column in data
  if(!response %in% names(data)){stop(paste(response, "is not the name of a column in data"))}
  
  # Set nvmax equal to the number of features in data if it is not provided
  if(is.null(nvmax)){nvmax = ncol(data) - 1}
  
  # Split the data into K folds
  folds = suppressWarnings(sample_k_fold_indices(data, K))
  
  # Create a matrix to store the CV results for each fold for each model size
  cv_results = matrix(NA, nrow = nvmax + 1, ncol = K)
  
  # Perform subset selection on each fold
 `%do%` <- foreach::`%do%`
  mse_for_cv_models = foreach::foreach(fold = folds, i = seq_along(folds)) %do% {
    
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

### Add function to return an lm model of the best size
