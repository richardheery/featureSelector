#' Calculate test error after permuting a feature. 
#'
#' @param model A model.
#' @param response_values A numeric vector with values for the response
#' @param newdata A data.frame with new data to make predictions using model.
#' @param feature The name of a feature in newdata to permute.
#' @param error_function A function to calculate prediction error.
.test_error_after_feature_permutation = function(model, newdata, response_values, feature, error_function){
  
  # Permute the indicated feature in data
  newdata[[feature]] = sample(newdata[[feature]])
  
  # Get predictions with the permuted feature
  predictions = predict(model, newdata = newdata)

  # Calculate the test error for the model
  test_error = error_function(response_values, predictions)
  return(test_error)
  
}

#' Calculate variable importance using feature shuffling (aka permutation importance)
#'
#' @param data Matrix or data.frame containing the response and features as columns.
#' @param formula An object of class formula indicating which features to use in data and which column is the response.
#' @param model_function A function (or a string with its name) to fit the model e.g. lm or randomForest::randomForest.
#' @param error_function A function (or a string with its name) to calculate the error given the predictions of a function.
#' Should take two arguments, the first a vector with the actual values for the response and the second a vector of predicted values
#' and return a single numeric value.
#' mse() from the Metrics package is an example of such a function.
#' @param K Number of folds for cross-validation. Default is 10.
#' @param ncores Number of cores to use to process folds separately in parallel. Default is process each fold sequentially.
#' Cannot be greater than K. 
#' @param ... Additional arguments to be passed to model_function. 
#' @return A matrix with the feature importance measures.
#' @export
permutation_importance = function(data, formula, model_function, error_function, K = 10, ncores = 1, ...){
  
  # Collect additional arguments in a list
  additional_args = list(...)
  
  # Check that formula is a formula object 
  if(!is(formula, "formula")){stop("Supplied formula is not an object of class formula")}
  
  # Get the name of the response and the features
  response_name = all.vars(formula)[1]
  feature_names = setdiff(colnames(data), response_name)
  
  # Check that formula works with the supplied data
  tryCatch(model.matrix(formula, data), error = function(e) 
    stop("Columns indicated by the supplied formula do not seem to be present in data"))
  
  # If model function is a character string, try to get the function with that name and check that it is a function
  if(is.character(model_function)){model_function = get(model_function)}
  if(!is(model_function, "function")){
    stop("model_function does not seem to be either a function or the name of a function")
  }
  
  # Split data into K folds for cross-validation
  folds = suppressWarnings(sample_k_fold_indices(data, K))
  
  # Create matrices to store the CV results for each fold
  cv_error_diff = matrix(NA, nrow = length(feature_names), ncol = K, 
    dimnames = list(feature_names, paste0("fold_", 1:K)))
  cv_error_ratio = cv_error_diff
  
  # Make a cluster with the specified number of cores and register it
  if(ncores > 1){
    cl = parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl, ncores)
    on.exit(parallel::stopCluster(cl))
    `%dopar%` = foreach::`%dopar%`
  } else {
    `%dopar%` <- foreach::`%do%`
  }
  
  # Permute features across folds
  test_errors_for_folds = foreach::foreach(fold = folds, i = seq_along(folds), .packages = "featureSelector") %dopar% {
    
    # Separate data into train and test sets
    training = separate_training_and_test_data(data, k_folds_indices = folds, test_fold = i)$training
    test = separate_training_and_test_data(data, k_folds_indices = folds, test_fold = i)$test
    test_response = test[[response_name]]
    
    # Fit the model to the training data
    model_arguments = c(additional_args, list(formula = formula, data = test))
    model = do.call(model_function, model_arguments)
    
    # Get predictions with the test data
    predictions = predict(model, newdata = test)
  
    # Calculate the test error for the model
    test_error = error_function(test_response, predictions)
    
    # Permute each feature and recalculate the test error
    permuted_feature_test_error = sapply(feature_names, function(x) 
      .test_error_after_feature_permutation(model, newdata = test, response_values = test_response, 
        feature = x, error_function = error_function))
    
    # Update cv_error_diff and cv_error_ratio with the difference and 
    # ratio of permuted_feature_test_error and cv_error_diff respectively
    cv_error_diff[, i] = permuted_feature_test_error - test_error
    cv_error_ratio[, i] = permuted_feature_test_error / test_error
    
  }
  
  # Return v_error_diff and cv_error_ratio in a list
  return(list(cv_error_diff = cv_error_diff, cv_error_ratio = cv_error_ratio))
  
}

# Make sure K is appropriate for the dimensions of the data so that there are enough samples relative to features
# Handle case when K = 1

x = permutation_importance(data = mtcars, formula = formula("mpg ~ ."), model_function = lm, 
  error_function = Metrics::mse, K = 2, ncores = 1)
