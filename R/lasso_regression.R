#' Fit a lasso regression model using cross-validation to identify the best value for the tuning parameter.
#'
#' @param data Matrix or data.frame containing the response and features as columns.
#' @param response Name of the column containing the response in data. 
#' @param lambda A vector of values for the tuning parameter to be tested using cross-validation. 
#' Default is to let glmnet choose its own values.
#' @param K Number of folds. Default is 10. 
#' @param ncores Number of cores to use to process folds separately in parallel. Default is process each fold sequentially.
#' Cannot be greater than K. 
#' @return An object of class glmnet for lasso regression. 
#' @export
fit_best_lasso_with_cv = function(data, response, lambda = NULL, K = 10, ncores = 1){
  
  # Give a warning if ncores is greater than K and set ncores equal to K.
  if(ncores > K){
    warning(sprintf("Provided value for ncores (%s) is greater than K (%s). Will just use %s cores.", ncores, K, K))
    ncores = K
  }
  
  # Make a cluster with the specified number of cores and register it
  if(ncores > 1){
    cl = parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl, ncores)
    on.exit(parallel::stopCluster(cl))
  }
  
  # Create a model.matrix from the data
  formula = as.formula(paste(response, "~ ."))
  features = model.matrix(formula, data)[ , -1]
  
  # Perform CV in order to identify the best value for the tuning parameter for the lasso
  cv_glmnet = glmnet::cv.glmnet(x = features, y = data[, response], alpha = 1, lambda = lambda, nfolds = K)
  
  # Get the lambda which gives the lowest error
  best_lambda = cv_glmnet$lambda.min
  
  # Fit a lasso model using the identified value of lambda
  best_lasso_model = glmnet::glmnet(x = features, y = data[, response], alpha = 1, lambda = best_lambda)
  return(best_lasso_model)
  
}

#' Extract the names of the features selected by lasso regression
#'
#' @param lasso_model An object of class glmnet produced by fit_best_lasso_with_cv()
#' @return A character vector with the names of the selected features.
#' @export
extract_best_features_from_lasso = function(lasso_model){
  
  # Check that only one lambda value was used when fitting lasso_model
  if(length(lasso_model$lambda) > 1){
    stop(paste0("lasso_model contains coefficient estimates for multiple values of lambda.\n  ", 
      "lasso_model should be the output of fit_best_lasso_with_cv() containing coefficient estimates\n  ", 
      "only for the best value of lambda identified by CV."))
  }
  
  # Get the coefficients from the model 
  coefficients = as.matrix(predict(lasso_model, type = "coefficients"))[, 1]
  
  # Filter for features with absolute coefficient values greater than 0 exclduing the intercept
  selected_features = names(coefficients[abs(coefficients) > 0])
  selected_features = setdiff(selected_features, "(Intercept)")
  return(selected_features)
  
}
