#' Calculate variable importance using a random forest. 
#'
#' @param response A vector with the response variable to be predicted.
#' @param features A matrix or data.frame of features where each column is a feature. 
#' @param ntree Number of trees. Default is 500. 
#' @param mtry Number of features used at each split. Uses same default as randomForest::randomForest(). 
#' @return A matrix with the feature importance measures.
#' @export
rf_var_importance = function(data, response, mtry = NULL, ntree = 500){
  
  # Fit random forest to data
  if(is.null(mtry)){
    rf = randomForest::randomForest(y = response, x = as.matrix(features), ntree = ntree, importance = TRUE)
  } else {
    rf = randomForest::randomForest(y = response, x = as.matrix(features), mtry = mtry, ntree = ntree, importance = TRUE)
  }
  
  # Return feature importance from the model
  return(randomForest::importance(rf))

}