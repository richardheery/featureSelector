#' Calculate variable importance using a random forest. 
#'
#' @param data Matrix or data.frame containing the response and features as columns.
#' @param response Name of the column containing the response in data. 
#' @param ntree Number of trees. Default is 500. 
#' @param mtry Number of features used at each split. Uses same default as randomForest::randomForest(). 
#' @return A matrix with the feature importance measures.
#' @export
rf_var_importance = function(data, response, mtry = NULL, ntree = 500){
  
  # Convert data into a data.frame
  data = as.matrix(data)
  
  # Fit random forest to data
  if(is.null(mtry)){
    rf = randomForest::randomForest(y = data[, response], 
      x = data[, colnames(data) != response], ntree = ntree, importance = TRUE)
  } else {
    rf = randomForest::randomForest(y = data[, response], 
      x = data[, colnames(data) != response], mtry = mtry, ntree = ntree, importance = TRUE)
  }
  
  # Return feature importance from the model
  return(randomForest::importance(rf))

}
