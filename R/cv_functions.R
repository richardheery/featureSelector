#' Create row indices for K folds of a dataset
#'
#' @param data Dataset to split into folds. Should contain the response variable as well as the associated features. 
#' A warning will be given if there is not at least a ratio of 10:1 of observations to features in each training fold.
#' @param K Number of folds. Default is 10. 
#' @return A list with the row indices for K folds of data
#' @export
sample_k_fold_indices = function(data, K = 10){
  
  # Throw an error if K is 1
  if(K == 1){
    stop("K must be greater than 1")
  }
  
  # Give a warning if there is not at least 10 times the number of 
  if(nrow(data)*(K-1)/K < 10*(ncol(data)-1)){
    warning("There are not enough observations to have a 10:1 ratio of observations to features in each training fold")
  }
  
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