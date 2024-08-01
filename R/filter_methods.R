# Removing constant, quasi-constant and duplicated features

#' Remove constant features from a dataset
#' 
#' @param data Any tabular data structure. 
#' @return The dataset with any constant features removed.
#' @export
remove_constant_features = function(data){
  
  # Find data type of data and convert it to a data.frame
  data_type = class(data)
  data = as.data.frame(data)
  
  # Find constant features
  constant_features = names(which(sapply(data, function(x) length(unique(x))) == 1))
  
  # Return original data if no constant features found
  if(length(constant_features) == 0){
    return(as(data, data_type))
  }
  
  # Remove constant features
  data = dplyr::select(data, !dplyr::all_of(constant_features))
  message(paste("Removed", length(constant_features), "constant features."))
  
  # Return data to original data type and return it
  return(as(data, data_type))
  
}

#' Remove duplicated features from a dataset
#' 
#' @param data Any tabular data structure. 
#' @return The dataset with any constant features removed.
#' @export
remove_duplicated_features = function(data){
  
  # Find data type of data and convert it to a data.frame
  data_type = class(data)
  data = as.data.frame(data)
  
  # Find duplicated features
  duplicated_features = names(data)[duplicated(as.list(data))]
  
  # Return original data if no duplicated features found
  if(length(duplicated_features) == 0){
    return(as(data, data_type))
  }
  
  # Remove duplicated features
  data = dplyr::select(data, !dplyr::all_of(duplicated_features))
  message(paste("Removed", length(duplicated_features), "duplicated features."))
  
  # Return data to original data type and return it
  return(as(data, data_type))
  
}



find_low_sd_features = function(df, threshold){
  names(which(sapply(df, function(x) sd(x)) < threshold))
}

find_narrow_range_features = function(df, threshold){
  names(which(sapply(df, function(x) diff(range(x))) < threshold))
}

# Can be more aggressive than using sd as some quasi constant features may have relatively high sd 
# if a small proprotion of observations have substantially different values to the majority value.
find_quasi_constant_features = function(df, threshold){
  names(which(sapply(df, function(x) max(table(x)/length(x))) >= threshold))
}

find_duplicated_features = function(df){
  names(df)[duplicated(as.list(df))]
}
