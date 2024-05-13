validate_format <- function(data, file_name) {
  # Define the correct column names
  correct_column_names <- c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE", "UNIT", "OBS_VALUE")
  
  # Check if the columns in the dataframe match the correct column names
  if (!identical(sort(names(data)), sort(correct_column_names))) {
    message(paste("Column names in the file", file_name, "do not match the expected names."))
    return(FALSE)  # Return FALSE indicating a failure in validation
  }
  
  # Check REF_AREA for 3 capitalised letters
  if (any(!grepl("^[A-Z]{3}$", data$REF_AREA))) {
    message(paste("REF_AREA in file", file_name, "contains invalid entries."))
    return(FALSE)
  }
  
  # Check TIME_PERIOD for format YYYY
  if (any(!grepl("^\\d{4}$", data$TIME_PERIOD))) {
    message(paste("TIME_PERIOD in file", file_name, "contains invalid entries."))
    return(FALSE)
  }
  
  # Check SEX for either F or M or T
  if (any(!data$SEX %in% c("F", "M", "T"))) {
    message(paste("SEX in file", file_name, "contains invalid entries."))
    return(FALSE)
  }
  
  # ++ AGE
  
  # Check OBS_VALUE for being a number
  if (any(!is.numeric(data$OBS_VALUE))) {
    message(paste("OBS_VALUE in file", file_name, "contains non-numeric entries."))
    return(FALSE)
  }
  
  # If all checks pass
  message(paste("All values in file", file_name, "have the correct format."))
  return(TRUE)
}
