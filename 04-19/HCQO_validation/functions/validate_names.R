validate_names <- function(data, file_name) {
  
  # ++ add validation of observations too
  # Define the correct, expected column names
  correct_column_names <- c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE", "UNIT", "OBS_VALUE")
  
  # Check if the columns in the dataframe match the correct column names
  if (!identical(sort(names(data)), sort(correct_column_names))) {
    message(paste("Column names in the file", file_name, "do not match the expected names."))
    return(FALSE)  # Return FALSE indicating a failure in validation
  }
  
  message(paste("All column names in file", file_name, "are correct."))
  return(TRUE)  # Return TRUE indicating successful validation
}
