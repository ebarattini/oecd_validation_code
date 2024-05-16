####################################################################################################
## validation check 1 function
# this function checks the format of the data input. Specifically, it
# 1 - checks that mandatory column names are present and written as follows: "REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "UNIT", "OBS_VALUE"
# 2 - identifies if there are any additional columns in the data and outputs their name if any
# 3 - checks that ISO formats are respected
####################################################################################################

####################################################################################################
## validation check 1 function
# this function checks the format of the data input. Specifically, it
# 1 - checks that mandatory column names are present and written as follows: "REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "UNIT", "OBS_VALUE"
# 2 - identifies if there are any additional columns in the data and outputs their name if any
# 3 - checks that ISO formats are respected
####################################################################################################

validate_format <- function(df, file_name) {
  mandatory_column_names <- c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "UNIT", "OBS_VALUE")
  optional_column_names <- c("AGE", "INCOME")
  
  if (!all(mandatory_column_names %in% names(df))) {
    missing_columns <- setdiff(mandatory_column_names, names(df))
    message(paste("Validation Check 1 - Failed: Missing mandatory columns", paste(missing_columns, collapse=", "), "in file", file_name))
    result <- as_tibble(df)
    result$FAILED_CHECK <- "Validation Check 1"
    return(result)
  }
  
  additional_columns <- setdiff(names(df), c(mandatory_column_names, optional_column_names))
  if (length(additional_columns) > 0) {
    message(paste("Validation Check 1 - Notice: Additional column(s) found", paste(additional_columns, collapse=", "), "in file", file_name))
  }
  
  if (any(!grepl("^[A-Z]{3}$", df$REF_AREA))) {
    message(paste("Validation Check 1 - Failed: REF_AREA in file", file_name, "contains invalid entries."))
    result <- as_tibble(df[!grepl("^[A-Z]{3}$", df$REF_AREA)])
    result$FAILED_CHECK <- "Validation Check 1"
    return(result)
  }
  
  if (any(!grepl("^\\d{4}$", df$TIME_PERIOD))) {
    message(paste("Validation Check 1 - Failed: TIME_PERIOD in file", file_name, "contains invalid entries."))
    result <- as_tibble(df[!grepl("^\\d{4}$", df$TIME_PERIOD)])
    result$FAILED_CHECK <- "Validation Check 1"
    return(result)
  }
  
  if (any(!df$SEX %in% c("F", "M", "T"))) {
    message(paste("Validation Check 1 - Failed: SEX in file", file_name, "contains invalid entries."))
    result <- as_tibble(df[!df$SEX %in% c("F", "M", "T")])
    result$FAILED_CHECK <- "Validation Check 1"
    return(result)
  }
  
  if (any(grepl("-", df$AGE))) {
    message(paste("Validation Check 1 - Failed: AGE in file", file_name, "is old format, of type 15-20Y. New format is of type Y15T20."))
    result <- as_tibble(df[grepl("-", df$AGE)])
    result$FAILED_CHECK <- "Validation Check 1"
    return(result)
  }
  
  if (any(!is.numeric(df$OBS_VALUE))) {
    message(paste("Validation Check 1 - Failed: OBS_VALUE in file", file_name, "contains non-numeric entries."))
    result <- as_tibble(df[!is.numeric(df$OBS_VALUE)])
    result$FAILED_CHECK <- "Validation Check 1"
    return(result)
  }
  
  message(paste("Validation Check 1 - Passed: All column names and observations in", file_name, "have the expected format."))
  return(TRUE)
}