#################################################################################################################################################
## validation check 1 function

# this function checks the format of the data input. Specifically, it
# 1 - checks that mandatory column names are present and written as follows: "REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "UNIT", "OBS_VALUE"
# 2 - identifies if there are any additional columns in the data and outputs their name if any
# 3 - checks that ISO formats are respected

#################################################################################################################################################


validate_format <- function(data, file_name) {
  # 1 - checks that mandatory column names are present and written as follows: "REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "UNIT", "OBS_VALUE"
  mandatory_column_names <- c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "UNIT", "OBS_VALUE")
  optional_column_names <- c("AGE", "INCOME")  # define optional column names
  
  # 2 - identifies if there are any additional columns in the data and outputs their name if any
  if (!all(mandatory_column_names %in% names(data))) {
    missing_columns <- mandatory_column_names[!mandatory_column_names %in% names(data)]
    message(paste("Validation Check 1 - Failed: Missing mandatory columns", paste(missing_columns, collapse=", "), "in file", file_name))
    return(FALSE)
  }
  
  # identify any additional columns not in the defined list
  additional_columns <- setdiff(names(data), c(mandatory_column_names, optional_column_names))
  if (length(additional_columns) > 0) {
    message(paste("Validation Check 1 - Notice: Additional column(s) found", paste(additional_columns, collapse=", "), "in file", file_name))
  }
  
  # 3 - checks that ISO formats are respected
  # check ref_area for 3 capitalized letters
  if (any(!grepl("^[A-Z]{3}$", data$REF_AREA))) {
    message(paste("Validation Check 1 - Failed: REF_AREA in file", file_name, "contains invalid entries."))
    return(FALSE)
  }
  
  # check time_period for format YYYY
  if (any(!grepl("^\\d{4}$", data$TIME_PERIOD))) {
    message(paste("Validation Check 1 - Failed: TIME_PERIOD in file", file_name, "contains invalid entries."))
    return(FALSE)
  }
  
  # check sex for either 'f', 'm', or 't'
  if (any(!data$SEX %in% c("F", "M", "T"))) {
    message(paste("Validation Check 1 - Failed: SEX in file", file_name, "contains invalid entries."))
    return(FALSE)
  }
  
  # check that AGE is not in the old format (e.g. 15-20). Correct format is Y15T20
  if (any(grepl("-", data$AGE))) {
    message(paste("Validation Check 1 - Failed: AGE in file", file_name, "is old format, of type 15-20Y. New format is of type Y15T20."))
    return(FALSE)
  }
  
  # check obs_value for being a number
  if (any(!is.numeric(data$OBS_VALUE))) {
    message(paste("Validation Check 1 - Failed: OBS_VALUE in file", file_name, "contains non-numeric entries."))
    return(FALSE)
  }
  
  # if all checks pass
  message(paste("Validation Check 1 - Passed: All column names and observations ",file_name, "have the expected format."))
  return(TRUE)
}
