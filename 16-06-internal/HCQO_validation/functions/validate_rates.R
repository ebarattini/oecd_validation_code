####################################################################################################
## validation check 3 function
# this function checks for rates where NUM > DEN
# TODO: not applicable for PR
####################################################################################################
validate_rates <- function(df, file_name) {
  setDT(df)  # Convert to data.table for faster operations
  
  if (!("NUM" %in% df$UNIT) || !("DEN" %in% df$UNIT)) {
    return(TRUE)
  }
  
  # Extract the rows with NUM and DEN values separately
  numerators <- df[UNIT == "NUM"]
  denominators <- df[UNIT == "DEN"]
  
  # Merge NUM and DEN rows by the key columns
  invalid_rates <- merge(numerators, denominators, by = c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE"), suffixes = c("_NUM", "_DEN"))
  
  # Check where NUM_VAL > DEN_VAL
  invalid_rates <- invalid_rates[OBS_VALUE_NUM > OBS_VALUE_DEN]
  
  if (nrow(invalid_rates) > 0) {
    invalid_rates[, SOURCE_FILE := file_name]
    result <- as_tibble(invalid_rates)
    result$FAILED_CHECK <- "Validation Check 3"
    message(paste("Validation Check 3 - Failed: Invalid rate(s) found where NUM > DEN in file", file_name))
    return(result)
  }
  
  message(paste("Validation Check 3 - Passed:", file_name, "contains no rates where NUM > DEN"))
  return(TRUE)
}
