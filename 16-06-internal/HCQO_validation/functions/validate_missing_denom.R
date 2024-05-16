####################################################################################################
## validation check 2 function
# this function checks for any numerators that have no matching denominators
####################################################################################################

validate_missing_denom <- function(df, file_name) {
  setDT(df)  # Convert to data.table for faster operations
  
  numerators <- df[UNIT == "NUM"]
  unique_combinations <- unique(numerators[, .(REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE)])
  denominators <- df[UNIT == "DEN"]
  missing_denom <- unique_combinations[!denominators, on = .(TIME_PERIOD, MEASURE, SEX, AGE)]
  
  if (nrow(missing_denom) > 0) {
    missing_denom[, SOURCE_FILE := file_name]
    result <- as_tibble(missing_denom)
    result$FAILED_CHECK <- "Validation Check 2"
    message(paste("Validation Check 2 - Failed: Missing denominator(s) found in file", file_name))
    return(result)
  }
  
  message(paste("Validation Check 2 - Passed:", file_name, "contains a matching denominator for every numerator."))
  return(TRUE)
}