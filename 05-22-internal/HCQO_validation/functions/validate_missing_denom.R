####################################################################################################
## validation check 2 function
# this function checks for any numerators that have no matching denominators
####################################################################################################

library(dplyr)

validate_missing_denom <- function(df_raw, source_file_name) {
  numerators <- df_raw %>% filter(UNIT == "NUM")
  
  unique_combinations <- numerators %>%
    select(REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE) %>%
    distinct()
  
  check_denom_existence <- function(row) {
    exists <- any(df_raw$TIME_PERIOD == row$TIME_PERIOD &
                    df_raw$MEASURE == row$MEASURE &
                    df_raw$SEX == row$SEX &
                    df_raw$AGE == row$AGE &
                    df_raw$UNIT == "DEN")
    return(!exists)
  }
  
  missing_denom <- unique_combinations %>%
    rowwise() %>%
    filter(check_denom_existence(cur_data())) %>%
    ungroup()
  
  # Add the source file name to the missing_denom dataframe
  if (nrow(missing_denom) > 0) {
    missing_denom <- missing_denom %>%
      mutate(FAILED_CHECK = "Check 2: Missing denominator", SOURCE_FILE = source_file_name)
    warning(paste("Validation Check 2 - Failed: Missing denominator(s) found in file", source_file_name))
    print(missing_denom %>% select(SOURCE_FILE, REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE, FAILED_CHECK))
    
    # Merge annotations back to the original dataframe
    df_raw <- df_raw %>%
      left_join(missing_denom %>% select(REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE, FAILED_CHECK, SOURCE_FILE),
                by = c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE"))
  } else {
    message(paste("Validation Check 2 - Passed:", source_file_name, "contains a matching denominator for every numerator."))
    df_raw <- df_raw %>%
      mutate(SOURCE_FILE = source_file_name, FAILED_CHECK = NA)
  }
  
  return(df_raw)
}
