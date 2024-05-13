# Validation check 1:
# Checks for any numerators that have no matching denominators

ValidateMissingDenominators <- function(df_raw, source_file_name) {
  
  numerators <- df_raw %>% filter(UNIT == "NUM")
  #numerators <- df_raw[UNIT %in% "NUM"]
  
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
    filter(check_denom_existence(cur_data()))
  
  # Add the source file name to the missing_denom dataframe
  if(nrow(missing_denom) > 0) {
    missing_denom$SOURCE_FILE <- source_file_name
    warning(paste("Missing denominator(s) found in file", source_file_name, ":\n"))
    print(missing_denom)
  } else {
    print("Validation Check 1 - Passed")
  }
  
  return(df_raw)
}
