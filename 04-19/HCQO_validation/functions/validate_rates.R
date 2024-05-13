# Validation check 2:
# Checks for rates where NUM > DEN
# ++ not applicable for PR

validate_rates <- function(df, source_file_name) {
  # Check if NUM or DEN columns are present
  if (!("NUM" %in% unique(df$UNIT)) || !("DEN" %in% unique(df$UNIT))) {
    print("Validation Check 2 - Skipped: No numerators or denominators found.")
    return(df)
  }
  
  # Extract the distinct identifiers for rows that have a RATE value
  rate_identifiers <- df %>%
    filter(grepl("RATE", UNIT)) %>%
    select(REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE) %>%
    distinct()
  
  # If no rates are found, print a message and return
  if (nrow(rate_identifiers) == 0) {
    print("Validation Check 2 - Skipped: No RATE values found.")
    return(df)
  }
  
  # Join to get the matching NUM and DEN rows for RATE identifiers
  df_rates <- rate_identifiers %>%
    left_join(df %>% filter(UNIT %in% c("NUM", "DEN")), by = c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE"))
  
  # Pivot wider to create NUM and DEN columns
  df_wide <- df_rates %>%
    pivot_wider(names_from = UNIT, values_from = OBS_VALUE)
  
  # Add the source file name to the df_wide dataframe
  df_wide$SOURCE_FILE <- source_file_name
  
  # Check if NUM > DEN
  invalid_rates <- df_wide %>%
    filter(NUM > DEN) %>%
    select(SOURCE_FILE, REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE, NUM, DEN) %>%
    as_tibble()
  
  # If there are any invalid rates, display them as a tibble
  if (nrow(invalid_rates) > 0) {
    warning("Invalid rate(s) found where NUM > DEN:", nrow(invalid_rates), "cases.\n")
    cat("Invalid Rates:\n")
    print(invalid_rates)
  } else {
    print("Validation Check 2 - Passed")
  }
  
  return(df)
}
