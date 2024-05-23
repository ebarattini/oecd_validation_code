####################################################################################################
## validation check 3 function
# this function checks for rates where NUM > DEN

####################################################################################################
library(dplyr)
library(tidyr)

validate_rates <- function(df, source_file_name) {
  # List of PR indicators for which the check is not applicable
  pr_indicators <- c(
    "PRDMPCDD", "PRDMPADD", "PRBZOZDD", "PRBZLAOP", 
    "PRABCQDD", "PRABOUDD", "PRACCUNS", "PRPPOPFM", 
    "PROPOUDD", "PROPPCOU", "PRPPANTI"
  )
  
  # Check if MEASURE contains any of the PR indicators
  if (any(df$MEASURE %in% pr_indicators)) {
    message("Validation Check 3 - Skipped: PR indicators are not applicable for this check.")
    return(df)
  }
  
  # Check if NUM or DEN columns are present
  if (!("NUM" %in% unique(df$UNIT)) || !("DEN" %in% unique(df$UNIT))) {
    message("Validation Check 3 - Skipped: No numerators or denominators found.")
    return(df)
  }
  
  # Extract the distinct identifiers for rows that have a RATE value
  rate_identifiers <- df %>%
    filter(grepl("RATE", UNIT)) %>%
    select(REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE) %>%
    distinct()
  
  # If no rates are found, print a message and return
  if (nrow(rate_identifiers) == 0) {
    message(paste("Validation Check 3 - Skipped: No RATE values found."))
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
  
  # Check if NUM > DEN and create a column 'FAILED_CHECK' indicating the check failed
  df_wide <- df_wide %>%
    mutate(FAILED_CHECK = ifelse(NUM > DEN, "Check 3: NUM > DEN", NA))
  
  # Print a warning and the invalid rates if any exist
  if (any(!is.na(df_wide$FAILED_CHECK))) {
    warning(paste("Validation Check 3 - Failed: NUM > DEN found in file", source_file_name))
    print(df_wide %>% filter(!is.na(FAILED_CHECK)) %>% select(SOURCE_FILE, REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE, NUM, DEN, FAILED_CHECK))
  } else {
    message(paste("Validation Check 3 - Passed:", source_file_name, "contains no rates where NUM > DEN"))
  }
  
  # Merge annotations back to the original dataframe
  df <- df %>%
    left_join(df_wide %>% select(REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE, FAILED_CHECK, SOURCE_FILE, NUM, DEN),
              by = c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE"))
  
  return(df)
}



