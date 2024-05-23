####################################################################################################
## validation check 4 function
# this function checks for missing data. Specifically, it checks whether there are empty cells
# in populated rows/columns.
####################################################################################################

library(dplyr)

validate_missing_data <- function(df, file_name) {
  # Ensure TIME_PERIOD is of the same type in both dataframes
  df <- df %>% mutate(TIME_PERIOD = as.character(TIME_PERIOD))
  
  # Check for missing data in the dataframe
  missing_data <- df %>%
    filter_all(any_vars(is.na(.)))
  
  # If there are any rows with missing data, add annotations and merge back to original df
  if (nrow(missing_data) > 0) {
    message(paste("Validation Check 4 - Failed: Missing data found in file", file_name))
    
    # Annotate the missing data rows with check details
    missing_annotations <- missing_data %>%
      mutate(FAILED_CHECK = "Check 4: Missing data",
             SOURCE_FILE = file_name)
    
    # Print the problematic rows as a tibble for immediate inspection
    print(missing_annotations %>% select(SOURCE_FILE, REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE, FAILED_CHECK))
    
    # Merge annotations back to the original dataframe
    df <- df %>%
      left_join(missing_annotations %>% select(REF_AREA, TIME_PERIOD, MEASURE, SEX, AGE, FAILED_CHECK, SOURCE_FILE),
                by = c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE"))
    
    return(df)
  } else {
    message(paste("Validation Check 4 - Passed:", file_name, "does not contain missing cells."))
    df <- df %>%
      mutate(FAILED_CHECK = NA, SOURCE_FILE = file_name)
    return(df)
  }
}
