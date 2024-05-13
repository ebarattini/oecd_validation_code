load_csv <- function(file_path) {
  df_raw <- tryCatch({
    fread(file_path, quote = "")
  }, error = function(e) {
    message(paste("Error loading file:", file_path, ":", e$message))
    return(NULL)  # Return NULL on error to handle later
  })
  
  if (!is.null(df_raw)) {
    # Clean column names to remove unexpected quotes
    names(df_raw) <- gsub('^"|"$', '', names(df_raw))
    
    # Check for required columns
    required_columns <- c("REF_AREA", "TIME_PERIOD", "MEASURE", "SEX", "AGE", "UNIT", "OBS_VALUE")
    if (!all(required_columns %in% names(df_raw))) {
      message(paste("Missing required columns in file:", file_path, 
                    paste(required_columns[!required_columns %in% names(df_raw)], collapse=", ")))
      return(NULL)
    }
    
    # Clean data within character columns
    df_raw[] <- lapply(df_raw, function(column) {
      if (is.character(column)) {
        column <- gsub('^"|"$', '', column)
      }
      return(column)
    })
    
    # Ensure OBS_VALUE column is treated as integer
    # This will return NA for non-integer values which can then be handled accordingly
    df_raw$OBS_VALUE <- as.integer(df_raw$OBS_VALUE)
    if (any(is.na(df_raw$OBS_VALUE))) {
      warning(paste("Some OBS_VALUE data in file", file_path, "could not be converted to integer."))
    }
  }
  
  return(df_raw)
}
