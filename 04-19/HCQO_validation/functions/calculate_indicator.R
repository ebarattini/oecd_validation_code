CalculateIndicator <- function(raw_file) {

  # setwd(path)
  # Importing Indicators. The file contains the methods for standardizations and the standardization population and constants.
  source("functions/indicator_classes.R", local =  TRUE)

  df_raw <- load_csv(raw_file)
  
  # Check if the data frame is loaded correctly
  if (is.null(df_raw)) {
    stop(paste("Failed to load data from:", raw_file))
  }
  
  file_name <- basename(raw_file)
  print(paste("Processing file:", file_name))
  df_raw$Source_File <- file_name
  
  
  # Ensure that 'NUM' and 'DEN' are numeric
  if (!all(c('UNIT') %in% names(df_raw))) {
    stop('NUM or DEN columns are missing.')
  }
  
  
  df_raw$Source_File <- file_name
  
  alll <<- append(alll, list(df_raw))
  
  # Infer type of indicator based on column MEASURE
  indicators <- InferIndicatorType(df_raw) 
  
  # Preprocess the indicators
  indicators <- lapply(indicators, PreprocessData)
  
  
  # Calculate crude rates
  indicators <- lapply(indicators, CalculateCrdRate)
  #browser()
  
  # Calculate standardized rates
  indicators <- lapply(indicators, CalculateStdRate)
  #browser()
  
  # Initializing empty lists for separating indicators into two groups.
  calculations_required <- list()
  as_is <- list()
  
  
  # Seperate data into 'as-is' and 'calculations_required'
  temp <- lapply(indicators, FUN = function(indicator) {
    # Type of object
    indicator.type <- class(indicator)
    
    # assign the object to the appropriate list based on its type
    if (indicator.type %in% as.is.types) {
      as_is <<- append(as_is,indicator)
      return(1)
    } else {
      calculations_required <<- append(calculations_required,indicator)
      return(1)
    }
  })
  
  # freeing memory 
  indicators <- 1
  
  
  output_1 <- list()
  # if not empty, we process type 'calculations_required' 
  if(!is.null(calculations_required)) {
    
    std_rates <- dplyr::bind_rows(lapply(calculations_required, FUN = function(indicator) {
      indicator@standardized_rates
    }))
    
    # Get the final output in right format for 'Calculations_required'
    output_1 <- dplyr::bind_rows(GetOutputDataframe(std_rates))  
    
  }
  output_2 <- list()
  # if not empty, we process type 'as is' 
  if(!is.null(as_is)) {
    output_2 <- dplyr::bind_rows(lapply(as_is, FUN = function(indicator) {
      return(indicator@standardized_rates)
    }))
    
    
  }
  
  # Combining results into one
  output <- dplyr::bind_rows(output_1, output_2)
  
  return (output)
  return(alll)
  
}