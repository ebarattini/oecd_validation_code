


# Initializes a PE Indicator.
# corresponds to all indicators
InitializePE <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y16T24', 'Y25T44', 'Y45T65', 'Y_GE65', "Total_16")
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100000
  
  indicator <- new("PE_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  
  
}

# For PE, indicators there is no need to preprocess data. The data is ready 'as is'
setMethod("PreprocessData", "PE", function(object) {
  # We are only interested in the total
  df_raw_sub <- subset(object@data_raw, AGE == "Total_16")
  # converting to wide
  df_sub <- df_raw_sub %>% pivot_wider(id_cols = c(SEX, MEASURE, TIME_PERIOD, REF_AREA), names_from = UNIT, values_from = OBS_VALUE )
  
  # Calculating confidence intervals
  df_sub <- df_sub %>% mutate(up_ci = CRUDE_RATE_CPAT + 1.96 * CRUDE_SE_PE, lo_ci = CRUDE_RATE_CPAT - 1.96 * CRUDE_SE_PE) %>% select(-CRUDE_SE_PE)
  
  # Pivoting to result format
  output <- pivot_longer(data = df_sub,
                         c(CRUDE_RATE_CPAT, lo_ci, up_ci),
                         names_to =  "value",
                         values_to = "number",
                         values_drop_na = FALSE) 
  object@standardized_rates <- output
  # Assignment just for the next Filter step in calculate_indicator
  object@preprocessed <- output
  
  return(object)
})

# For PE, the data is 'as is', hence we return the object without any modifications
setMethod("CalculateCrdRate", "PE", function(object) {
  return(object)
})
# For PE, the data is 'as is', hence we return the object without any modifications
setMethod("CalculateStdRate", "PE", function(object) {
  return(object)
})
