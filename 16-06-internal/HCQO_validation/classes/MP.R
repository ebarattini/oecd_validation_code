


# Initializes a MP Indicator.
# corresponds to all indicators
InitializeMP <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y16T24', 'Y25T44', 'Y45T64', 'Y_GE65')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 1
  
  indicator <- new("MP_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  
  
}

# For MP, indicators there is no need to preprocess data. The data is ready 'as is'
setMethod("PreprocessData", "MP", function(object) {
  # We are only interested in the total
  df_raw_sub <- subset(object@data_raw, AGE == "Total_16")
  # converting to wide
  df_sub <- df_raw_sub %>% pivot_wider(id_cols = c(SEX, MEASURE, TIME_PERIOD, REF_AREA), names_from = UNIT, values_from = OBS_VALUE )
  
  # Pivoting to result format
  output <- pivot_longer(data = df_sub,
                         c(CRUDE_RATE_CPAT),
                         names_to =  "value",
                         values_to = "number",
                         values_drop_na = FALSE) 
  object@standardized_rates <- output
  
  # Assignment only for next Filter step in calculate_indicator
  object@preprocessed <- output  
  return(object)
})

# For MP, the data is 'as is', hence we return the object without any modifications
setMethod("CalculateCrdRate", "MP", function(object) {
  return(object)
})
# For MP, the data is 'as is', hence we return the object without any modifications
setMethod("CalculateStdRate", "MP", function(object) {
  return(object)
})
