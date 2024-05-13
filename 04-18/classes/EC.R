
# Initializes a EC type 1 indicator. 
# Corresponds to all EC indicators, i.e., 'MORTACUT','MORTHOSP', "ADMMOCAN", 'ADMMOCAR', 'ADMMOCHR', 'ADMMOALZ', 'ADMMOALL',
# 'ADMDECAN', 'ADMDECAR', 'ADMDECHR', 'ADMDEALZ', 'ADMDEALL'
InitializeEC1 <- function(df_raw) {
  c # Defining column and row names
  col.names <- c('0-9Y', '10-19Y', '20-29Y', '30-39Y', '40-49Y', '50-59Y','60-69Y', '70-79Y', '80Y_OVER')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("EC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar) 
  
  
}


# For EC Indicators, we do not standardize. We calculate the crude rates.
setMethod("CalculateStdRate", "EC", function(object) {
  CalculateHelper <- function(row) {
    results_df <- data.frame(NUM = numeric(1),
                             DEN = numeric(1),
                             MEASURE = character(1),
                             SEX = character(1),
                             REF_AREA = character(1),
                             TIME_PERIOD = character(1),
                             std_rate = numeric(1))
    results_df$MEASURE <- row$MEASURE
    results_df$SEX <- row$SEX
    results_df$REF_AREA <- row$REF_AREA
    results_df$TIME_PERIOD <- row$TIME_PERIOD
    results_df$NUM <- sum(row$data$NUM) * object@kScalar
    results_df$DEN <- sum(row$data$DEN)
    results_df$std_rate <- results_df$NUM/results_df$DEN
    return(results_df)
  } 
  
  
  std_rates <- object@crude_rates %>% dplyr::rowwise() %>% 
    do(result = CalculateHelper(.)) %>% unnest(result)
  
  object@standardized_rates <- as.data.frame(std_rates)
  
  
  return(object)
})
