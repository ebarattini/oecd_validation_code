


# Initializes a PS type 1 indicator. 
# Corresponds to these indicators "POSTSESP", "POSTDVSP", "POSTPESP", "FORBPROC"
InitializePS1 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('15-19Y', '20-24Y', '25-29Y', '30-34Y', '35-39Y', '40-44Y','45-49Y', '50-54Y', '55-59Y', '60-64Y', '65-69Y', '70-74Y', '75-79Y','80-84Y', '85Y_OVER')
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
  
  indicator <- new("PS_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
}
  
  


# Initializes a PS type 2 indicator. 
# Corresponds to these indicators  "OBSTVDWI", "OBSTVDWO"
InitializePS2 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('15Y_UNDER','15-19Y', '20-24Y', '25-29Y', '30-34Y', '35-39Y', '40-44Y','45-49Y', '50-54Y', '55Y_OVER')
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
  
  indicator <- new("PS_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
}


# For PS, we do not standardize. We just sum the numerators and denominators.
setMethod("CalculateStdRate", "PS", function(object) {
  
  CalculateHelper <- function(row) {
    
    results_df <- data.frame(NUM = numeric(1),
                             DEN = numeric(1),
                             MEASURE = character(1),
                             SEX = character(1),
                             COUNTRY = character(1),
                             TIME_PERIOD = character(1))
    
    results_df$NUM <- sum(row$data$NUM) * object@kScalar
    results_df$DEN <- sum(row$data$DEN)
    results_df$MEASURE <- row$MEASURE
    results_df$SEX <- row$SEX
    results_df$COUNTRY <- row$COUNTRY
    results_df$TIME_PERIOD <- row$TIME_PERIOD
    return(results_df)
  } 
  
  std_rates <- object@crude_rates %>% dplyr::rowwise() %>% 
    do(result = CalculateHelper(.)) %>% unnest(result)
  
  
  object@standardized_rates <- as.data.frame(std_rates)

  
  return(object)
})