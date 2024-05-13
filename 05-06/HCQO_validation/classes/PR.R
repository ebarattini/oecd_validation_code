


# Initializes a PR type 1 indicator. 
# Corresponds to all 'PRDMPCDD', 'PRDMPADD', 'PRABCQDD', 'PRACCUNS'
InitializePR1 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y0T9', 'Y10T19', 'Y20T29', 'Y30T39', 'Y40T49', 'Y50T59','Y60T69', 'Y70T79', 'Y80T_O')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population_m <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population_m) <- col.names 
  base::rownames(population_m) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("PR_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population_m,
                   kScalar = kScalar) 
  
  
}

# Initializes a PR type 2 indicator. 
# Corresponds to all 'PRBZOZDD', 'PRBZLAOP'
InitializePR2 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y65T74', 'Y75T84Y', 'Y85T_O')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population_m <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population_m) <- col.names 
  base::rownames(population_m) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 1000
  indicator <- new("PR_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population_m,
                   kScalar = kScalar) 
  
  
  
}


# Initializes a PR type 3 indicator. 
# Corresponds to all 'PRPPOPFM'
InitializePR3 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y65T74', 'Y75T84', 'Y85T_O')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population_m <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population_m) <- col.names 
  base::rownames(population_m) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("PR_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population_m,
                   kScalar = kScalar) 
  
  
  
}



# Initializes a PR type 4 indicator. 
# Corresponds to all 'PROPOUDD'
InitializePR4 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y18T49', 'Y50T69', 'Y70T_O')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population_m <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population_m) <- col.names 
  base::rownames(population_m) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 1000
  indicator <- new("PR_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population_m,
                   kScalar = kScalar) 
  
  
  
}

# Initializes a PR type 5 indicator. 
# Corresponds to all 'PROPPCOU'
InitializePR5 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y18T49', 'Y50T69', 'Y70T_O')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population_m <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population_m) <- col.names 
  base::rownames(population_m) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("PR_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population_m,
                   kScalar = kScalar) 
  
  
  
}



# Initializes a PR type 6 indicator. 
# Corresponds to all 'PRPPANTI'
InitializePR6 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y65T69', 'Y70T74', 'Y75T79', 'Y80T84', 'Y85T_O')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(23725699, 17515803, 13214632, 8764367, 6354136)
  data.females <- c(26657966, 21242908, 18041732, 13862761, 13905892)
  
  data.total <- data.males + data.females
  # Creating standardization matrix
  population_m <- rbind(data.males,data.females,data.total)
  print(rowSums(population_m))
  # Naming the rows and columns of the matrix.
  base::colnames(population_m) <- col.names 
  base::rownames(population_m) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 1000
  indicator <- new("PR_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population_m,
                   kScalar = kScalar) 
  
  
  
}
# For PRABOUDD
InitializePR7 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y0T9', 'Y10T19', 'Y20T29', 'Y30T39', 'Y40T49', 'Y50T59','Y60T69', 'Y70T79', 'Y80T_O')
  row.names <- c('M', 'F','T')
  # Creating matrix
  number.of.cells <- length(col.names) * length(row.names)
  
  # We define a matrix of ones as for this indicator, we apply equal weighting.
  population_m <- matrix(rep(1,number.of.cells), nrow = length(row.names), ncol = length(col.names))
  
  # Naming the rows and columns of the matrix.
  base::colnames(population_m) <- col.names 
  base::rownames(population_m) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 1000
  indicator <- new("PR_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population_m,
                   kScalar = kScalar) 
  
  
}

# The standardization method for PR indicators.
# For groups 1-4, we calculate the crude rates only. For group 5,
# we calculate the default (std rate, CI's etc.)
setMethod("CalculateStdRate", "PR", function(object) {
  # Merges standardization population to crude rate data
  MergePopulationAndRates <- function(row) {
    # Accessing the population matrix and transposing it
    pop_t <- t(object@population)
    # We select the population for the SEX and the total
    pop_sub <- as.data.frame(pop_t[,c(row$SEX,'T')])
    # We have only one row remaining. We rename it as weight. This adds more functionality (less need for hardcoding)
    colnames(pop_sub) <- c("pop", 'pop_total')
    
    # Merging populations to crude rate data
    crude_rates_w_pop <- base::merge(row$data,
                                     pop_sub,
                                     by.x = "AGE", 
                                     by.y = "row.names",
                                     all.x = FALSE,
                                     all.y = TRUE)
    # should we add zeros to possible NaN values here?
    return(crude_rates_w_pop)
  }
  # Calculates the default standardized rates along with standard errors and CIs
  CalculateHelper <- function(row) {
    crude_rates_w_pop <- MergePopulationAndRates(row)
    population.total <- sum(object@population['T',])
    
    # Standard errors of crude rates
    std.error.num <- crude_rates_w_pop$crude_rate * object@kScalar**2  * (1 - crude_rates_w_pop$crude_rate  )
    # Calculation of standard errors for crude rates
    crude_rates_w_pop$std_error <- sqrt( std.error.num/ crude_rates_w_pop$DEN )
    # replace any NaNs with 0
    crude_rates_w_pop$std_error <- crude_rates_w_pop$std_error %>% replace(is.na(.), 0)
    
    # We calculate the standardized rate by the sex and also standard errors. Moreover, we also calculate the 'first' part of the over sex rate and standard error
    # In function(), we determine which rows are kept
    results_df <- crude_rates_w_pop %>% mutate(wt_SEX = pop_total/population.total,
                                               std_rate = sum(crude_rate * object@kScalar * wt_SEX),
                                               wt = pop/population.total,
                                               std_rate_sub_tot = sum(crude_rate * object@kScalar * wt) ) %>%
      select(std_rate, std_rate_sub_tot) %>%
      distinct()
    
    # Calculating of standard errors
    standard_errors <- crude_rates_w_pop %>% mutate(wt = (pop_total/population.total)**2,
                                                    standard_error = sqrt(sum(std_error**2 * wt)),
                                                    wt_tot = (pop/population.total)**2,
                                                    standard_error_sub_tot = sum(std_error**2 * wt_tot) ) %>%
      select(standard_error, standard_error_sub_tot) %>%
      distinct()
    
    results_df$standard_error <- standard_errors$standard_error
    results_df$standard_error_sub_tot <- standard_errors$standard_error_sub_tot
    results_df$MEASURE <- row$MEASURE
    results_df$SEX <- row$SEX
    results_df$REF_AREA <- row$REF_AREA
    results_df$TIME_PERIOD <- row$TIME_PERIOD
    
    # Calculating confidence intervals
    results_df<- results_df %>% mutate(up_ci = std_rate + 1.96 * standard_error, 
                                       lo_ci = std_rate - 1.96 * standard_error)
    
    return(results_df)
  } 
  
  
  # Calculates crude rates for M and Females. We also store total numerators and
  # denominators for Total calculation later.
  CalculateHelper_2 <- function(row) {
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
  
  
  
  #Indicator is the same for all, so we just look at the first one
  # For this group, we calculate the crude rates only
  if (object@data_raw$MEASURE[1] %in% c(PR.type.one, PR.type.two,PR.type.three, PR.type.four, PR.type.five, PR.type.seven) ){
    
    # Calculating the total crude rate
    std_rates <- object@crude_rates %>% dplyr::rowwise() %>% 
      do(result = CalculateHelper_2(.)) %>% unnest(result)
    
    
    object@standardized_rates <- as.data.frame(std_rates)
    
  }
  # For group six ('PRPPANTI'), we calculate the default.
  else {
    std_rates <- object@crude_rates %>% dplyr::rowwise() %>% 
      do(result = CalculateHelper(.)) %>% unnest(result)
    
    object@standardized_rates <- as.data.frame(std_rates)
    
  }
  
  return(object)
  
})



setMethod('PreprocessData','PR', function(object) {
  # We remove 'T' rows
  df_raw_sub <- subset(object@data_raw, SEX != "T")
  
  # We only keep rows that are NUM or DEN
  df_raw_sub <- subset(df_raw_sub, UNIT %in% c('NUM', 'DEN'))
  # Pivoting
  df_raw_sub <- df_raw_sub %>% tidyr::pivot_wider(id_cols = c(SEX,MEASURE, TIME_PERIOD, REF_AREA, AGE),
                                                  names_from = UNIT,
                                                  values_from = OBS_VALUE )
  
  # We process only if non empty
  if (dim(df_raw_sub)[1] != 0) {
  
  
  # dropping rows that have empty denominator
  df_raw_sub <- df_raw_sub %>% filter(!is.na(DEN))
  
  # # replacing all empty numerators with 0
  df_raw_sub$NUM <- df_raw_sub$NUM %>% replace(is.na(.), 0)
  
  # For these indicators, denominator is multiplied by 365
  if (df_raw_sub$MEASURE[1] %in% c("PROPOUDD","PRABOUDD") ) {
    # Normalizing denominator by 365
    df_raw_sub$DEN <- df_raw_sub$DEN * 365
    
  }
  
  }
  
  object@preprocessed <- df_raw_sub
  return(object)
  
})



