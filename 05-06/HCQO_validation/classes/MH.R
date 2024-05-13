


# Initializes a MH type 1 indicator.
# Corresponds to these indicators 'SUICMENT', 'MORTSUMD', 'MORTSUMS','ICISCMACR', 'ICISCMDSR'
InitializeMH1 <- function(df_raw) {
  col.names <- c('Y15T19', 'Y20T24', 'Y25T29', 'Y30T34', 'Y35T39', 'Y40T44','Y45T49', 'Y50T54', 'Y55T59Y', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y85T_O')
  row.names <- c('M', 'F','T')

  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(30691981, 33469335, 34508354, 34894399, 35002373, 35208558, 34660372, 33945806, 31047338, 27306071, 23725699, 17515803, 13214632, 8764367, 6354136)
  data.females <- c(29111324, 31890350, 33499971, 34352178, 34701217, 35185172, 34893929, 34644220, 32459999, 29538678, 26657966, 21242908, 18041732, 13862761, 13905892)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("MH_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  
  
}
# Initializes a MH type 2 indicator.
# Corresponds to these indicatos "EXCEBIPO", "EXCESCHI"
InitializeMH2 <- function(df_raw) {
# Defining column and row names
  col.names <- c('Y15T19', 'Y20T24', 'Y25T29', 'Y30T34', 'Y35T39', 'Y40T44','Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74')
  row.names <- c('M', 'F','T')
# Creating matrix
number.of.cells <- length(col.names) * length(row.names)

# The data entries of the population matrix. Note that we read in row by row
data.males <- c(30691981, 33469335, 34508354, 34894399, 35002373, 35208558, 34660372, 33945806, 31047338, 27306071, 23725699, 17515803)
data.females <- c(29111324, 31890350, 33499971, 34352178, 34701217, 35185172, 34893929, 34644220, 32459999, 29538678, 26657966, 21242908)
data.total <- data.males + data.females

# Creating standardization matrix
population <- rbind(data.males,data.females,data.total)

# Naming the rows and columns of the matrix.
base::colnames(population) <- col.names 
base::rownames(population) <- row.names 


# Defining the scalar by which crude rate is multiplied by.
kScalar <- 1
indicator <- new("MH_Indicator", 
                 data_raw = df_raw, 
                 preprocessed = data.frame(),
                 standardized_rates = data.frame(),
                 crude_rates = data.frame(),
                 population = population,
                 kScalar = kScalar)
return(indicator)
}


# The standardization method for MH indicators.
# Add explanation
setMethod("CalculateStdRate", "MH", function(object) {
  # Merges standardization population to crude rate data
  MergePopulationAndRates <- function(row) {
    # Accessing the population matrix and transposing it
    pop_t <- t(object@population)
    # We select the population for the sex and the total
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
    
    # Calculating standard errors
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
    results_df<- results_df %>% mutate(up_ci = std_rate + 1.96 * standard_error, lo_ci = std_rate - 1.96 * standard_error)
    
    return(results_df)
  } 
  
  # Calculates weighted numerators and denominators
  CalculateHelper2 <- function(row) {
    crude_rates_w_pop <- MergePopulationAndRates(row)
    population.total <- sum(object@population['T',])
    
    # We calculate the standardized rate by the sex and also standard errors. Moreover, we also calculate the 'first' part of the over sex rate and standard error
    # In function(), we determine which rows are kept
    results_df <- crude_rates_w_pop %>% mutate(wt_SEX = pop_total/population.total, 
                                               std_SEX_num = sum(NUM * wt_SEX),
                                               std_SEX_den = sum(DEN * wt_SEX),
                                               std_rate = std_SEX_num/std_SEX_den,
                                               wt = pop/population.total,
                                               std_num_sub = sum(NUM * wt),
                                               std_den_sub = sum(DEN * wt)) %>%
      select(std_rate, std_num_sub, std_den_sub) %>%
      distinct()
    
    
    
    results_df$MEASURE <- row$MEASURE
    results_df$SEX <- row$SEX
    results_df$REF_AREA <- row$REF_AREA
    results_df$TIME_PERIOD <- row$TIME_PERIOD
    return(results_df)
    
  }
  
  #Indicator is the same for all, so we just look at the first one
  if (object@data_raw$MEASURE[1] %in% MH.type.one) {
  
  std_rates <- object@crude_rates %>% dplyr::rowwise() %>% 
    do(result = CalculateHelper(.)) %>% unnest(result)
  
  
  object@standardized_rates <- as.data.frame(std_rates)
  
  }
  # Calculation for EXCEBIPO AND EXCESCHI. We use weighted numerators and denominators.
  else {
    std_rates <- object@crude_rates %>% dplyr::rowwise() %>% 
      do(result = CalculateHelper2(.)) %>% unnest(result)
  
    object@standardized_rates <- as.data.frame(std_rates)
    
  }
  
  return(object)

})