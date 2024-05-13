#######################################################################
#        DEFINING THE CLASS OBJECT FOR CALCULATING MEASURES         #
#######################################################################


# Defining global parameters used in classes and functions
IC.type.one <- c("ICISCACR", "ICISCDSR", "ICISCACM", "ICISCMACR", "ICISCMDSR")
IC.type.two <- c("ICCHFACR", "ICCHFDSR", "ICCHFACM", "ICCHFMACR", "ICCHFMDSR", "ICCHFCF")


PS.type.one <- c("POSTSESP", "POSTDVSP", "POSTPESP", "FORBPROC")
PS.type.two <- c("OBSTVDWI", "OBSTVDWO")

PE.type <- c("COSKCOST", "MTSKCOST", "PMSKCOST", "HPRTIPAT", "RHTIPAT", "HPREXCLA",
             "RHPEXCLA", "HPRGOASK", "RHPGOASK", 'HPRIPDEC', "RHPIPDEC", 'HPRCORES', "RHPCORES", 
             "RHPTIPAT")

MP.type <- c("MPIPRESP", "MPCBRESP", "MPIPTIME", "MPCBTIME", "MPIPEXPL", "MPCBEXPL",
             "MPIPINVO", "MPCBINVO")

MH.type.one <- c('SUICMENT', 'MORTSUMD', 'MORTSUMS')
MH.type.two <- c("EXCEBIPO", "EXCESCHI")

AA.type.one <- c('ADMRASTH', 'ADMRCOPD', 'ADMRCHFL', 'ADMRHYPT', 'ADMRDBUC')
AA.type.two <- c('ADMRDBLE', 'PATRDBLE')


AC.type.one <- c('MORTAMIO', 'MORTAMII', 'MORTHSTO', 'MORTHSTI', 'MORTISTO', 'MORTISTI')
AC.type.two <- c("IHWTHIPS", "IHWTSAME", "IHWTDONE", "IHWTDTWO")


EC.type <- c('MORTACUT','MORTHOSP', "ADMMOCAN", 'ADMMOCAR', 'ADMMOCHR', 'ADMMOALZ', 'ADMMOALL',
             'ADMDECAN', 'ADMDECAR', 'ADMDECHR', 'ADMDEALZ', 'ADMDEALL')

PR.type.one <- c('PRDMPCDD', 'PRDMPADD', 'PRABCQDD', 'PRACCUNS')
PR.type.two <- c('PRBZOZDD', 'PRBZLAOP')
PR.type.three <- c('PRPPOPFM')
PR.type.four <- c('PROPOUDD')
PR.type.five <- c('PROPPCOU')
PR.type.six <- c("PRPPANTI")
PR.type.seven <- c("PRABOUDD")


default.output.types <- c(AA.type.one, PR.type.six, AA.type.two, AC.type.one, MH.type.one, IC.type.one, IC.type.two)

as.is.types <- c("PE_Indicator", 'MP_Indicator')

# Define the root of classes.
setClass('Indicators')

# Define the Patient Safety class
setClass("PS", contains = 'Indicators')

# Define the PE Indicator class
setClass("PE", contains = 'Indicators')

# Define the PE Indicator class
setClass("PR", contains = 'Indicators')

# Define the Intensive Care Indicator class
setClass("IC", contains = 'Indicators')

# Define the Mental Health Indicator class
setClass("MH", contains = 'Indicators')
# Define the MP Indicator class
setClass("MP", contains = 'Indicators')

# Define the AA Indicator class
setClass("AA", contains = 'Indicators')

# Define the AC Indicator class
setClass("AC", contains = 'Indicators')

# Define the EC Indicator class
setClass("EC", contains = 'Indicators')



default.parameters <- c(
  population = "matrix",
  data_raw = "data.frame",
  preprocessed = "data.frame",
  crude_rates = "data.frame",
  standardized_rates = 'data.frame',
  kScalar = "numeric")

# Define the Indicator class
setClass("PS_Indicator",
         slots = default.parameters,
         contains = "PS")
# Define the Indicator class
setClass("PR_Indicator",
         slots = default.parameters,
         contains = "PR")
# Define the Indicator class
setClass("EC_Indicator",
         slots = default.parameters,
         contains = "EC")
# Define the Indicator class
setClass("PE_Indicator",
         slots = default.parameters,
         contains = "PE")
setClass("IC_Indicator",
          slots = default.parameters,
          contains = "IC")
setClass("MH_Indicator",
          slots = default.parameters,
          contains = "MH")
setClass("MP_Indicator",
         slots = default.parameters,
         contains = "MP")
setClass("AA_Indicator",
         slots = default.parameters,
         contains = "AA")
setClass("AC_Indicator",
          slots = default.parameters,
          contains = "AC")


# GENERIC CLASS METHOD DEFINITIONS
# Define a generic method for CalculateStdRate. By default, we calculate standardized
# rates with and standard errors.
setGeneric(name = "PreprocessData", def = function(object) standardGeneric("PreprocessData"))
setGeneric(name = "CalculateCrdRate", def = function(object) standardGeneric("CalculateCrdRate"))

setGeneric(name = "CalculateStdRate", def = function(object) standardGeneric("CalculateStdRate"))

setMethod('PreprocessData','Indicators', function(object) {
  
  #object <- indicators[[1]]
  # We remove 'T' rows
  df_raw_sub <- subset(object@data_raw, SEX != "T")
  # We only keep rows that are NUM or DEN
  df_raw_sub <- subset(df_raw_sub, UNIT %in% c('NUM', 'DEN'))
  
  #df_raw_sub <- subset(df_raw, SEX != "T")
  # Pivoting dataframe so that we have seperate columns for numerators and denominators
  df_raw_sub <- df_raw_sub %>% tidyr::pivot_wider(id_cols = c(SEX,MEASURE, TIME_PERIOD, REF_AREA, AGE), 
                                                  names_from = UNIT, 
                                                  values_from = OBS_VALUE )
  
  # convert to numerics
  #browser()
  # if not empty, keep processing
  if (dim(df_raw_sub)[1] != 0) {
    
    # dropping rows that have empty denominator
    df_raw_sub <- df_raw_sub %>% filter(!is.na(DEN))
    
    # replacing all empty numerators with 0
    df_raw_sub$NUM <- df_raw_sub$NUM %>% replace(is.na(.), 0) 
  }

  
  object@preprocessed <- df_raw_sub
  return(object)
  
})

setMethod('CalculateCrdRate', 'Indicators', function(object) {

  
  # splitting into seperate dataframes by SEX, indicator, time and REF_AREA and then we calculate the crude rates by age groups
  df_crude_rates <- object@preprocessed %>% dplyr::group_by(SEX, MEASURE, TIME_PERIOD, REF_AREA) %>% tidyr::nest() %>%
    mutate(data = purrr::map(data,~ .x %>%
                               mutate(crude_rate = NUM/DEN)))
  object@crude_rates <- df_crude_rates
  
  
  return(object)
  
})


# The default method for calculating the standardized rate. We assume it must be calculated by default and calculate the standardized rate, 
# standard errors, total subpart and confidence intervals.
setGeneric("CalculateStdRate", function(object) {
  # Merges standardization population to crude rate data
  MergePopulationAndRates <- function(row) {
    # Accessing the population matrix and transposing it
    pop_t <- t(object@population)
    # We select the population for the SEX and the total
    pop_sub <- as.data.frame(pop_t[,c(row$SEX,'T')])
    # We have only one row remaining. We rename it as weight. This adds more functionality (less need for hardcoding)
    colnames(pop_sub) <- c("pop", 'pop_total')
    
    # Merging populations to crude rate data
    crude_rates_w_pop <- base::merge(row$data, pop_sub, by.x = "AGE",
                                     by.y = "row.names", 
                                     all.x = FALSE, 
                                     all.y = TRUE)
    return(crude_rates_w_pop)
  }
  # Calculates the standardized rates
  CalculateHelper <- function(row) {
    crude_rates_w_pop <- MergePopulationAndRates(row)
    population.total <- sum(object@population['T',])
    
    # Standard errors of crude rates
    std.error.num <- crude_rates_w_pop$crude_rate * object@kScalar**2  * (1 - crude_rates_w_pop$crude_rate  )
    # Calculation of standard errors for crude rates
    crude_rates_w_pop$std_error <- sqrt( std.error.num/ crude_rates_w_pop$DEN )
    # replace any NaNs with 0
    crude_rates_w_pop$std_error <- crude_rates_w_pop$std_error %>% replace(is.na(.), 0)
    
    # We calculate the standardized rate by sex and standard errors. 
    # Moreover, we also calculate the 'first' part of the total rate and standard error
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
                                                    standard_error_sub_tot = sum((std_error**2) * wt_tot)) %>%
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
  
  
  std_rates <- object@crude_rates %>% dplyr::rowwise() %>% 
    do(result = CalculateHelper(.)) %>% unnest(result)
  
  
  object@standardized_rates <- as.data.frame(std_rates)
  
  
  return(object)
}) 



# Reading in definitions of different indicators
class.files <- list.files('./classes', full.names=TRUE)
# running each file
for (f in class.files) {
  source(f, local = TRUE)
}


### FUNCTIONS

# Calculates and processes the dataframe for output. 
# Based on what each indicator should return,
# the function calculates them. For each indicator, the total standardized rate is calculated.
# For some indicators, we also calculate the confidence intervals while for others we do not calculate them,
# but merge them from the data file to the dataframe. 
# After calculation, the function pivots the dataframes and then finally joins them all together.
GetOutputDataframe <- function(df) {
  
  # All auxiliary variables for indicators. We drop all/any of them. 
  # If any new 'auxiliary' variables are created they must be included in this vector. Otherwise they appear in the 
  # output as new columns.
  vars.to.drop <- c("std_den_sub","std_num_sub", "standard_error_sub_tot", "std_rate_sub_tot", 'standard_error', 'NUM', 'DEN')
  # splitting dataframe by indicators
  dfs <- split(df, df$MEASURE)
  

  
  # Helper function that takes a dataframe as input, drops auxiliary variables, adds new rows to it
  # and pivots it to long form.
  PivotAndCleanDF <- function(df, new_rows_df, variables.to.long, additional.variables.to.drop, vars.to.drop) {
    # drop auxiliary sub vars
    df <- df %>% select(-any_of(append(vars.to.drop, additional.variables.to.drop) ) )
    # Adding total rows 
    df <- dplyr::bind_rows(df, new_rows_df)
    
    # Pivoting to result format
    df <- df %>% pivot_longer(variables.to.long,
                              names_to =  "value",
                              values_to = "number",
                              values_drop_na = FALSE)
    return(df)
    
  }
  # Helper function that calculates the total rows for standardized rate and standard error. 
  # The function formats into right form. By default we have standardized rate and confidence interval as output
  DefaultOutput <- function(df_input) {
    
    # Generate 'Total' rows from M and F
    # We calculate the total standard error and
    # standardized rate as a sum of the sub parts.
    total_rows <- df_input %>% 
      group_by(MEASURE, REF_AREA, TIME_PERIOD) %>% 
      summarize(std_rate = sum(std_rate_sub_tot),
                standard_error = sqrt(sum(standard_error_sub_tot)),
                .groups = "keep") %>% 
      mutate(SEX = "T")
    
    # Calculating confidence intervals
    total_rows <- total_rows %>% mutate(up_ci = std_rate + 1.96 * standard_error,
                                        lo_ci = std_rate - 1.96 * standard_error)  %>% select(-standard_error)
    
    
    res <- PivotAndCleanDF(df_input, 
                                new_rows_df = total_rows, 
                                variables.to.long =  c("std_rate", "up_ci", "lo_ci"),
                                additional.variables.to.drop = c(),
                                vars.to.drop = vars.to.drop)
    return(res)
    
  }
  # Creating an indicator class for each indicator dataframe
  output <- lapply(names(dfs), function(indicator_name) {
    # By default, we output the standardized rate for M,T AND F, and the CI's.
    if (indicator_name %in% default.output.types) {
      output_df <- DefaultOutput(dfs[[indicator_name]])
      return(output_df)
      
    }
    
    # For PS, we return only the total crude rates. We drop Males and Females.
    else if (indicator_name %in% append(PS.type.one, PS.type.two)) {
      total_rows <- dfs[[indicator_name]] %>% 
        group_by(MEASURE, REF_AREA, TIME_PERIOD) %>% 
        summarize(tot_num = sum(NUM),
                  tot_den = sum(DEN),
                  std_rate = tot_num/tot_den,
                  .groups = "keep") %>%
        select(-c(tot_num,tot_den)) %>%
        mutate(SEX = "T")
      total_rows <- total_rows %>% select(-any_of(append(vars.to.drop, c('NUM', 'DEN')) ))
      
      # Pivoting to result format
      output_df <- total_rows %>% pivot_longer(std_rate,
                                              names_to =  "value",
                                              values_to = "number",
                                              values_drop_na = FALSE)
     
    return(output_df)  
       
    } 
    # For AC type two, we just return the standardized rate for M,F and T
    else if (indicator_name %in% AC.type.two) {
         total_rows <- dfs[[indicator_name]] %>% 
           group_by(MEASURE, REF_AREA, TIME_PERIOD) %>% 
           summarize(std_rate = sum(std_rate_sub_tot),
                     .groups = "keep") %>% 
           mutate(SEX = "T")
         total_rows <- total_rows %>% select(-any_of(vars.to.drop) )
         
         
         output_df <- PivotAndCleanDF(dfs[[indicator_name]], 
                                         new_rows_df = total_rows, 
                                         variables.to.long = c("std_rate"),
                                         additional.variables.to.drop = c( "up_ci", 'lo_ci'),
                                         vars.to.drop = vars.to.drop)
         return(output_df)
      
    } # The output is the crude rates for M,F and T
    else if (indicator_name %in% c(EC.type, PR.type.one, PR.type.two, PR.type.three, PR.type.four, PR.type.five, PR.type.seven)) {
      total_rows <- dfs[[indicator_name]] %>% 
             group_by(MEASURE, REF_AREA, TIME_PERIOD) %>% 
             summarize(tot_num = sum(NUM),
                       tot_den = sum(DEN),
                       std_rate = tot_num/tot_den,
                       .groups = "keep") %>%
             select(-c(tot_num,tot_den)) %>%
             mutate(SEX = "T")
      total_rows <- total_rows %>% select(-any_of(append(vars.to.drop, c('NUM', 'DEN')) ))
          
      output_df <- PivotAndCleanDF(dfs[[indicator_name]], 
                                   new_rows_df = total_rows, 
                                   variables.to.long = c("std_rate"),
                                   additional.variables.to.drop = c("up_ci", "lo_ci"),
                                   vars.to.drop = vars.to.drop)
      return(output_df)
      
    } # For MH type two, we use standardized numerator and denominator to calculate the standardized rate. Output is only that
    else if (indicator_name %in% MH.type.two) {
      total_rows <- dfs[[indicator_name]] %>% 
        group_by(MEASURE, REF_AREA, TIME_PERIOD) %>% 
        summarize(std_num = sum(std_num_sub),
                  std_den = sum(std_den_sub),
                  std_rate = std_num/std_den,
                  .groups = "keep" 
                  ) %>%
        select(-c(std_num,std_den)) %>%
        mutate(SEX = "T")
      output_df <- PivotAndCleanDF(dfs[[indicator_name]], 
                                      new_rows_df = total_rows, 
                                      variables.to.long = c("std_rate"),
                                      additional.variables.to.drop = c("up_ci", "lo_ci"),
                                      vars.to.drop = vars.to.drop)
      
      return(output_df)
      
    }
    else {
      # This case corresponds to 'as is'.
      output_df <- dfs[[indicator_name]]
      
    }
    
    })
  
  return(output)
}

# Define a function to create Indicator objects from a data frame row
InferIndicatorType <- function(df_raw) {
  
  
  # Dividing dataframe into smaller ones based on indicator.
  dfs <- split(df_raw, df_raw$MEASURE)
  
  # Creating an indicator class for each indicator dataframe
  
  indicators <- lapply(names(dfs), function(indicator_name) {
    
    if (indicator_name %in% PS.type.one) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePS1(indicator_data)
    }
    else if ( indicator_name %in% PS.type.two )  {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePS2(indicator_data)
    }
    
    else if (indicator_name %in% AA.type.one ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAA1(indicator_data)
      
    }
    else if ( indicator_name %in% AA.type.two ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAA2(indicator_data)
      
    }
    else if (indicator_name %in% PE.type) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePE(indicator_data)
    }
    else if (indicator_name == "MORTAMIO") {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAC1(indicator_data)
      
    }
    else if (indicator_name == "MORTAMII") {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAC2(indicator_data)
    }
    else if (indicator_name == "MORTHSTO") {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAC3(indicator_data)
      
    }
    else if (indicator_name == "MORTHSTI") {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAC4(indicator_data)
      
    }
    else if (indicator_name == "MORTISTO") {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAC5(indicator_data)
    }
    else if (indicator_name == "MORTISTI") {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAC6(indicator_data)
      
    }
    else if (indicator_name %in% AC.type.two) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeAC7(indicator_data)
      
    }
    else if (indicator_name %in% IC.type.one ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeIC1(indicator_data)
      
    }
    else if ( indicator_name %in% IC.type.two ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeIC2(indicator_data)
      
    }
    else if (indicator_name %in% MH.type.one) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeMH1(indicator_data)
      
    }
    else if (indicator_name %in% MH.type.two ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeMH2(indicator_data)
      
    }
    else if (indicator_name %in% EC.type ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeEC1(indicator_data)
    }
    else if (indicator_name %in% PR.type.one ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePR1(indicator_data)
    }
    else if (indicator_name %in% PR.type.two) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePR2(indicator_data)
    }
    else if (indicator_name %in% PR.type.three ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePR3(indicator_data)
    }
    else if (indicator_name %in% PR.type.four ) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePR4(indicator_data)
    }
    else if (indicator_name %in% PR.type.five) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePR5(indicator_data)
    }
    else if (indicator_name %in% PR.type.six) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePR6(indicator_data)
    }
    else if (indicator_name %in% PR.type.seven) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializePR7(indicator_data)
    }
    
    else if (indicator_name %in% MP.type) {
      indicator_data <- as.data.frame(dfs[[indicator_name]])
      indicator <- InitializeMP(indicator_data)
    }
    
    else stop(cat("Indicator:",indicator,"not found. Must be added to IndicatorClasses.R"))
  })

}


