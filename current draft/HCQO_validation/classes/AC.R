


# Initializes a AC type 1 indicator. 
# Corresponds to MORTAMIO
InitializeAC1 <- function(df_raw) {
  col.names <- c('Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y_GE85')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(23351, 36698, 45461, 47495, 49784, 42042, 38990, 32187, 29640)
  data.females <- c(5090, 8239, 11657, 15116, 9872, 22023, 26630, 29379, 41766)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("AC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar) 
  
}
# Initializes a AC type 2 indicator. 
# Corresponds to MORTAMII
InitializeAC2 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y_GE85')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(36185, 55989, 68949, 73358, 76475, 64954, 60520, 49603, 45559)
  data.females <- c(8125, 13092, 18125, 23328, 29709, 32580, 40008, 44079, 62979)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("AC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  return(indicator)
}

# Initializes a AC type 3 indicator. 
# Corresponds to MORTHSTO
InitializeAC3 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y_GE85')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(4123, 5482, 6372, 7146, 8538, 9526, 10850, 10561, 9980)
  data.females <- c(3075, 4289, 4700, 5013, 6353, 7546, 9863, 11275, 14609)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("AC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  return(indicator)
}

# Initializes a AC type 4 indicator. 
# Corresponds to MORTHSTI
InitializeAC4 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y_GE85')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(6126, 8215, 9649, 11048, 12905, 13921, 15802, 14896, 14372)
  data.females <- c(4787, 6634, 7360, 7754, 9492, 10950, 14168, 16227, 21789)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("AC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  return(indicator)
}

# Initializes a AC type 5 indicator. 
# Corresponds to MORTISTO
InitializeAC5 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y_GE85')
  row.names <- c('M', 'F','T')

  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(7663, 13300, 19705, 26260, 34932, 38722, 42196, 38034, 35326)
  data.females <- c(4318, 6340, 8686, 12515, 19712, 27301, 38057, 46806, 71777)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("AC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  return(indicator)
}

# Initializes a AC type 6 indicator. 
# Corresponds to MORTISTI
InitializeAC6 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y_GE85')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(21751, 40022, 56631, 72207, 86551, 90291, 94168, 85910, 86451)
  data.females <- c(15063, 22106, 29584, 40244, 55898, 68460, 88310, 106217, 179366)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("AC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  return(indicator)
}


# Initializes a AC type 7 indicator. 
# Corresponds to IHWTHIPS, IHWTSAME, IHWTSAME, IHWTDTWO
InitializeAC7 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y65T69', 'Y70T74', 'Y75T79','Y80T84', 'Y_GE85')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(23725699, 17515803, 13214632, 8764367, 6354136)
  data.females <- c(26657966, 21242908, 18041732, 13862761, 13905892)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("AC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  return(indicator)
}



setMethod('PreprocessData','AC', function(object) {
  # We remove 'T' rows
  df_raw_sub <- subset(object@data_raw, SEX != "T")
  # Copy the denominators from IHWTHIPS TO rest
  # For these groups, they dont have denominator. Thus we add it manually. Denominator is the same as...
  if (df_raw_sub$MEASURE[1] %in% c("IHWTDONE", "IHWTSAME", "IHWTDTWO")) {
    
  # Read IHWHTIPS indicator DENOMINATORS
   df_ihwthips_den <- subset(df_raw, ((MEASURE == "IHWTHIPS") & (UNIT == "DEN") )) %>%
     select(-MEASURE) # we do not need indicator for merging
   # Pivot both dataframes 
   df_ihwthips_den <- df_ihwthips_den %>% tidyr::pivot_wider(id_cols = c(SEX, TIME_PERIOD, REF_AREA, AGE), names_from = UNIT, values_from = OBS_VALUE )  
   df_raw_sub <- df_raw_sub %>% tidyr::pivot_wider(id_cols = c(SEX,MEASURE, TIME_PERIOD, REF_AREA, AGE), names_from = UNIT, values_from = OBS_VALUE )  
   
  
   # We merge DENs by AGE,PERIOD and SEX. We only include observations that are 
   # present in both dataframes. 
   df_raw_sub <- base::merge(df_raw_sub, df_ihwthips_den, 
                                    by = c("AGE", 'TIME_PERIOD', 'SEX', 'REF_AREA'), 
                                    all.x = FALSE, 
                                    all.y = FALSE)
   
     }
  
  else {
    # Pivoting
    df_raw_sub <- df_raw_sub %>% tidyr::pivot_wider(id_cols = c(SEX,MEASURE, TIME_PERIOD, REF_AREA, AGE),
                                                    names_from = UNIT,
                                                    values_from = OBS_VALUE )
    
  }

   # dropping rows that have empty denominator
 df_raw_sub <- df_raw_sub %>% filter(!is.na(DEN))

  # # replacing all empty numerators with 0
 df_raw_sub$NUM <- df_raw_sub$NUM %>% replace(is.na(.), 0) 
  
  object@preprocessed <- df_raw_sub
  return(object)
  
})
#
