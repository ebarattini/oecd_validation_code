


# Initializes a IC type 1 indicator. 
InitializeIC1 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y45T54', 'Y55T64', 'Y65T74', 'Y75T84','Y_GE85')
  row.names <- c('M', 'F', 'T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(3991, 8926, 17276, 16617, 7171)
  data.females <- c(1687, 3540, 9512, 15067, 13599)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("IC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  
  
}

InitializeIC2 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y45T54', 'Y55T64', 'Y65T74', 'Y75T84','Y_GE85')
  row.names <- c('M', 'F', 'T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(3031, 6275, 13919, 19459, 15292)
  data.females <- c(903, 2330, 7924, 18734, 26481)
  data.total <- data.males + data.females
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100
  indicator <- new("IC_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
}