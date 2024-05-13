


# Initializes a IC type 1 indicator. 
InitializeAA1 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('15-19Y', '20-24Y', '25-29Y', '30-34Y', '35-39Y', '40-44Y','45-49Y', '50-54Y', '55-59Y', '60-64Y', '65-69Y', '70-74Y', '75-79Y', '80-84Y', '85Y_OVER')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(30691981, 33469335, 34508354, 34894399, 35002373, 35208558, 34660372, 33945806, 31047338, 27306071, 23725699, 17515803, 13214632, 8764367, 6354136)
  data.females <- c(29111324, 31890350, 33499971, 34352178, 34701217, 35185172, 34893929, 34644220, 32459999, 29538678, 26657966, 21242908, 18041732, 13862761, 13905892)
  data.total <- data.males + data.females
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  #print(rowSums(population))
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100000
  indicator <- new("AA_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
  
}

InitializeAA2 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('15-34Y', '35-44Y', '45-54Y', '55-59Y', '60-64Y', '65-74Y', '75Y_OVER')
  row.names <- c('M', 'F','T')
  
  # The data entries of the population matrix. Note that we read in row by row
  data.males <- c(133564069, 70210931, 68606178, 31047338, 27306071, 41241502, 28333135)
  data.females <- c(128853823, 69886389, 69538149, 32459999, 29538678, 47900874, 45810385)
  data.total <- data.males + data.females
  
  # Creating standardization matrix
  population <- rbind(data.males,data.females,data.total)
  
  # Naming the rows and columns of the matrix.
  base::colnames(population) <- col.names 
  base::rownames(population) <- row.names 
  
  
  # Defining the scalar by which crude rate is multiplied by.
  kScalar <- 100000
  indicator <- new("AA_Indicator", 
                   data_raw = df_raw, 
                   preprocessed = data.frame(),
                   standardized_rates = data.frame(),
                   crude_rates = data.frame(),
                   population = population,
                   kScalar = kScalar)
}