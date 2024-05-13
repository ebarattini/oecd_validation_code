


# Initializes a AA type 1 indicator. 
InitializeAA1 <- function(df_raw) {
  # Defining column and row names
  col.names <- c('Y15T19', 'Y20T24', 'Y25T29', 'Y30T34', 'Y35T39', 'Y40T44','Y45T49', 'Y50T54', 'Y55T59', 'Y60T64', 'Y65T69', 'Y70T74', 'Y75T79', 'Y80T84', 'Y_GE85')
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
  col.names <- c('Y15T34', 'Y35T44', 'Y45T54', 'Y55T59', 'Y60T64', 'Y65T74', 'Y_GE75')
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