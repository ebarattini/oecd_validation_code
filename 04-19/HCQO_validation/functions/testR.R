path <- "C:/Users/barattini_e/OneDrive - OECD/Documents/01_PROJECTS/Validation/validation_code/draft/HCQO_validation/data"
setwd(path)

getwd()

# Remove leading and trailing whitespace from column names
names(df_raw) <- trimws(names(df_raw))

# Trim all character fields in the data frame
df_raw[] <- lapply(df_raw, function(x) if (is.character(x)) return(trimws(x, whitespace = "\"?\\s*")) else x)

# Drop empty columns
df_raw <- df_raw[, sapply(df_raw, function(x) !all(is.na(x)))]

# Print cleaned data frame structure
print(str(df_raw))


# Rename columns correctly and remove any trailing quotes or spaces from column names
names(df_raw) <- gsub('\"', '', names(df_raw))  # Remove quotes from column names
names(df_raw) <- gsub('^\\s+|\\s+$', '', names(df_raw))  # Trim spaces from column names

# Drop columns that are completely empty
df_raw <- df_raw[, colSums(is.na(df_raw) | df_raw == "") < nrow(df_raw)]

# Print the final structure of the data frame to ensure it's correct
print(names(df_raw))
print(str(df_raw))

