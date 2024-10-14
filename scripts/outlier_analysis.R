# outlier_analysis.R
# This script identifies and handles outliers in the dataset.
# It converts specified columns to factors, detects outliers, and processes them accordingly.

# Load necessary libraries
library(dplyr)

# List of all categorical variables
factorCols <- c(
  "DMDEDUC",
  "SMQ020",
  "ALQ110",
  "PAD590",
  "PAD320",
  "BPQ040A",
  "BPQ090D",
  "DIQ010",
  "MCQ220",
  "KIQ081",
  "KIQ101",
  "KIQ106",
  "KIQ121",
  "KIQ141",
  "KIQ182",
  "KIQ321",
  "KIQ341",
  "KIQ115",
  "SXQ280",
  "SXQ260",
  "SXQ265",
  "SXQ270",
  "SXQ272",
  "STI",
  "CDQ001",
  "CDQ010",
  "CVDFITLV",
  "ProstateExam"
)

test5 = read.csv("test5.csv")

# Display the structure of the dataset
str(test5)

# Convert specified columns to factor variables
for (i in factorCols) {
  test5[, i] <- as.factor(test5[, i])
}  

# Function to handle a single variable for outlier detection
unusual_num <- function(colname, df) {
  # Calculate the number of missing values and the missing rate
  na_num <- length(which(is.na(df[[colname]])))
  na_rate <- paste(round(na_num / length(df[[colname]]) * 100, 2), "%", sep = '')
  
  # Check if the variable is a factor
  if (class(df[[colname]]) == "factor") {
    result <- data.frame(
      'Variable' = colname,
      'Lower Bound' = '-',
      'Upper Bound' = '-',
      'Number of Outliers' = '-',
      'Number of Missing Values' = na_num,
      'Missing Rate' = na_rate
    )
  } else {
    # Calculate summary statistics
    s <- summary(df[[colname]])
    
    # Calculate lower bound l = Q1 - 1.5 * IQR
    IQR <- as.numeric(s[5] - s[2])
    l <- round(as.numeric(s[2] - 1.5 * IQR), 3)
    
    # Calculate upper bound u = Q3 + 1.5 * IQR
    u <- round(as.numeric(s[5] + 1.5 * IQR), 3)
    
    # Calculate the number of outliers
    unusual_num <- length(which((df[[colname]] < l) | (df[[colname]] > u)))
    
    # Create result dataframe
    result <- data.frame(
      'Variable' = colname,
      'Lower Bound' = l,
      'Upper Bound' = u,
      'Number of Outliers' = unusual_num,
      'Number of Missing Values' = na_num,
      'Missing Rate' = na_rate
    )
  }
  
  return(result)
}

# Function to handle multiple variables for outlier detection
unusual_num_multi <- function(var_list, df) {
  results_list <- lapply(var_list, function(colname) {
    unusual_num(colname, df)
  })
  results_df <- do.call(rbind, results_list)
  return(results_df)
}

# Define the list of variables to analyze (excluding categorical and target variables)
all_vars <- names(test5)
var_list <- setdiff(all_vars, c(factorCols, "KIQ400")) 

# Select variables that have outliers based on the analysis
df = test5
results <- unusual_num_multi(var_list, df) # Call the function

# Variables with outliers, excluding specific variables（which do not have outliers ）
var_weird <- setdiff(var_list, c("WTMEC2YR", "SDMVPSU", "SDMVSTRA", "INDFMPIR")) 

# Function to handle outliers by capping them at the calculated bounds
resolve_unusual <- function(colnames, df) {
  res <- df
  for(colname in colnames) {
    s <- summary(res[[colname]])
    IQR <- as.numeric(s[5] - s[2])
    l <- round(as.numeric(s[2] - 1.5 * IQR), 3)
    u <- round(as.numeric(s[5] + 1.5 * IQR), 3)
    
    # Cap values above the upper bound
    res[[colname]][which(res[[colname]] > u)] <- u
    # Cap values below the lower bound
    res[[colname]][which(res[[colname]] < l)] <- l 
  }
  return(res)
}

# Apply outlier handling function
data105 <- resolve_unusual(var_weird, df)
write.csv(data104, "data105.csv", row.names = FALSE)

# Verify the outlier situation after handling
df = data104
results <- unusual_num_multi(var_list, df) # Call the function again
