# data_split_randomforest_imputation.R
# This script performs data splitting and handles missing data imputation using Random Forest (missRanger).
# It splits the dataset into training and testing sets, converts specified columns to factors,
# performs imputation on both sets, and exports the processed data.

# Load necessary libraries
library(dplyr)
library(caret)
library(missRanger)

# ------------------------ Data Splitting ------------------------

# Read the dataset
data5 <- read.csv("outputs/data105.csv")

# Set seed for reproducibility
set.seed(234)

# Perform stratified random sampling based on the target variable 'ED'
train_index <- createDataPartition(data5$ED, p = 0.8, list = FALSE)
training_data <- data5[train_index, ]
test_data1 <- data5[-train_index, ]

# Export the training and testing datasets
write.csv(training_data, "outputs/data107_train.csv", row.names = FALSE)
write.csv(test_data1, "outputs/data107_test.csv", row.names = FALSE)

# ------------------------ Imputation on Training Set ------------------------

# Note: In Excel, exclude the target variable 'ED' from 'data107_train.csv' and save the result as 'train_RF.csv'

# Read the training data without the target variable
test6 <- read.csv('train_RF.csv')

# List of categorical variables to be converted to factors
factorCols <- c(
  "DMDEDUC",
  "SMQ020",
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
  "SXQ280",
  "ProstateExam",
  "CDQ001",
  "CDQ010"
)

# Convert specified columns to factor type
test6 <- test6 %>%
  mutate(across(all_of(factorCols), as.factor))

# Display the structure of the dataset
str(test6)

# Perform imputation using missRanger
dataRF_train <- missRanger(test6, 
                           seed = 123, 
                           num.trees = 500, 
                           pmm.k = 5, 
                           verbose = 1, 
                           maxiter = 5)

# Export the imputed training data
write.csv(dataRF_train, "outputs/dataRF_train.csv", row.names = FALSE)

# ------------------------ Imputation on Testing Set ------------------------

# Export the testing data to CSV
write.csv(test_data1, "outputs/test_RF.csv", row.names = FALSE)

# Note: In Excel, exclude the target variable 'ED' from 'test_RF.csv' and save the result as 'test_RF.csv'

# Read the testing data without the target variable
dataRF_test <- read.csv("test_RF.csv")

# Convert specified columns to factor type
dataRF_test <- dataRF_test %>%
  mutate(across(all_of(factorCols), as.factor))

# Display the structure of the testing dataset
str(dataRF_test)

# Perform imputation using missRanger
dataRF_test <- missRanger(dataRF_test, 
                          seed = 123, 
                          num.trees = 500, 
                          pmm.k = 5, 
                          verbose = 1, 
                          maxiter = 5)

# Export the imputed testing data
write.csv(dataRF_test, "outputs/dataRF_test.csv", row.names = FALSE)

# ------------------------ Reintegrate Target Variable ------------------------

# Note: In Excel, merge the target variable 'ED' back into 'dataRF_train.csv' and 'dataRF_test.csv'

# After reintegrating, you should have the final imputed datasets with the target variable included.
