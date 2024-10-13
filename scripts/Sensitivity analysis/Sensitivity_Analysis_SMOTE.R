# Sensitivity_Analysis_SMOTE.R
# This script performs sensitivity analysis using SMOTE (Synthetic Minority Over-sampling Technique) and ENN (Edited Nearest Neighbors)
# for handling class imbalance. It includes data preprocessing, applying SMOTE and ENN, cross-validation with XGBoost,
# evaluation of model performance, and bootstrap analysis.

# Load necessary libraries
library(caret)
library(dplyr)
library(xgboost)
library(rocit)
library(boot)
library(UBL)          
library(smotefamily)     
library(doParallel)

# Set seed for reproducibility
set.seed(6668)

# Read the filtered training and testing datasets
test_data_final <- read.csv("dataRF_test_flitered.csv")
train_data_final <- read.csv("dataRF_train_flitered.csv")

# Define the target column
target_col <- "ED"

# Define the feature columns
features <- setdiff(names(train_data_final), target_col)

# Preprocessing function using SMOTE and ENN
preprocess_data <- function(data, target_col) {
  data_temp <- data
  # Extract features and target variable
  X <- data_temp[, setdiff(names(data_temp), target_col)]
  target <- data_temp[[target_col]]
  
  # Print class distribution before SMOTE
  cat("Before SMOTE: Number of positive =", sum(target == 1), "\n")
  cat("Before SMOTE: Number of negative =", sum(target == 0), "\n")
  
  # Apply SMOTE (using Borderline-SMOTE)
  smote_result <- SMOTE(X, target, perc.over = 100, perc.under = 200)  # Adjust parameters as needed
  
  # Combine original majority class samples with synthetic minority class samples
  data_smote <- smote_result$data
  
  # Rename the target column back to original
  names(data_smote)[names(data_smote) == "class"] <- target_col
  
  # Print class distribution after SMOTE
  cat("After SMOTE: Number of positive =", sum(data_smote[[target_col]] == 1), "\n")
  cat("After SMOTE: Number of negative =", sum(data_smote[[target_col]] == 0), "\n")
  
  # Apply ENN to clean the data
  enn_result <- ENNClassif(as.formula(paste(target_col, "~ .")), data_smote, k = 5, dist = "Euclidean", Cl = "0")
  
  # Print class distribution after ENN
  cat("After ENN: Number of positive =", sum(enn_result[[1]][[target_col]] == 1), "\n")
  cat("After ENN: Number of negative =", sum(enn_result[[1]][[target_col]] == 0), "\n")
  
  # Return the cleaned dataset
  return(enn_result[[1]])
}

# Initialize fold list for cross-validation
k <- 5 
folds <- list()

# Get indices for positive and negative classes
positive_indices <- which(train_data_final[[target_col]] == 1)
negative_indices <- which(train_data_final[[target_col]] == 0)

# Perform stratified sampling to create folds
for(i in 1:k) {
  train_indices <- c(
    sample(positive_indices, length(positive_indices) * (k - 1) / k, replace = FALSE),
    sample(negative_indices, length(negative_indices) * (k - 1) / k, replace = FALSE)
  )
  folds[[i]] <- train_indices
}

# Initialize lists to store evaluation metrics and results
evaluation_metrics <- list()
f1_score_p <- numeric(length(folds))
f1_score_n <- numeric(length(folds))

# Loop through each fold for cross-validation
for(i in seq_along(folds)) {
  
# Split data into training and testing for the current fold
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(train_data_final)), train_indices)
  test_data <- train_data_final[test_indices, ]
  train_data <- train_data_final[train_indices, ]
  
  # Apply SMOTE and ENN to handle class imbalance
  train_data_smote <- preprocess_data(train_data, target_col)
  
  # Prepare data for XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(train_data_smote[, features]), label = train_data_smote[[target_col]])
  dtest <- xgb.DMatrix(data = as.matrix(test_data[, features]), label = test_data[[target_col]])
  
  # Define XGBoost parameters
  params1 <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.1,               # Learning rate
    gamma = 0.2,             # Minimum loss reduction
    max_depth = 2,           # Maximum tree depth
    min_child_weight = 4,    # Minimum sum of instance weight (hessian) needed in a child
    subsample = 0.75,        # Subsample ratio of the training instance
    colsample_bytree = 0.5,  # Subsample ratio of columns when constructing each tree
    scale_pos_weight = sum(train_data_smote[[target_col]] == 0) / sum(train_data_smote[[target_col]] == 1)  # Balancing positive and negative weights
  )
  
  # Train the XGBoost model with the specified parameters
  model <- xgb.train(
    params = params1,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # Save the trained model
  saveRDS(model, file = paste0("xg_model_fold_smote_RF", i, ".rds"))
  
  # Predict on validation data and calculate ROC
  pred <- predict(model, dtest)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "0", method = "bin")
  pred_class <- ifelse(pred > 0.5, "1", "0") 
  
  # Print AUC for test data
  auc_value <- roc_result$AUC
  print(paste("AUC value:", auc_value))
  
  # Plot ROC curve
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)
  }
  
  # Compute confusion matrix
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  cm <- confusionMatrix(pred_class, test_class)
  
  # Extract precision and recall for positive class
  precision_p <- cm$byClass['Pos Pred Value']
  recall_p <- cm$byClass['Sensitivity']
  
  # Calculate F1 Score for positive class
  f1_score_p[i] <- 2 * ((precision_p * recall_p) / (precision_p + recall_p))
  
  # Extract precision and recall for negative class
  precision_n <- cm$byClass['Neg Pred Value']
  recall_n <- cm$byClass['Specificity']
  
  # Calculate F1 Score for negative class
  f1_score_n[i] <- 2 * ((precision_n * recall_n) / (precision_n + recall_n))
  
  # Store evaluation metrics
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
}

# Output evaluation metrics for each fold
print(evaluation_metrics)
print(f1_score_p)
print(f1_score_n)

best_model_XG_SMOTE <- readRDS("xg_model_fold_smote_RF3.rds")

# Define bootstrap function to calculate evaluation metrics
best_model_XG_SMOTE <- function(data, indices) {
  test_sample <- data[indices, ]  # Resample the test set
  predictions <- predict(best_model_XG_SMOTE, as.matrix(test_sample[, features]))
  
  # Classify predictions based on a threshold of 0.5
  pred_class <- ifelse(predictions > 0.50, "1", "0")
  
  # Calculate AUC
  roc_result <- rocit(score = predictions, class = as.factor(test_sample[[target_col]]), negref = "0", method = "bin")
  auc <- roc_result$AUC
  
  # Calculate confusion matrix and related metrics
  conf_matrix <- confusionMatrix(as.factor(pred_class), as.factor(test_sample[[target_col]]), positive = "1")
  sensitivity <- conf_matrix$byClass['Sensitivity']
  specificity <- conf_matrix$byClass['Specificity']
  ppv <- conf_matrix$byClass['Pos Pred Value']
  npv <- conf_matrix$byClass['Neg Pred Value']
  f1_score_p <- 2 * ((ppv * sensitivity) / (ppv + sensitivity))
  f1_score_n <- 2 * ((npv * specificity) / (npv + specificity))
  Accuracy <- conf_matrix$overall['Accuracy']
  KAPPA <- conf_matrix$overall['Kappa']
  
  # Return all metrics
  c(
    AUC = auc,
    Sensitivity = sensitivity,
    Specificity = specificity,
    PPV = ppv,
    NPV = npv,
    F1_Score_Positive = f1_score_p,
    F1_Score_Negative = f1_score_n,
    Accuracy = Accuracy,
    Kappa = KAPPA
  )
}

# Perform Bootstrap Analysis
set.seed(123)  # Ensure reproducibility
results_metricsSMOTE_RF <- boot(data = test_data_final, statistic = best_model_XG_SMOTE, R = 1000)  # R is the number of resamples
