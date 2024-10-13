# lightgbm_model_training.R
# This script trains a LightGBM model using stratified k-fold cross-validation.
# It evaluates model performance using ROC AUC and F1 scores, and saves the best model.

# Load necessary libraries
library(lightgbm)
library(caret)
library(dplyr)
library(doParallel)
library(foreach)
library(rocit)

# ------------------------ Load Data ------------------------

# Read the filtered training and testing datasets
train_data_final <- read.csv("outputs/dataRF_train_flitered.csv")
test_data_final <- read.csv("outputs/dataRF_test_flitered.csv")

# Define the target variable
target_col <- "ED"  # KIQ400 is equivalent to ED

# Define feature columns by excluding the target variable
features <- setdiff(names(train_data_final), target_col)

# Extract the label from the training data
label <- train_data_final[[target_col]]

# ------------------------ Stratified K-Fold Splitting ------------------------

# Initialize number of folds
k <- 5

# Initialize list to store fold indices
folds <- list()

# Get indices for positive and negative classes
positive_indices <- which(train_data_final[[target_col]] == 1)
negative_indices <- which(train_data_final[[target_col]] == 0)

# Set seed for reproducibility
set.seed(6668)

# Perform stratified k-fold splitting
for(i in 1:k) {
  # Sample indices for training
  train_pos <- sample(positive_indices, length(positive_indices) * (k - 1) / k, replace = FALSE)
  train_neg <- sample(negative_indices, length(negative_indices) * (k - 1) / k, replace = FALSE)
  train_indices <- c(train_pos, train_neg)
  
  # Store training indices for this fold
  folds[[i]] <- train_indices
}

# ------------------------ Initialize Parallel Backend ------------------------

# Detect number of cores and set up parallel processing
num_cores <- parallel::detectCores() - 1  # Reserve one core for the system
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# ------------------------ Initialize Storage Structures ------------------------

# Initialize lists to store evaluation metrics
evaluation_metrics <- list()

# Initialize vectors to store F1 scores for positive and negative classes
f1_score_p <- numeric(length(folds))
f1_score_n <- numeric(length(folds))

# ------------------------ Model Training and Evaluation ------------------------

# Loop through each fold for cross-validation
for(i in seq_along(folds)) {
  # Define training and testing indices for this fold
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(train_data_final)), train_indices)
  
  # Split the data into training and testing sets for this fold
  train_data <- train_data_final[train_indices, ]
  test_data <- train_data_final[test_indices, ]
  
  # Prepare LightGBM datasets
  dtrain <- lgb.Dataset(data = as.matrix(train_data[, features]), label = train_data[[target_col]],
                        categorical_feature = c(17, 18, 19, 20, 21, 22))  # Adjust categorical_feature indices as needed
  dtest <- lgb.Dataset(data = as.matrix(test_data[, features]), label = test_data[[target_col]])
  
  # Define LightGBM parameters based on hyperparameter tuning results
  best_params <- list(
    objective = "binary",
    metric = "auc",
    learning_rate = 0.1,
    num_leaves = 31,
    bagging_fraction = 0.6,
    feature_fraction = 0.6,
    bagging_freq = 5,
    scale_pos_weight = sum(train_data[[target_col]] == 0) / sum(train_data[[target_col]] == 1)  # Handle class imbalance
  )
  
  # Train the LightGBM model
  model <- lgb.train(
    params = best_params,
    data = dtrain,
    nrounds = 100,
    valids = list(test = dtest),
    early_stopping_rounds = 10,
    verbose = -1  # Suppress verbose output
  )
  
  # Save the trained model for this fold
  saveRDS(model, file = paste0("model_fold_Light", i, ".rds"))
  
  # ------------------------ Training Set Evaluation ------------------------
  
  # Predict on the training set
  train_preds_Light <- predict(model, as.matrix(train_data[, features]))
  
  # Calculate ROC AUC for the training set
  roc_result_train <- rocit(score = train_preds_Light, class = as.factor(train_data[[target_col]]), negref = "0", method = "bin")
  
  # Print AUC for the training set
  auc_value_train <- roc_result_train$AUC
  print(paste("Fold", i, "Training AUC:", auc_value_train))
  
  # ------------------------ validation Set Evaluation ------------------------
  
  # Predict on the validation set
  validation_preds_Light <- predict(model, as.matrix(test_data[, features]))
  
  # Calculate ROC AUC for the validation set
  roc_result <- rocit(score = validation_preds_Light, class = as.factor(test_data[[target_col]]), negref = "0", method = "bin")
  
  # Print AUC for the validation set
  auc_value <- roc_result$AUC
  print(paste("Fold", i, "validation AUC:", auc_value))
  
  # ------------------------ Plot ROC Curve ------------------------
  
  # Plot ROC curve for each fold
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)
  }
  
  # ------------------------ Confusion Matrix and F1 Score ------------------------
  
  # Convert probabilities to class labels based on a threshold of 0.5
  pred_class <- ifelse(pred > 0.5, "1", "0")
  
  # Convert to factor for confusion matrix
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  
  # Compute confusion matrix
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
  
  # Store evaluation metrics for this fold
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
}

# ------------------------ Final Evaluation Metrics ------------------------

# Print all confusion matrices
print(evaluation_metrics)

# Print F1 scores for positive and negative classes
print(f1_score_p)
print(f1_score_n)

# ------------------------ Select and Save the Best Model ------------------------

# Determine which fold had the highest validation AUC
# Since AUCs were printed earlier, manually identify the best model
# For demonstration, we'll assume the best model is from the third fold
best_model_Light_RF <- readRDS("model_fold_Light3.rds")

# ------------------------ Stop Parallel Backend ------------------------

# Stop the parallel cluster
stopCluster(cl)
registerDoSEQ()
