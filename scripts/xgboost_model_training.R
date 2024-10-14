# xgboost_model_training.R
# This script trains an XGBoost model using stratified k-fold cross-validation.
# It evaluates model performance using ROC AUC and F1 scores, and saves the best model.


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
  
  # Prepare DMatrix for XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, features]), label = train_data[[target_col]])
  dtest <- xgb.DMatrix(data = as.matrix(test_data[, features]), label = test_data[[target_col]])
  
  # Define XGBoost parameters based on hyperparameter tuning results
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.1,               # Learning rate
    gamma = 0.2,             # Minimum loss reduction
    max_depth = 2,           # Maximum tree depth
    min_child_weight = 4,    # Minimum sum of instance weight needed in a child
    subsample = 0.75,        # Subsample ratio of the training instances
    colsample_bytree = 0.5,  # Subsample ratio of columns when constructing each tree
    scale_pos_weight = sum(train_data[[target_col]] == 0) / sum(train_data[[target_col]] == 1)  # Balancing positive and negative classes
  )
  
  # Train the XGBoost model
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # Save the trained model for this fold
  saveRDS(model, file = paste0("outputs/xg_model_fold_rf", i, ".rds"))
  
  # ------------------------ Training Set Evaluation ------------------------
  
  # Predict on the training set
  train_preds_XG <- predict(model, as.matrix(train_data[, features]))
  
  # Calculate ROC AUC for the training set
  roc_result_train <- rocit(score = train_preds_XG, class = as.factor(train_data[[target_col]]), negref = "0", method = "bin")
  
  # Print AUC for the training set
  auc_train <- roc_result_train$AUC
  print(paste("Fold", i, "Training AUC:", auc_train))
  
  # ------------------------ validation Set Evaluation ------------------------
  
  # Predict on the validation set
  validation_pred_XG <- predict(model, dtest)
  
  # Calculate ROC AUC for the validation set
  roc_result <- rocit(score = validation_pred_XG, class = as.factor(test_data[[target_col]]), negref = "0", method = "bin")
  pred_class <- ifelse(validation_pred_XG > 0.5, "1", "0")
  
  # Print AUC for the validation set
  auc_test <- roc_result$AUC
  print(paste("Fold", i, "validation AUC:", auc_test))
  
  # ------------------------ Plot ROC Curve ------------------------
  
  # Plot ROC curve for the first fold
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)
  }
  
  # ------------------------ Confusion Matrix and F1 Score ------------------------
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

# ------------------------ Select and Save the Best Model by auc and F1score------------------------
best_model_XG_RF <- readRDS("outputs/xg_model_fold_rf3.rds")

# ------------------------ Stop Parallel Backend ------------------------

# Stop the parallel cluster
stopCluster(cl)
registerDoSEQ()
