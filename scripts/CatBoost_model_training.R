# catboost_model_training.R
# This script trains a CatBoost model using stratified k-fold cross-validation.
# It evaluates model performance using ROC AUC and F1 scores, and saves the best model.


# ------------------------ Load Data ------------------------

# Read the filtered training and testing datasets
train_data_final <- read.csv("outputs/dataRF_train_flitered.csv")
test_data_final <- read.csv("outputs/dataRF_test_flitered.csv")

# Define the target variable
target_col <- "ED"  # KIQ400 is equivalent to ED

# Define feature columns by excluding the target variable
features <- setdiff(names(train_data_final), target_col)

# Define categorical feature columns
factorCols <- c(
  "DMDEDUC",
  "DIQ010",
  "PAD590",
  "CDQ010",
  "PAD320",
  "KIQ081"
)

# ------------------------ Data Preprocessing ------------------------

# Convert specified columns to factor type in training data
train_data_final <- train_data_final %>%
  mutate(across(all_of(factorCols), as.factor))
str(train_data_final)

# Convert specified columns to factor type in testing data
test_data_final <- test_data_final %>%
  mutate(across(all_of(factorCols), as.factor))
str(test_data_final)

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
  
  # Define class weights based on class imbalance
  ratio <- sum(train_data_final[[target_col]] == 0) / sum(train_data_final[[target_col]] == 1)
  class_weights <- c(1, ratio)
  
  # Create CatBoost pools for training and testing
  train_pool <- catboost.load_pool(data = train_data[, features, drop = FALSE], label = train_data[[target_col]])
  test_pool <- catboost.load_pool(data = test_data[, features, drop = FALSE], label = test_data[[target_col]])
  
  # Define CatBoost parameters
  params <- list(
    iterations = 100,
    learning_rate = 0.05,
    depth = 4,
    loss_function = 'Logloss',
    boosting_type = "Ordered",
    bootstrap_type = "Bayesian",
    eval_metric = 'AUC',
    random_seed = 123,
    od_type = 'Iter',
    od_wait = 20,
    l2_leaf_reg = 3,
    bagging_temperature = 0.5,
    class_weights = class_weights
  )
  
  # Train the CatBoost model
  model <- catboost.train(train_pool, test_pool, params = params)
  
  # Save the trained model for this fold
  saveRDS(model, file = paste0("cat_model_fold_", i, ".rds"))
  
  # ------------------------ Training Set Evaluation ------------------------
  
  # Predict on the training set
  train_preds_CAT <- catboost.predict(model, train_pool)
  
  # Calculate ROC AUC for the training set
  roc_result_train <- rocit(score = train_preds_CAT, class = as.factor(train_data[[target_col]]), negref = "0", method = "bin")
  
  # Append training predictions and actuals
  # Uncomment if you want to store training predictions and actuals
  # train_preds_CAT <- c(train_preds_CAT, train_preds)
  # train_actuals_CAT <- c(train_actuals_CAT, fold_train_data[[target_col]])
  
  # Print AUC for the training set
  auc_value_train <- roc_result_train$AUC
  print(paste("Fold", i, "Training AUC:", auc_value_train))
  
  # ------------------------ validation Set Evaluation ------------------------
  
  # Predict on the testing set
  validation_preds_CAT <- catboost.predict(model, test_pool)
  
  # Calculate ROC AUC for the validation set
  roc_result <- rocit(score = validation_preds_CAT, class = as.factor(test_data[[target_col]]), negref = "0", method = "bin")
  
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
  pred_class <- ifelse(validation_preds_CAT > 0.5, "1", "0")
  
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
best_model_CAT_RF <- readRDS("cat_model_fold_3.rds")

# ------------------------ Stop Parallel Backend ------------------------

# Stop the parallel cluster
stopCluster(cl)
registerDoSEQ()
