# Read the imputed training and testing datasets
training_data <- read.csv("outputs/dataRF_train.csv")
test_data1 <- read.csv("outputs/dataRF_test.csv")

# Define the target variable
target_col <- "ED"  # KIQ400 is the target variable, equivalent to ED

# Define the feature columns by excluding the target variable
features <- setdiff(names(training_data), target_col)

# Extract the label from the training data
label <- training_data[[target_col]]

# ------------------------ LightGBM Feature Importance ------------------------

# Prepare data for LightGBM
dtrain_lgb <- lgb.Dataset(data = as.matrix(training_data[, features]), label = label)

# Define LightGBM parameters
params_lgb <- list(
  objective = "binary",
  metric = "binary_logloss"
)

# Train the LightGBM model
model_lgb <- lgb.train(
  params = params_lgb,
  data = dtrain_lgb,
  nrounds = 100,
  verbose = -1
)

# Extract feature importance from LightGBM
importance_lgb <- lgb.importance(model = model_lgb, percentage = TRUE)

# Select the top 20 features based on importance
top_features_lgb <- importance_lgb$Feature[1:20]

# Print the top features from LightGBM
print("Top 20 Features from LightGBM:")
print(top_features_lgb)

# ------------------------ XGBoost Feature Importance ------------------------

# Prepare data for XGBoost
dtrain_xgb <- xgb.DMatrix(data = as.matrix(training_data[, features]), label = label)

# Define XGBoost parameters
params_xgb <- list(
  objective = "binary:logistic",
  eval_metric = "logloss"
)

# Train the XGBoost model
model_xgb <- xgb.train(
  params = params_xgb,
  data = dtrain_xgb,
  nrounds = 100,
  verbose = 0
)

# Extract feature importance from XGBoost
importance_xgb <- xgb.importance(model = model_xgb)

# Select the top 20 features based on importance
top_features_xgb <- importance_xgb$Feature[1:20]

# Print the top features from XGBoost
print("Top 20 Features from XGBoost:")
print(top_features_xgb)

# ------------------------ CatBoost Feature Importance ------------------------

# Prepare data for CatBoost
pool <- catboost.load_pool(data = as.matrix(training_data[, features]), label = label)

# Define CatBoost parameters
params_cat <- list(
  loss_function = "Logloss",
  iterations = 100,
  verbose = FALSE
)

# Train the CatBoost model
model_cat <- catboost.train(pool, params = params_cat)

# Extract feature importance from CatBoost
importance_cat <- catboost.get_feature_importance(model_cat)

# Create a dataframe for CatBoost feature importance
importance_cat_df <- data.frame(
  Feature = features,
  Importance = as.vector(importance_cat)
)

# Order the features by importance in descending order
importance_cat_df <- importance_cat_df %>%
  arrange(desc(Importance))

# Select the top 20 features based on importance
top_features_cat <- importance_cat_df$Feature[1:20]

# Print the top features from CatBoost
print("Top 20 Features from CatBoost:")
print(top_features_cat)

# ------------------------ Identify Common Top Features ------------------------

# Identify the common top features across all three models
selected_features <- Reduce(intersect, list(top_features_lgb, top_features_xgb, top_features_cat))

# Print the selected common features
print("Selected Common Features across LightGBM, XGBoost, and CatBoost:")
print(selected_features)

# ------------------------ Filter Datasets with Selected Features ------------------------

# Subset the training and testing data to include only the selected features and the target variable
train_data_RF <- training_data[, c(selected_features, 'ED')]
test_data_RF <- test_data1[, c(selected_features, 'ED')]

# Export the filtered datasets as CSV files
write.csv(train_data_RF, "outputs/dataRF_train_flitered_20.csv", row.names = FALSE)
write.csv(test_data_RF, "outputs/dataRF_test_flitered_20.csv", row.names = FALSE)

#----------------------Same As XGBoost_model_Training-----------------------------------------
# ------------------------ Load Data ------------------------

# Read the filtered training and testing datasets
train_data_final <- read.csv("outputs/dataRF_train_flitered_20.csv")
test_data_final <- read.csv("outputs/dataRF_test_flitered_20.csv")

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
  saveRDS(model, file = paste0("outputs/xg_model_fold_rf20", i, ".rds"))
  
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
best_model_XG_RF_20 <- readRDS("outputs/xg_model_fold_rf203.rds")

# --------------------------bootstrap----------------------------------------
# Define bootstrap function for XGBoost
bootstrap_metrics_XG_RF_20 <- function(data, indices) {
  test_sample <- data[indices, ]  # Resampled test set
  predictions <- predict(best_model_XG_RF_20, as.matrix(test_sample[, features_XG]))
  
  # Predict classes based on threshold 0.5
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
  f1_score_p <- ifelse((ppv + sensitivity) > 0, 2 * ((ppv * sensitivity) / (ppv + sensitivity)), 0)
  f1_score_n <- ifelse((npv + specificity) > 0, 2 * ((npv * specificity) / (npv + specificity)), 0)
  Accuracy <- conf_matrix$overall['Accuracy']
  KAPPA <- conf_matrix$overall['Kappa']
  
  # Return all metrics
  c(AUC = auc, Sensitivity = sensitivity, Specificity = specificity, PPV = ppv, NPV = npv,
    F1_Score_Pos = f1_score_p, F1_Score_Neg = f1_score_n, Accuracy = Accuracy, Kappa = KAPPA)
}

# Execute Bootstrap for XGBoost RF15
set.seed(123)  # Ensure reproducibility
bootstrap_metrics_XG_RF_20 <- boot(data = test_data_final, statistic = bootstrap_metrics_XG_RF_20, R = 1000)  # R is the number of resampling iterations
