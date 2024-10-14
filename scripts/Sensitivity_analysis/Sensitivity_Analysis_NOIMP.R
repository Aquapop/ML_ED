# Sensitivity_Analysis_NOIMP.R
# This script performs sensitivity analysis without using the imputation method
# It includes data preparation, feature selection, model training with cross-validation, and bootstrap analysis.


# Set seed for reproducibility
set.seed(234)

# Read the dataset
data5 <- read.csv("data107.csv")

# Split the data into training and testing sets (80% training, 20% testing)
train_index <- createDataPartition(data5$ED, p = 0.8, list = FALSE)
training_data <- data5[train_index, ]
test_data1 <- data5[-train_index, ]

# Save the training and testing data to CSV files
write.csv(training_data, "data107_train.csv", row.names = FALSE)
write.csv(test_data1, "data107_test.csv", row.names = FALSE)

# Separate features and target variable
target_col <- "ED"  # Name of the target variable column
features <- setdiff(names(training_data), target_col)
label <- training_data[[target_col]]

# LightGBM Feature Importance
dtrain_lgb <- lgb.Dataset(data = as.matrix(training_data[, features]), label = label)
params_lgb <- list(objective = "binary")
model_lgb <- lgb.train(params_lgb, dtrain_lgb, nrounds = 100, verbose = -1)
importance_lgb <- lgb.importance(model_lgb)
top_features_lgb <- importance_lgb$Feature[1:25]

print("Top 25 Features from LightGBM:")
print(top_features_lgb)

# XGBoost Feature Importance
dtrain_xgb <- xgb.DMatrix(data = as.matrix(training_data[, features]), label = label)
params_xgb <- list(objective = "binary:logistic")
model_xgb <- xgb.train(params_xgb, dtrain_xgb, nrounds = 100, verbose = 0)
importance_xgb <- xgb.importance(model = model_xgb)
top_features_xgb <- importance_xgb$Feature[1:25]

print("Top 25 Features from XGBoost:")
print(top_features_xgb)

# CatBoost Feature Importance
pool <- catboost.load_pool(data = as.matrix(training_data[, features]), label = label)
params_cat <- list(loss_function = "Logloss", iterations = 100)
model_cat <- catboost.train(pool, params = params_cat)
importance_cat <- catboost.get_feature_importance(model_cat)
importance_cat_df <- data.frame(Feature = features, Importance = as.vector(importance_cat))
importance_cat_df <- importance_cat_df[order(-importance_cat_df$Importance), ]
top_features_cat <- importance_cat_df$Feature[1:25]

print("Top 25 Features from CatBoost:")
print(top_features_cat)

# Select common features across all three models
selected_features <- Reduce(intersect, list(top_features_lgb, top_features_xgb, top_features_cat))

print("Selected Features (Common to all models):")
print(selected_features)

# Create final training and testing datasets with selected features
train_data_final <- training_data[, c(selected_features, 'ED')]
test_data_final <- test_data1[, c(selected_features, 'ED')]
write.csv(test_data_final, "data107_test_filtered.csv")
write.csv(train_data_final, "data107_train_filtered.csv")

# Reload the filtered datasets
test_data_final <- read.csv("data107_test_filtered.csv")
train_data_final <- read.csv("data107_train_filtered.csv")
str(train_data_final)

# Initialize fold list for cross-validation
k <- 5 
folds <- list()
target_col <- "ED" 
features <- setdiff(names(train_data_final), target_col)
label <- train_data_final[[target_col]]

# Get indices for positive and negative classes
positive_indices <- which(train_data_final$ED == 1)
negative_indices <- which(train_data_final$ED == 0)

# Set seed for reproducibility
set.seed(6668)

# Stratified sampling to create folds
for(i in 1:k) {
  train_indices <- c(
    sample(positive_indices, length(positive_indices) * (k - 1) / k, replace = FALSE),
    sample(negative_indices, length(negative_indices) * (k - 1) / k, replace = FALSE)
  )
  folds[[i]] <- train_indices
}

# Initialize lists to store evaluation metrics
evaluation_metrics <- list()
f1_score_p <- numeric(length(folds))
f1_score_n <- numeric(length(folds))

# Loop through each fold for cross-validation
for(i in seq_along(folds)) {
  # Split data into training and validaiton for the current fold
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(train_data_final)), train_indices)
  test_data <- train_data_final[test_indices, ]
  train_data <- train_data_final[train_indices, ]
  
  # Prepare data for XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, features]), label = train_data[[target_col]])
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
    scale_pos_weight = sum(train_data[[target_col]] == 0) / sum(train_data[[target_col]] == 1)  # Balancing positive and negative weights
  )
  
   # Train the XGBoost model with the best parameters
  model <- xgb.train(
    params = params1,
    data = dtrain,
    nrounds = 100
  )
  
  # Save the trained model
  saveRDS(model, file = paste0("xg_model_fold_", i, ".rds"))
  
  # Calculate ROC for training data
  train_preds <- predict(model, as.matrix(train_data[, features]))
  roc_result_train <- rocit(score = train_preds, class = as.factor(train_data[[target_col]]), negref = "0", method = "bin")
  
  # Print AUC for training data
  auc_value_train <- roc_result_train$AUC
  print(paste("Fold", i, "- Training AUC:", round(auc_value_train, 4)))
  
  # Predict on validation data and calculate ROC
  pred <- predict(model, dtest)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "0", method = "bin")
  
  # Classify predictions based on a threshold of 0.5
  pred_class <- ifelse(pred > 0.5, "1", "0")
  
  # Print AUC for test data
  auc_value <- roc_result$AUC
  print(paste("validation AUC value:", auc_value))
  
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

# Load the best model
best_model_XG <- readRDS("xg_model_fold_3.rds")

# Reload the filtered datasets
test_data_final <- read.csv("data107_test_filtered.csv")
train_data_final <- read.csv("data107_train_filtered.csv")
target_col <- "ED" 
features <- setdiff(names(train_data_final), target_col)
best_model_XG <- readRDS("xg_model_fold_3.rds")

# Define bootstrap function to calculate evaluation metrics
bootstrap_metrics_XG <- function(data, indices) {
  test_sample <- data[indices, ]  # Resample the test set
  predictions <- predict(best_model_XG, as.matrix(test_sample[, features]))
  
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
results_metrics1 <- boot(data = test_data_final, statistic = bootstrap_metrics_XG, R = 1000)  # R is the number of resamples
