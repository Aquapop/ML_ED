# feature_selection.R
# This script performs feature selection using LightGBM, XGBoost, and CatBoost.
# It identifies the most important features from each model and selects the common top features.
# The filtered datasets with selected features are exported for further analysis.


# ------------------------ Load Data ------------------------

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

# Select the top 25 features based on importance
top_features_lgb <- importance_lgb$Feature[1:25]

# Print the top features from LightGBM
print("Top 25 Features from LightGBM:")
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

# Select the top 25 features based on importance
top_features_xgb <- importance_xgb$Feature[1:25]

# Print the top features from XGBoost
print("Top 25 Features from XGBoost:")
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

# Select the top 25 features based on importance
top_features_cat <- importance_cat_df$Feature[1:25]

# Print the top features from CatBoost
print("Top 25 Features from CatBoost:")
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
write.csv(train_data_RF, "outputs/dataRF_train_filtered.csv", row.names = FALSE)
write.csv(test_data_RF, "outputs/dataRF_test_filtered.csv", row.names = FALSE)

