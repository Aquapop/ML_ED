#--------lightGBM-----------------------------
data5=read.csv('datanet.csv')
# Split features and target variable
target_col <- "ED"  # Name of your target variable column
features <- setdiff(names(data5), target_col)
label <- data5[[target_col]]

# Preprocessing function-----
preprocess_data <- function(data, factor_cols, target_col) {
  
  data_temp <- data5
  # Extract features and target variable
  X <- data_temp[, setdiff(names(data_temp), target_col)]
  target <- data_temp[[target_col]]
  # Using Borderline-SMOTE
  slsmote_result <- SLS(X, target, K = 5, C = 5, dupSize = 0)
  # Merge original majority class samples and synthetic minority class samples
  data_smote <- rbind(slsmote_result$orig_N, slsmote_result$syn_data)
  # Convert to dataframe
  data_smote_df <- as.data.frame(data_smote)
  # Convert to dataframe, and rename 'class' column to original target column name
  names(data_smote_df)[names(data_smote_df) == "class"] <- target_col
  
  # Print results after SMOTE
  print(paste("After SMOTE: Number of positive =", sum(data_smote_df[[target_col]] == 1)))
  print(paste("After SMOTE: Number of negative =", sum(data_smote_df[[target_col]] == 0)))
  
  # Apply ENN
  ENN_result <- ENNClassif(ED~., data_smote_df, k = 5, dist = "Euclidean", Cl = "0")
  
  # Print results after ENN
  print(paste("After ENN: Number of positive =", sum(ENN_result[[1]][[target_col]] == 1)))
  print(paste("After ENN: Number of negative =", sum(ENN_result[[1]][[target_col]] == 0)))
  
  # Return the dataset filtered through ENN
  return(ENN_result[[1]])
}

k <- 5 
folds <- list()

# Get indices for positive and negative classes
positive_indices <- which(data5$ED == 1)
negative_indices <- which(data5$ED == 0)

# Set random seed
set.seed(6666)

# Stratified sampling
for(i in 1:k) {
  train_indices <- c(sample(positive_indices, length(positive_indices) * (k - 1) / k),
                     sample(negative_indices, length(negative_indices) * (k - 1) / k))
  folds[[i]] <- train_indices
}

# Initialize
evaluation_metrics <- list()
results <- list()
all_preds_Light <- c() # Initialize list to save predictions for each fold
all_actuals_Light <- c()
train_preds_Light <- c()
train_actuals_Light <- c()
f1_scores <- numeric(length(folds))


# Loop through each fold---------
for(i in seq_along(folds)) {
  # Split data
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(data5)), train_indices)
  test_data <- data5[test_indices, ]
  
  # Apply preprocessing function to training data
  train_data <- data5[train_indices, ]
  train_data_smote <- preprocess_data(train_data, factorCols, target_col)
  

  ratio <- sum(data5[[target_col]] == 0) / sum(data5[[target_col]] == 1)
  
  # Prepare LightGBM data
  feature_names <- colnames(data5[, -label])  # Save names of all columns except the target column
  dtrain <- lgb.Dataset(data = as.matrix(train_data_smote[, features]), label = train_data_smote[[target_col]])
  dtest <- lgb.Dataset(data = as.matrix(data5[test_indices, features]), label = as.numeric(data5[test_indices, target_col]))

  # Set LightGBM parameters
  params <- list(
    objective = "binary",
    metric = "auc",
    learning_rate = 0.1,
    num_leaves = 31,
    bagging_fraction = 0.8,
    feature_fraction = 0.8,
    bagging_freq = 5,
    #categorical_feature = 2,4,8,9,10,11,12,13,14,15,16,
    scale_pos_weight = ratio 
  )
  
  # Train model
  model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    valids = list(test = dtest),
    early_stopping_rounds = 10
  )
  
   test_matrix <- as.matrix(test_data[, features])
   
  # Calculate ROC for training set-------
  # Predict on the training dataset using the trained model
  train_preds <- predict(model, as.matrix(train_data[, features]))
  roc_result_train <- rocit(score = train_preds, class = as.factor(train_data[[target_col]]), negref = "0", method = "bin")
  
  # Append current fold's training predictions and actual labels to storage vectors
  train_preds_Light <- c(train_preds_Light, train_preds)
  train_actuals_Light <- c(train_actuals_Light, train_data[[target_col]])
  
  # Print AUC value
  auc_value_train <- roc_result_train$AUC
  print(paste("AUC value train:", auc_value_train))
  
  
  # Predict and calculate ROC------
  pred <- predict(model, test_matrix)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "0",method = "bin")
  pred_class <- ifelse(pred > 0.5, "1", "0")# Set threshold to reduce false positives

  # Append current fold's predictions and actual labels to storage vectors
  all_preds_Light <- c(all_preds_Light, pred)
  all_actuals_Light <- c(all_actuals_Light, test_data[[target_col]])
  
  # Print AUC value
  auc_value <- roc_result$AUC
  print(paste("AUC value:", auc_value))
  
  # Plot ROC curve
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)  # Use plot function for the first fold
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)  # Use lines function for subsequent folds
  }
  
  # Calculate confusion matrix
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  cm <- confusionMatrix(pred_class, test_class)
  
  # Extract precision and recall from the confusion matrix
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  
  # Calculate F1 Score
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # Save this fold's F1 Score
  f1_scores[i] <- f1_score
  
  # Store evaluation metrics for each fold
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
  
  # Save results
  results[[i]] <- list(model = model)
}

print(evaluation_metrics)
print(f1_scores)

# Feature importance---------
importance <- lgb.importance(model)
print(importance)

# Order feature importance for better visualization
importance <- importance[order(-importance$Gain), ]
ggplot(importance, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#82B29A") +
  coord_flip() +  # Flip the axes for easier reading of feature names
  labs(x = "Feature", y = "Importance (Gain)", title = "Feature Importance") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),  # Center title
        panel.background = element_blank(),  # Set transparent background
        panel.border = element_rect(colour = "black", fill=NA, size=1), # Add black border, no fill
        plot.background = element_rect(fill = "transparent", colour = NA))   # Set chart background to transparent


# SHAP---------------
# Get the test dataset
test_indices <- setdiff(seq_len(nrow(data5)), folds[[3]])
test_data <- data5[test_indices, ]

# Select the positive dataset
positive_test_data <- test_data[test_data[[target_col]] == 1, ]

# Convert the test dataset to xgb.DMatrix
positive_X_test_matrix <- as.matrix(positive_test_data[, -which(names(positive_test_data) == target_col)])

# Calculate SHAP values for the positive dataset
positive_shap_values <- predict(model, positive_X_test_matrix, type = 'contrib')


# Convert SHAP values to a dataframe, excluding the last column of expected output
positive_shap_values_df <- as.data.frame(positive_shap_values[, -ncol(positive_shap_values)])

# Create shapviz object using the filtered test dataset

# Use the modified positive_X_test_matrix as the X_pred parameter
sv <- shapviz(object = model, X_pred = positive_X_test_matrix, X = positive_X_test_matrix)

# Plot a summary graph of SHAP values
sv_plot <- sv_importance(sv, kind = "beeswarm", max_display = 23)
print(sv_plot)











#XGBoost
data4=read.csv('data.csv')
# Separate features and target variable
target_col <- "ED"  # Name of your target variable column
features <- setdiff(names(data4), target_col)
label <- data4[[target_col]]

# Preprocessing function-----
preprocess_data <- function(data, factor_cols, target_col) {
  data_temp <- data4
  # Extract features and target variable
  X <- data_temp[, setdiff(names(data_temp), target_col)]
  target <- data_temp[[target_col]]
  # Use Borderline-SMOTE
  slsmote_result <- SLS(X, target, K = 5, C = 5, dupSize = 0)
  # Merge original majority class samples and synthetic minority class samples
  data_smote <- rbind(slsmote_result$orig_N, slsmote_result$syn_data)
  # Convert to dataframe
  data_smote_df <- as.data.frame(data_smote)
  # Convert to dataframe, and rename 'class' column to original target column name
  names(data_smote_df)[names(data_smote_df) == "class"] <- target_col
  # Apply ENN
  ENN_result <- ENNClassif(ED~., data_smote_df, k = 5, dist = "Euclidean", Cl = "0")
  # Return the dataset filtered through ENN
  return(ENN_result[[1]])
}

# Initialize fold list
k <- 5 
folds <- list()

# Get indices for positive and negative classes
positive_indices <- which(data4$ED == 1)
negative_indices <- which(data4$ED == 0)

# Set random seed
set.seed(6666)

# Stratified sampling
for(i in 1:k) {
  train_indices <- c(sample(positive_indices, length(positive_indices) * (k - 1) / k),
                     sample(negative_indices, length(negative_indices) * (k - 1) / k))
  folds[[i]] <- train_indices
}

# Initialize
evaluation_metrics <- list()
results <- list()
all_preds <- list() # Initialize list to save predictions for each fold
# Initialize plot
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves for All Folds")
abline(a = 0, b = 1, col = "gray", lty = 2)  # Add diagonal line


# Loop through each fold---------
for(i in seq_along(folds)) {
  # Split data
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(data4)), train_indices)
  test_data <- data4[test_indices, ]
  
  # Apply preprocessing function to training data
  train_data <- data4[train_indices, ]
  train_data_smote <- preprocess_data(train_data, factorCols, target_col)
  
  ratio <- sum(data4[[target_col]] == 0) / sum(data4[[target_col]] == 1)
  
  # Prepare data
  feature_names <- colnames(data4[, -label])  # Save names of all columns except the target column
  dtrain <- xgb.DMatrix(data = as.matrix(train_data_smote[, features]), label = train_data_smote[[target_col]])
  dtest <- xgb.DMatrix(data = as.matrix(data4[test_indices, features]), label = as.numeric(data4[test_indices, target_col]))
  
     params1 = list(
         booster = "gbtree",
         objective = "binary:logistic",
         eval_metric = "auc",
         eta = 0.05,               # Learning rate
         gamma = 0.2,              # Minimum loss reduction
         max_depth = 4,            # Maximum depth of the tree
         min_child_weight = 4,     # Minimum sum of instance weight (hessian) needed in a child
         subsample = 0.5,          # Subsample ratio of the training instances
         colsample_bytree = 0.75,  # Subsample ratio of columns when constructing each tree
         scale_pos_weight = ratio  # Balancing of positive and negative weights
      )
  
  # Train model with best parameters
  model <- xgb.train(
    params = params1,
    data = dtrain,
    nrounds = 100, 
  )
  
  # Draw feature importance graph
  importance <- xgb.importance(feature_names = feature_names, model = model)
  head(importance)
  xgb.ggplot.importance(importance)
  
  # Predict and calculate ROC
  pred <- predict(model, dtest)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "0",method = "bin")
  pred_class <- ifelse(pred > 0.5, "1", "0")# Set threshold to reduce false positives
  
  # Print AUC value
  auc_value <- roc_result$AUC
  print(paste("AUC value:", auc_value))
  
  # Plot ROC curve
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)  # Use plot function for the first fold
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)  # Use lines function for subsequent folds
  }
  
  # Calculate confusion matrix
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  cm <- confusionMatrix(pred_class, test_class)
  
  # Extract precision and recall from the confusion matrix
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  
  # Calculate F1 Score
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # Save this fold's F1 Score
  f1_scores[i] <- f1_score
  
  # Store evaluation metrics for each fold
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
  
  # Save this fold's predictions
  all_preds[[i]] <- pred
  
  # Save results
  results[[i]] <- list(model = model)
}
# Add legend
legend("topleft", legend = paste("Fold", 1:length(folds)), col = 1:length(folds), lty = 1)  

# Output evaluation metrics for each fold---------
print(evaluation_metrics)
print(f1_scores)



#CatBoost
# CatBoost
data6=read.csv('data307.csv')
# Separate features and target variable
target_col <- "ED"  # Name of your target variable column
features <- setdiff(names(data6), target_col)
label <- data6[[target_col]]

# Preprocessing function-----
preprocess_data <- function(data, factor_cols, target_col) {
  
  data_temp <- data6
  # Extract features and target variable
  X <- data_temp[, setdiff(names(data_temp), target_col)]
  target <- data_temp[[target_col]]
  # Use Borderline-SMOTE
  slsmote_result <- SLS(X, target, K = 5, C = 5, dupSize = 0)
  # Merge original majority class samples and synthetic minority class samples
  data_smote <- rbind(slsmote_result$orig_N, slsmote_result$syn_data)
  # Convert to dataframe
  data_smote_df <- as.data.frame(data_smote)
  # Convert to dataframe, and rename 'class' column to original target column name
  names(data_smote_df)[names(data_smote_df) == "class"] <- target_col
  
  # Print results after SMOTE
  print(paste("After SMOTE: Number of positive =", sum(data_smote_df[[target_col]] == 1)))
  print(paste("After SMOTE: Number of negative =", sum(data_smote_df[[target_col]] == 0)))
  
  # Apply ENN
  ENN_result <- ENNClassif(ED~., data_smote_df, k = 5, dist = "Euclidean", Cl = "0")
  
  # Print results after ENN
  print(paste("After ENN: Number of positive =", sum(ENN_result[[1]][[target_col]] == 1)))
  print(paste("After ENN: Number of negative =", sum(ENN_result[[1]][[target_col]] == 0)))
  
  # Return the dataset filtered through ENN
  return(ENN_result[[1]])
}

# Initialize-------
# Initialize fold list
k <- 5 
folds <- list()

# Get indices for positive and negative classes
positive_indices <- which(data6$ED == 1)
negative_indices <- which(data6$ED == 0)

# Set random seed
set.seed(6666)

# Stratified sampling
for(i in 1:k) {
  train_indices <- c(sample(positive_indices, length(positive_indices) * (k - 1) / k),
                     sample(negative_indices, length(negative_indices) * (k - 1) / k))
  folds[[i]] <- train_indices
}

# Initialize
evaluation_metrics <- list()
results <- list()
all_preds <- list() # Initialize list to save predictions for each fold
f1_scores <- numeric(length(folds))
# Initialize plot
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves for All Folds")
abline(a = 0, b = 1, col = "gray", lty = 2)  # Add diagonal line

# Loop through each fold---------
for(i in seq_along(folds)) {
  # Split data
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(data6)), train_indices)
  test_data <- data6[test_indices, ]
  
  # Apply preprocessing function to training data
  train_data <- data6[train_indices, ]
  train_data_smote <- preprocess_data(train_data, factor_cols, target_col)
  # Specify weights
  ratio <- sum(data6[[target_col]] == 0) / sum(data6[[target_col]] == 1)
  class_weights <- c(1, ratio)
  
  str(train_data_smote)
  
  # Convert target variable to integer
  train_data_smote[[target_col]] <- as.integer(train_data_smote[[target_col]])
  test_data[[target_col]] <- as.integer(test_data[[target_col]])
  
  # Create CatBoost data pool, specify categorical features if any
  train_pool <- catboost.load_pool(data = train_data_smote[, features, drop = FALSE], label = train_data_smote[[target_col]])
  test_pool <- catboost.load_pool(data = test_data[, features, drop = FALSE], label = test_data[[target_col]])
  
  
  # Define CatBoost parameters
  params <- list(iterations = 100,
                 learning_rate = 0.1,
                 depth = 6,
                 loss_function = 'Logloss',
                 eval_metric = 'AUC',
                 random_seed = 123,
                 od_type = 'Iter',
                 od_wait = 20,
                 class_weights = class_weights)
  
  
  
  # Train the model
  model <- catboost.train(train_pool, test_pool, params = params)
  
  
  # Predict and calculate ROC
  pred <- catboost.predict(model, test_pool)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "1",method = "bin")
  pred_class <- ifelse(pred > 0.5, "1", "0")
  levels(pred_class)
  
  # Print AUC value
  auc_value <- roc_result$AUC
  print(paste("AUC value:", auc_value))
  
  # Plot ROC curve
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)  # Use plot function for the first fold
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)  # Use lines function for subsequent folds
  }
  
  # Calculate confusion matrix
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  cm <- confusionMatrix(pred_class, test_class)
  
  # Extract precision and recall from the confusion matrix
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  
  # Calculate F1 Score
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # Save this fold's F1 Score
  f1_scores[i] <- f1_score
  
  # Store evaluation metrics for each fold
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
  
  # Save this fold's predictions
  all_preds[[i]] <- pred
  
  # Save results
  results[[i]] <- list(model = model)
}
# Output evaluation metrics for each fold---------
print(evaluation_metrics)  
print(f1_scores)
