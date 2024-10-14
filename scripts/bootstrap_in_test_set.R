# bootstrap_in_test_set.R
# This script performs bootstrap evaluation on the test set for XGBoost, LightGBM, and CatBoost models.
# It calculates various performance metrics such as AUC, Sensitivity, Specificity, PPV, NPV, F1 Scores, Accuracy, and Kappa.


# ------------------------ XGBoost Bootstrap Evaluation ------------------------

# Load XGBoost model
best_model_XG_RF <- readRDS("outputs/xg_model_fold_rf3.rds")

# Load training and testing data
train_data_final_XG <- read.csv("outputs/dataRF_train_flitered.csv")
test_data_final_XG <- read.csv("outputs/dataRF_test_flitered.csv")

# Define features
features_XG <- setdiff(names(train_data_final_XG), target_col)

# Define label
label_XG <- train_data_final_XG[[target_col]]

#prediction on the test set
test_preds_XG <-predict(best_model_XG_RF, as.matrix(test_data_final_XG[,features]))

# 计算AUC
roc_result <- rocit(score = test_preds_XG, class = as.factor(test_data_final[[target_col]]), negref = "0", method = "bin")
auc_value <- roc_result$AUC
print(paste("Test AUC:", auc_value))


# Define bootstrap function for XGBoost
bootstrap_metrics_XG_RF <- function(data, indices) {
  test_sample <- data[indices, ]  # Resampled test set
  predictions <- predict(best_model_XG_RF, as.matrix(test_sample[, features_XG]))
  
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

# Execute Bootstrap for XGBoost
set.seed(123)  # Ensure reproducibility
results_metrics_XG_RF <- boot(data = test_data_final_XG, statistic = bootstrap_metrics_XG_RF, R = 1000)  # R is the number of resampling iterations

# ------------------------ LightGBM Bootstrap Evaluation ------------------------

# Load LightGBM model
best_model_Light_RF <- readRDS("outputs/model_fold_Light3.rds")

# Load training and testing data
train_data_final_Light <- read.csv("outputs/dataRF_train_flitered.csv")
test_data_final_Light <- read.csv("outputs/dataRF_test_flitered.csv")

# Define features
features_Light <- setdiff(names(train_data_final_Light), target_col)

# Define label
label_Light <- train_data_final_Light[[target_col]]

#prediction on the test set
test_preds_Light <-predict(best_model_Light_RF, as.matrix(test_data_final_Light[,features]))

# 计算AUC
roc_result <- rocit(score = test_preds_Light, class = as.factor(test_data_final[[target_col]]), negref = "0", method = "bin")
auc_value <- roc_result$AUC
print(paste("Test AUC:", auc_value))


# Define bootstrap function for LightGBM
bootstrap_metrics_Light_RF <- function(data, indices) {
  test_sample <- data[indices, ]  # Resampled test set
  predictions <- predict(best_model_Light_RF, as.matrix(test_sample[, features_Light]))
  
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
  accuracy <- conf_matrix$overall['Accuracy']
  KAPPA <- conf_matrix$overall['Kappa']
  
  # Return all metrics
  c(AUC = auc, Sensitivity = sensitivity, Specificity = specificity, PPV = ppv, NPV = npv,
    F1_Score_Pos = f1_score_p, F1_Score_Neg = f1_score_n, Accuracy = accuracy, Kappa = KAPPA)
}

# Execute Bootstrap for LightGBM
set.seed(123)  # Ensure reproducibility
results_metrics_Light_RF <- boot(data = test_data_final_Light, statistic = bootstrap_metrics_Light_RF, R = 1000)  # R is the number of resampling iterations

# ------------------------ CatBoost Bootstrap Evaluation ------------------------

# Load CatBoost model
best_model_CAT_RF <- readRDS("outputs/cat_model_fold_3.rds")

# Load training and testing data
train_data_final_CAT <- read.csv("outputs/dataRF_train_flitered.csv")
test_data_final_CAT <- read.csv("outputs/dataRF_test_flitered.csv")

# Define categorical feature columns
factorCols_CAT <- c("DMDEDUC", "DIQ010", "PAD590", "CDQ010", "PAD320", "KIQ081")

# Convert specified columns to factor type in training data
train_data_final_CAT <- train_data_final_CAT %>%
  mutate(across(all_of(factorCols_CAT), as.factor))
str(train_data_final_CAT)

# Convert specified columns to factor type in testing data
test_data_final_CAT <- test_data_final_CAT %>%
  mutate(across(all_of(factorCols_CAT), as.factor))
str(test_data_final_CAT)

# Define features
features_CAT <- setdiff(names(train_data_final_CAT), target_col)

# Define label
label_CAT <- train_data_final_CAT[[target_col]]

test_pool_1 <- catboost.load_pool(data = test_data_final_CAT[, features, drop = FALSE], label = test_data_final_CAT[[target_col]])
test_preds_CAT <-catboost.predict(best_model_CAT_RF, test_pool_1)

# 计算AUC
roc_result <- rocit(score = test_preds_CAT, class = as.factor(test_data_final_CAT[[target_col]]), negref = "0", method = "bin")
auc_value <- roc_result$AUC
print(paste("Test AUC:", auc_value))


# Define bootstrap function for CatBoost
bootstrap_metrics_CAT_RF <- function(data, indices) {
  test_sample <- data[indices, ]  # Resampled test set
  
  # Create CatBoost pool for the resampled test set
  test_pool1 <- catboost.load_pool(data = test_sample[, features_CAT, drop = FALSE], label = test_sample[[target_col]])
  
  # Predict probabilities
  pred <- catboost.predict(best_model_CAT_RF, test_pool1)
  
  # Predict classes based on threshold 0.5
  pred_class <- ifelse(pred > 0.50, "1", "0")
  
  # Calculate AUC
  roc_result <- rocit(score = pred, class = as.factor(test_sample[[target_col]]), negref = "0", method = "bin")
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

# Execute Bootstrap for CatBoost
set.seed(123)  # Ensure reproducibility
results_metrics_CAT_RF <- boot(data = test_data_final_CAT, statistic = bootstrap_metrics_CAT_RF, R = 1000)  # R is the number of resampling iterations

# ------------------------ Final Output ------------------------

# Print evaluation metrics for each model
print("XGBoost Bootstrap Evaluation Metrics:")
print(results_metrics_XG_RF)
print("LightGBM Bootstrap Evaluation Metrics:")
print(results_metrics_Light_RF)
print("CatBoost Bootstrap Evaluation Metrics:")
print(results_metrics_CAT_RF)
