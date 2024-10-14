# hyperparameter_tuning.R
# This script performs hyperparameter tuning for LightGBM, XGBoost, and CatBoost models.
# It identifies the best set of parameters based on cross-validated AUC scores



# ------------------------ Load Data ------------------------

# Read the filtered training and testing datasets
train_data_final <- read.csv("outputs/dataRF_train_filtered.csv")
test_data_final <- read.csv("outputs/dataRF_test_filtered.csv")

# Define the target variable
target_col <- "ED"  # KIQ400 is equivalent to ED

# Define feature columns by excluding the target variable
features <- setdiff(names(train_data_final), target_col)

# Extract the label from the training data
label <- train_data_final[[target_col]]

# ------------------------ LightGBM Hyperparameter Tuning ------------------------

# Create LightGBM Dataset
dtrain <- lgb.Dataset(data = as.matrix(train_data_final[, -which(names(train_data_final) == "ED")]), label = train_data_final$ED)

# Define LightGBM parameter grid
param_grid <- expand.grid(
  learning_rate = c(0.01, 0.1, 0.2),
  num_leaves = c(31, 62),
  bagging_fraction = c(0.6, 0.8, 1.0),
  feature_fraction = c(0.6, 0.8, 1.0)
)

# Initialize parallel backend
num_cores <- detectCores() - 1 # Reserve one core for the system
cl <- makeCluster(num_cores)
registerDoParallel(cl)

best_auc <- 0
best_params <- list()
# Perform cross-validation for each parameter combination using parallel processing
results <- foreach(i = 1:nrow(param_grid), .combine = rbind, .packages = 'lightgbm') %dopar% {
  params <- list(
    objective = "binary",
    metric = "auc",
    learning_rate = param_grid$learning_rate[i],
    num_leaves = param_grid$num_leaves[i],
    feature_fraction = param_grid$feature_fraction[i],
    bagging_fraction = param_grid$bagging_fraction[i]
  )
  
  # Perform cross-validation
  cv_result <- lgb.cv(
    params = params,
    data = dtrain,
    nfold = 5,
    nrounds = 100,
    early_stopping_rounds = 10,
    verbose = -1
  )
  
  # Extract best AUC and iteration
 best_iter <- cv_result$best_iter
  best_auc_local <- cv_result$best_score
  
  # Return the results as a data frame
  data.frame(
    auc = best_auc_local,
    learning_rate = params$learning_rate,
    num_leaves = params$num_leaves,
    feature_fraction = params$feature_fraction,
    bagging_fraction = params$bagging_fraction,
    best_iter = best_iter,
    stringsAsFactors = FALSE
  )
}

# Identify the best parameter set based on AUC
best_result_lgb <- results[which.max(results$auc), ]
best_auc_lgb <- best_result$auc
best_params_lgb <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = best_result_lgb$learning_rate,
  num_leaves = best_result_lgb$num_leaves,
  feature_fraction = best_result_lgb$feature_fraction,
  bagging_fraction = best_result_lgb$bagging_fraction
)
best_nrounds_lgb <- best_result_lgb$best_iter

# Print the best LightGBM results
print(paste("Best LightGBM AUC:", best_auc_lgb))
print("Best LightGBM Parameters:")
print(best_params_lgb)
print(paste("Best LightGBM nrounds:", best_nrounds_lgb))

# ------------------------ XGBoost Hyperparameter Tuning ------------------------

num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Define XGBoost parameter grid
param_grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1),          # Learning rate
  max_depth = c(2, 4, 6),           # Maximum depth
  subsample = c(0.5, 0.75, 1),
  colsample_bytree = c(0.5, 0.75, 1),
  min_child_weight = c(2, 4, 6),     # Minimum child weight
  gamma = c(0.1, 0.2, 0.3)            # Gamma parameter
)

# Perform cross-validation for each parameter combination using parallel processing
results <- foreach(i = 1:nrow(param_grid), .combine = rbind, .packages = 'xgboost') %dopar% {
  # Create DMatrix for XGBoost
  dtrain <- xgb.DMatrix(data = as.matrix(train_data_final[, features]), label = train_data_final[[target_col]])
  # Define parameters for this iteration
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = param_grid$eta[i],
    max_depth = param_grid$max_depth[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i],
    min_child_weight = param_grid$min_child_weight[i],
    gamma = param_grid$gamma[i]
  )
  
  # Perform cross-validation
  cv_result <- xgb.cv(
    params = params,
    data = dtrain,
    nfold = 5,
    nrounds = 100,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  # Extract best AUC and iteration
 best_auc_local <- max(cv_result$evaluation_log$test_auc_mean)
  best_iter <- which.max(cv_result$evaluation_log$test_auc_mean)
  
  # Return the results as a data frame
  data.frame(
    auc = best_auc_local,
    eta = params$eta,
    max_depth = params$max_depth,
    subsample = params$subsample,
    colsample_bytree = params$colsample_bytree,
    min_child_weight = params$min_child_weight,
    gamma = params$gamma,
    best_iter = best_iter,
    stringsAsFactors = FALSE
  )
}

stopCluster(cl)
# Identify the best parameter set based on AUC
best_result_xgb <- results[which.max(results$auc), ]
best_auc_xgb <- best_result_xgb$auc
best_params_xgb <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = best_result_xgb$eta,
  max_depth = best_result_xgb$max_depth,
  subsample = best_result_xgb$subsample,
  colsample_bytree = best_result_xgb$colsample_bytree,
  min_child_weight = best_result_xgb$min_child_weight,
  gamma = best_result_xgb$gamma
)
best_nrounds_xgb <- best_result_xgb$best_iter

# Print the best XGBoost results
print(paste("Best XGBoost AUC:", best_auc_xgb))
print("Best XGBoost Parameters:")
print(best_params_xgb)
print(paste("Best XGBoost nrounds:", best_nrounds_xgb))

# ------------------------ CatBoost Hyperparameter Tuning ------------------------

num_cores <- detectCores() - 1 
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Define CatBoost parameter grid
param_grid <- expand.grid(
  learning_rate = c(0.1, 0.05, 0.01),
  depth = c(4, 6, 8, 10, 12),
  l2_leaf_reg = c(1, 3, 5), 
  od_wait = c(10, 20, 30),
  bagging_temperature = c(0.5 , 1 , 5)
)

# 定义分类变量列名
factorCols <- c(
  "DMDEDUC",
  "DIQ010",
  "PAD590",
  "CDQ010",
  "PAD320",
  "KIQ081")

# 将分类列转换为因子类型
for (i in factorCols) {
  train_data_final[, i] <- as.factor(train_data_final[, i])
} 

str(train_data_final)

# Perform cross-validation for each parameter combination using parallel processing
results <- foreach(i = 1:nrow(param_grid), .combine = rbind, .packages = c('catboost')) %dopar% {
  for (col in factorCols) {
    train_data_final[, col] <- as.factor(train_data_final[, col])
  }
  
  # Define parameters for this iteration
  params <- list(
    learning_rate = param_grid$learning_rate[i],
    depth = param_grid$depth[i],
    od_wait = param_grid$od_wait[i],
    l2_leaf_reg = param_grid$l2_leaf_reg[i],
    bagging_temperature = param_grid$bagging_temperature[i],
    loss_function = 'Logloss',
    eval_metric = 'AUC',
    task_type = 'CPU' 
  )
  
  # Create CatBoost Pool
train_pool <- catboost.load_pool(
    data = train_data_final[, -which(names(train_data_final) == target_col)],
    label = train_data_final[[target_col]]
  )
  
  # Perform cross-validation
  fit_result <- catboost.cv(
    pool = train_pool,
    params = params,
    fold_count = 5,
   partition_random_seed = 123
  )
  
  # Extract best AUC and iteration
 best_auc_local <- max(fit_result$test.AUC.mean)
  best_iter <- which.max(fit_result$test.AUC.mean)
  
  
  # Return the results as a data frame
  data.frame(
    auc = best_auc_local,
    learning_rate = params$learning_rate,
    depth = params$depth,
    od_wait = params$od_wait,
    l2_leaf_reg = params$l2_leaf_reg,
    bagging_temperature = params$bagging_temperature,
    best_iter = best_iter,
    stringsAsFactors = FALSE
  )
}

stopCluster(cl)

# Identify the best parameter set based on AUC
best_result_cat <- results[which.max(results$auc), ]
best_auc_cat <- best_result_cat$auc
best_params_cat <- list(
  learning_rate = best_result_cat$learning_rate,
  depth = best_result_cat$depth,
  l2_leaf_reg = best_result_cat$l2_leaf_reg,
  od_wait = best_result_cat$od_wait,
  bagging_temperature = best_result_cat$bagging_temperature
)
best_iter_cat <- best_result_cat$best_iter

# Print the best CatBoost results
print(paste("Best CatBoost AUC:", best_auc_cat))
print("Best CatBoost Parameters:")
print(best_params_cat)
print(paste("Best CatBoost iterations:", best_iter_cat))

