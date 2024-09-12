# LightGBM
dtrain <- lgb.Dataset(data = as.matrix(data5[, -which(names(data5) == "ED")]), label = data5$ED)

# Define parameter grid
param_grid <- list(
  learning_rate = c(0.1, 0.01),
  num_leaves = c(31, 63),
  max_depth = c(-1, 10),
  feature_fraction = c(0.8, 1.0),
  bagging_fraction = c(0.8, 1.0),
  bagging_freq = c(5, 10),
  lambda_l1 = c(0, 0.5),
  lambda_l2 = c(0, 0.5)
)

# Initialize best AUC and parameters
best_auc <- 0
best_params <- list()

# Loop for cross-validation
for (lr in param_grid$learning_rate) {
  for (nl in param_grid$num_leaves) {
    for (md in param_grid$max_depth) {
      for (ff in param_grid$feature_fraction) {
        for (bf in param_grid$bagging_fraction) {
          for (bfreq in param_grid$bagging_freq) {
            for (l1 in param_grid$lambda_l1) {
              for (l2 in param_grid$lambda_l2) {
                # Define current parameter combination
                params <- list(
                  objective = "binary",
                  metric = "auc",
                  learning_rate = lr,
                  num_leaves = nl,
                  max_depth = md,
                  feature_fraction = ff,
                  bagging_fraction = bf,
                  bagging_freq = bfreq,
                  lambda_l1 = l1,
                  lambda_l2 = l2
                )

                # Perform cross-validation using lgb.cv
                cv_result <- lgb.cv(
                  params = params,
                  data = dtrain,
                  nfold = 5,
                  nrounds = 100,
                  early_stopping_rounds = 10,
                  verbose = -1
                )

                # Get all AUC values from iterations and compute average
                auc_evals <- cv_result$record_evals$valid$auc$eval
                mean_auc <- mean(unlist(auc_evals))

                # If average AUC is currently the best, update best parameter combination
                if (mean_auc > best_auc) {
                  best_auc <- mean_auc
                  best_params <- params
                  cat(sprintf("New best AUC: %f with params %s\n", best_auc, toString(params)))
                }
              }
            }
          }
        }
      }
    }
  }
}

print(paste("Best AUC:", best_auc))
print("Best Parameters:")
print(best_params)

# XGBoost
# Adjust target variable name (hyperparameter tuning requires changing 0 and 1 to characters)
data4[[target_col]] <- factor(data4[[target_col]], levels = c('0', '1'), labels = c('good', 'bad'))

grid <- expand.grid(
  nrounds = c(50, 100, 150),  # Boosting rounds
  eta = c(0.01, 0.05, 0.1),  # Learning rate
  max_depth = c(2, 4, 6),     # Decrease maximum depth
  subsample = c(0.5, 0.75, 1),
  colsample_bytree = c(0.5, 0.75, 1),
  min_child_weight = c(2, 4, 6),  # Increase minimum child weight
  gamma = c(0.1, 0.2, 0.3)        # Increase gamma
)

# Use caret for hyperparameter tuning
train_control <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)
print("Training starts")
tuned_model <- try(train(
  ED ~ ., data = data4,
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = grid,
  metric = "AUC"
), silent = FALSE)
print("Training ends")

if ("try-error" %in% class(tuned_model)) {
  print("An error occurred during training.")
} else {
  print("Training completed successfully.")
}


# View the best parameters
best_params <- tuned_model$bestTune
print(best_params)

# Convert best_params to a list
best_params_list <- as.list(best_params)
best_params_list$nrounds <- NULL

# Adjust the elements in the list to fit the parameter format of xgb.train
best_params_list <- lapply(best_params_list, function(x) x[[1]])

                           
# CatBoost
# Load required library
library(catboost)

# Prepare data
train_pool <- catboost.load_pool(data = data6[, -which(names(data6) == "ED")], label = data6$ED)

# Define parameter grid
param_grid <- expand.grid(
  iterations = c(100, 200, 300),
  learning_rate = c(0.1, 0.05, 0.01),
  depth = c(4, 6, 8),
  od_wait = c(10, 20, 30),
  stringsAsFactors = FALSE
)

# Initialize best AUC and parameters
best_auc <- 0
best_params <- list()

# Cross-validation and parameter selection
for(i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  # Set parameters
  fit_params <- list(
    loss_function = 'Logloss',
    eval_metric = 'AUC',
    iterations = params$iterations,
    learning_rate = params$learning_rate,
    depth = params$depth,
    od_type = 'Iter',
    od_wait = params$od_wait,
    random_seed = 123
  )

  # Perform cross-validation
  cv_result <- catboost.cv(
    pool = train_pool,
    params = fit_params,
    fold_count = 5,
    partition_random_seed = 123
  )

  # Get the best AUC value
  max_auc <- max(cv_result$test_auc_mean)
  
  # Update the best parameters and AUC if better
  if (max_auc > best_auc) {
    best_auc <- max_auc;
    best_params <- params;
    cat(sprintf("New best AUC: %f with params:\n", best_auc))
    print(params)
  }
}

# Output the best results
print(paste("Best AUC:", best_auc))
print("Best Parameters:")
print(best_params)
