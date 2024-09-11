# LightGBM
dtrain <- lgb.Dataset(data = as.matrix(data5[, -which(names(data5) == "ED")]), label = data5$ED)

# 定义参数网格
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

# 初始化最佳AUC和参数
best_auc <- 0
best_params <- list()

# 循环进行交叉验证
for (lr in param_grid$learning_rate) {
  for (nl in param_grid$num_leaves) {
    for (md in param_grid$max_depth) {
      for (ff in param_grid$feature_fraction) {
        for (bf in param_grid$bagging_fraction) {
          for (bfreq in param_grid$bagging_freq) {
            for (l1 in param_grid$lambda_l1) {
              for (l2 in param_grid$lambda_l2) {
                # 定义当前参数组合
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

                # 使用lgb.cv进行交叉验证
                cv_result <- lgb.cv(
                  params = params,
                  data = dtrain,
                  nfold = 5,
                  nrounds = 100,
                  early_stopping_rounds = 10,
                  verbose = -1
                )

                # 获取所有迭代的AUC值并计算平均值
                auc_evals <- cv_result$record_evals$valid$auc$eval
                mean_auc <- mean(unlist(auc_evals))

                # 如果平均AUC值是目前最好的，则更新最佳参数组合
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

#XGBoost
#调整目标变量名称（超参数调优需要将0 1改为字符）
data4[[target_col]] <- factor(data4[[target_col]], levels = c('0', '1'), labels = c('good', 'bad'))

grid <- expand.grid(
  nrounds = c(50, 100, 150),# Boosting rounds
  eta = c(0.01, 0.05, 0.1),  # 学习率
  max_depth = c(2, 4, 6),     # 减小最大深度
  subsample = c(0.5, 0.75, 1),
  colsample_bytree = c(0.5, 0.75, 1),
  min_child_weight = c(2, 4, 6),  # 增加最小子权重
  gamma = c(0.1, 0.2, 0.3)        # 增加gamma
)

# 使用caret进行超参数调优
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


# 查看最佳参数
best_params <- tuned_model$bestTune
print(best_params)

# 将best_params转换为列表
best_params_list <- as.list(best_params)
best_params_list$nrounds <- NULL

# 调整list中的元素以符合xgb.train的参数格式
best_params_list <- lapply(best_params_list, function(x) x[[1]])

#CatBoost
# 加载所需的库
library(catboost)

# 准备数据
data5$KIQ400 <- as.factor(data5$KIQ400)  # 确保目标变量是因子类型
train_pool <- catboost.load_pool(data = data5[, -which(names(data5) == "KIQ400")], label = data5$KIQ400)

# 定义参数网格
param_grid <- expand.grid(
  iterations = c(100, 200, 300),
  learning_rate = c(0.1, 0.05, 0.01),
  depth = c(4, 6, 8),
  od_wait = c(10, 20, 30),
  stringsAsFactors = FALSE
)

# 初始化最佳AUC和参数
best_auc <- 0
best_params <- list()

# 交叉验证和参数选择
for(i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  # 设置参数
  fit_params <- list(
    loss_function = 'Logloss',
    eval_metric = 'AUC',
    iterations = params$iterations,
    learning_rate = params$learning_rate,
    depth = params$depth,
    od_type = 'Iter',
    od_wait = params$od_wait,
    random_seed = 123,
    verbose = FALSE
  )

  # 进行交叉验证
  cv_result <- catboost.cv(
    pool = train_pool,
    params = fit_params,
    fold_count = 5,
    partition_random_seed = 123,
    verbose = FALSE
  )

  # 获取最佳AUC值
  max_auc <- max(cv_result$test_auc_mean)
  
  # 更新最佳参数和AUC
  if (max_auc > best_auc) {
    best_auc <- max_auc
    best_params <- params
    cat(sprintf("New best AUC: %f with params:\n", best_auc))
    print(params)
  }
}

# 输出最佳结果
print(paste("Best AUC:", best_auc))
print("Best Parameters:")
print(best_params)


