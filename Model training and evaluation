#lightGBM
data5=read.csv('datanet.csv')
# 分离特征和目标变量
target_col <- "ED"  # 您的目标变量列名
features <- setdiff(names(data5), target_col)
label <- data5[[target_col]]

#预处理函数-----
preprocess_data <- function(data, factor_cols, target_col) {
  
  
  data_temp <- data5
  # 提取特征和目标变量
  X <- data_temp[, setdiff(names(data_temp), target_col)]
  target <- data_temp[[target_col]]
  # 使用 Borderline-SMOTE
  slsmote_result <- SLS(X, target, K = 5, C = 5, dupSize = 0)
  # 合并原始多数类样本和合成少数类样本
  data_smote <- rbind(slsmote_result$orig_N, slsmote_result$syn_data)
  # 转换为数据框
  data_smote_df <- as.data.frame(data_smote)
  #转换为数据框，并将 'class' 列重命名为原始目标列名
  names(data_smote_df)[names(data_smote_df) == "class"] <- target_col
  
  # 打印SMOTE后的结果
  print(paste("After SMOTE: Number of positive =", sum(data_smote_df[[target_col]] == 1)))
  print(paste("After SMOTE: Number of negative =", sum(data_smote_df[[target_col]] == 0)))
  
  
  # 应用ENN
  ENN_result <- ENNClassif(ED~., data_smote_df, k = 5, dist = "Euclidean", Cl = "0")
  
  # 打印ENN后的结果
  print(paste("After ENN: Number of positive =", sum(ENN_result[[1]][[target_col]] == 1)))
  print(paste("After ENN: Number of negative =", sum(ENN_result[[1]][[target_col]] == 0)))
  
  # 返回经过ENN过滤后的数据集
  return(ENN_result[[1]])
}

k <- 5 
folds <- list()

# 获取正负类的索引
positive_indices <- which(data5$ED == 1)
negative_indices <- which(data5$ED == 0)

#设置随机种子
set.seed(6666)

# 分层抽样
for(i in 1:k) {
  train_indices <- c(sample(positive_indices, length(positive_indices) * (k - 1) / k),
                     sample(negative_indices, length(negative_indices) * (k - 1) / k))
  folds[[i]] <- train_indices
}

# 初始化
evaluation_metrics <- list()
results <- list()
all_preds_Light <- c() # 初始化保存每个fold预测结果的列表
all_actuals_Light <- c()
train_preds_Light <- c()
train_actuals_Light <- c()
f1_scores <- numeric(length(folds))


# 对每个折进行循环---------
for(i in seq_along(folds)) {
  # 分割数据
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(data5)), train_indices)
  test_data <- data5[test_indices, ]
  
  #应用预处理函数到训练数据
  train_data <- data5[train_indices, ]
  train_data_smote <- preprocess_data(train_data, factorCols, target_col)
  

  
  ratio <- sum(data5[[target_col]] == 0) / sum(data5[[target_col]] == 1)
  
  
  # 准备LightGBM数据
  feature_names <- colnames(data5[, -label])  # 保存除了目标列之外的所有列名
  dtrain <- lgb.Dataset(data = as.matrix(train_data_smote[, features]), label = train_data_smote[[target_col]])
  dtest <- lgb.Dataset(data = as.matrix(data5[test_indices, features]), label = as.numeric(data5[test_indices, target_col]))

  # 设置LightGBM参数
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
  
  # 训练模型
  model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    valids = list(test = dtest),
    early_stopping_rounds = 10
  )
  
   test_matrix <- as.matrix(test_data[, features])
   
  #训练集的roc计算-------
  # 使用训练好的模型对训练数据集进行预测
  train_preds <- predict(model, as.matrix(train_data[, features]))
  roc_result_train <- rocit(score = train_preds, class = as.factor(train_data[[target_col]]), negref = "0", method = "bin")
  
  # 追加当前折的训练集预测和实际标签到存储向量
  train_preds_Light <- c(train_preds_Light, train_preds)
  train_actuals_Light <- c(train_actuals_Light, train_data[[target_col]])
  
  # 打印AUC值
  auc_value_train <- roc_result_train$AUC
  print(paste("AUC value train:", auc_value_train))
  
  
  # 预测并计算ROC------
  pred <- predict(model, test_matrix)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "0",method = "bin")
  pred_class <- ifelse(pred > 0.5, "1", "0")#设置了阈值之后假阳性才不会那么多

  # 追加当前折的预测和实际标签到存储向量
  all_preds_Light <- c(all_preds_Light, pred)
  all_actuals_Light <- c(all_actuals_Light, test_data[[target_col]])
  
  # 打印AUC值
  auc_value <- roc_result$AUC
  print(paste("AUC value:", auc_value))
  
  # 绘制ROC曲线
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)  # 第一个fold使用plot函数
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)  # 后续的fold使用lines函数
  }
  
  # 计算混淆矩阵
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  cm <- confusionMatrix(pred_class, test_class)
  
  # 从混淆矩阵中提取精确率和召回率
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  
  # 计算F1 Score
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # 保存这一折的F1 Score
  f1_scores[i] <- f1_score
  
  # 存储每个fold的评估指标
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
  
  #保存结果
  results[[i]] <- list(model = model)
}

print(evaluation_metrics)
print(f1_scores)

#XGBoost
data4=read.csv('data.csv')
# 分离特征和目标变量
target_col <- "ED"  # 您的目标变量列名
features <- setdiff(names(data4), target_col)
label <- data4[[target_col]]

#预处理函数-----
preprocess_data <- function(data, factor_cols, target_col) {
  data_temp <- data4
  # 提取特征和目标变量
  X <- data_temp[, setdiff(names(data_temp), target_col)]
  target <- data_temp[[target_col]]
  # 使用 Borderline-SMOTE
  slsmote_result <- SLS(X, target, K = 5, C = 5, dupSize = 0)
  # 合并原始多数类样本和合成少数类样本
  data_smote <- rbind(slsmote_result$orig_N, slsmote_result$syn_data)
  # 转换为数据框
  data_smote_df <- as.data.frame(data_smote)
  #转换为数据框，并将 'class' 列重命名为原始目标列名
  names(data_smote_df)[names(data_smote_df) == "class"] <- target_col
  # 应用ENN
  ENN_result <- ENNClassif(ED~., data_smote_df, k = 5, dist = "Euclidean", Cl = "0")
  # 返回经过ENN过滤后的数据集
  return(ENN_result[[1]])
}

#初始化fold列表
k <- 5 
folds <- list()

# 获取正负类的索引
positive_indices <- which(data4$ED == 1)
negative_indices <- which(data4$ED == 0)

#设置随机种子
set.seed(6666)

# 分层抽样
for(i in 1:k) {
  train_indices <- c(sample(positive_indices, length(positive_indices) * (k - 1) / k),
                     sample(negative_indices, length(negative_indices) * (k - 1) / k))
  folds[[i]] <- train_indices
}

# 初始化
evaluation_metrics <- list()
results <- list()
all_preds <- list() # 初始化保存每个fold预测结果的列表
# 初始化绘图
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves for All Folds")
abline(a = 0, b = 1, col = "gray", lty = 2)  # 添加对角线


# 对每个折进行循环---------
for(i in seq_along(folds)) {
  # 分割数据
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(data4)), train_indices)
  test_data <- data4[test_indices, ]
  
  #应用预处理函数到训练数据
  train_data <- data4[train_indices, ]
  train_data_smote <- preprocess_data(train_data, factorCols, target_col)
  
  ratio <- sum(data4[[target_col]] == 0) / sum(data4[[target_col]] == 1)
  
  
  # 准备数据
  feature_names <- colnames(data4[, -label])  # 保存除了目标列之外的所有列名
  dtrain <- xgb.DMatrix(data = as.matrix(train_data_smote[, features]), label = train_data_smote[[target_col]])
  dtest <- xgb.DMatrix(data = as.matrix(data4[test_indices, features]), label = as.numeric(data4[test_indices, target_col]))
  
     params1 = list(
         booster = "gbtree",
         objective = "binary:logistic",
         eval_metric = "auc",
         eta = 0.05,               # 学习率
         gamma = 0.2,               # 最小损失减少
         max_depth = 4,           # 树的最大深度
         min_child_weight = 4,    # 决定最小叶子节点样本权重和
         subsample = 0.5,         # 训练实例的子样本比例
         colsample_bytree = 0.75,  # 在建立树时对特征采样的比例
         scale_pos_weight = ratio     # 在各类别样本十分不平衡时，把这个参数设置为一个正值，可以使算法更快收敛
      )
  
  
  # 使用最佳参数训练模型
  model <- xgb.train(
    params = params1,
    data = dtrain,
    nrounds = 100, 
  )
  
  #特征重要性图像绘制
  importance <- xgb.importance(feature_names = feature_names, model = model)
  head(importance)
  xgb.ggplot.importance(importance)
  
  # 预测并计算ROC
  pred <- predict(model, dtest)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "0",method = "bin")
  pred_class <- ifelse(pred > 0.5, "1", "0")#设置了阈值之后假阳性才不会那么多
  
  
  
  # 打印AUC值
  auc_value <- roc_result$AUC
  print(paste("AUC value:", auc_value))
  
  # 绘制ROC曲线
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)  # 第一个fold使用plot函数
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)  # 后续的fold使用lines函数
  }
  
  # 计算混淆矩阵
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  cm <- confusionMatrix(pred_class, test_class)
  
  # 从混淆矩阵中提取精确率和召回率
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  
  # 计算F1 Score
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # 保存这一折的F1 Score
  f1_scores[i] <- f1_score
  
  # 存储每个fold的评估指标
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
  
  # 保存这个fold的预测
  all_preds[[i]] <- pred
  
  #保存结果
  results[[i]] <- list(model = model)
}
#添加图例
legend("topleft", legend = paste("Fold", 1:length(folds)), col = 1:length(folds), lty = 1)  

# 输出每个fold的评估指标---------
print(evaluation_metrics)
print(f1_scores)

#CatBoost
data6=read.csv('data307.csv')
# 分离特征和目标变量
target_col <- "ED"  # 您的目标变量列名
features <- setdiff(names(data6), target_col)
label <- data6[[target_col]]

#预处理函数-----
preprocess_data <- function(data, factor_cols, target_col) {
  
  
  data_temp <- data6
  # 提取特征和目标变量
  X <- data_temp[, setdiff(names(data_temp), target_col)]
  target <- data_temp[[target_col]]
  # 使用 Borderline-SMOTE
  slsmote_result <- SLS(X, target, K = 5, C = 5, dupSize = 0)
  # 合并原始多数类样本和合成少数类样本
  data_smote <- rbind(slsmote_result$orig_N, slsmote_result$syn_data)
  # 转换为数据框
  data_smote_df <- as.data.frame(data_smote)
  #转换为数据框，并将 'class' 列重命名为原始目标列名
  names(data_smote_df)[names(data_smote_df) == "class"] <- target_col
  
  # 打印SMOTE后的结果
  print(paste("After SMOTE: Number of positive =", sum(data_smote_df[[target_col]] == 1)))
  print(paste("After SMOTE: Number of negative =", sum(data_smote_df[[target_col]] == 0)))
  
  
  # 应用ENN
  ENN_result <- ENNClassif(ED~., data_smote_df, k = 5, dist = "Euclidean", Cl = "0")
  
  # 打印ENN后的结果
  print(paste("After ENN: Number of positive =", sum(ENN_result[[1]][[target_col]] == 1)))
  print(paste("After ENN: Number of negative =", sum(ENN_result[[1]][[target_col]] == 0)))
  
  # 返回经过ENN过滤后的数据集
  return(ENN_result[[1]])
}

#初始化-------
#初始化fold列表
k <- 5 
folds <- list()

# 获取正负类的索引
positive_indices <- which(data6$ED == 1)
negative_indices <- which(data6$ED == 0)

#设置随机种子
set.seed(6666)

# 分层抽样
for(i in 1:k) {
  train_indices <- c(sample(positive_indices, length(positive_indices) * (k - 1) / k),
                     sample(negative_indices, length(negative_indices) * (k - 1) / k))
  folds[[i]] <- train_indices
}

# 初始化
evaluation_metrics <- list()
results <- list()
all_preds <- list() # 初始化保存每个fold预测结果的列表
f1_scores <- numeric(length(folds))
# 初始化绘图
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves for All Folds")
abline(a = 0, b = 1, col = "gray", lty = 2)  # 添加对角线

# 对每个折进行循环---------
for(i in seq_along(folds)) {
  # 分割数据
  train_indices <- folds[[i]]
  test_indices <- setdiff(seq_len(nrow(data6)), train_indices)
  test_data <- data6[test_indices, ]
  
  #应用预处理函数到训练数据
  train_data <- data6[train_indices, ]
  train_data_smote <- preprocess_data(data, factor_cols, target_col)
  #指定权重
  ratio <- sum(data6[[target_col]] == 0) / sum(data6[[target_col]] == 1)
  class_weights <- c(1, ratio)
  
  str(train_data_smote)
  
  #将目标变量转变为整数型
  train_data_smote[[target_col]] <- as.integer(train_data_smote[[target_col]])
  test_data[[target_col]] <- as.integer(test_data[[target_col]])
  
  # 创建CatBoost数据池，指定类别特征如果有的话
  train_pool <- catboost.load_pool(data = train_data_smote[, features, drop = FALSE], label = train_data_smote[[target_col]])
  test_pool <- catboost.load_pool(data = test_data[, features, drop = FALSE], label = test_data[[target_col]])
  
  
  # 定义CatBoost参数
  params <- list(iterations = 100,
                 learning_rate = 0.1,
                 depth = 6,
                 loss_function = 'Logloss',
                 eval_metric = 'AUC',
                 random_seed = 123,
                 od_type = 'Iter',
                 od_wait = 20,
                 class_weights = class_weights)
  
  
  
  # 训练模型
  model <- catboost.train(train_pool, test_pool, params = params)
  
  
  # 预测并计算ROC
  pred <- catboost.predict(model, test_pool)
  roc_result <- rocit(score = pred, class = as.factor(test_data[[target_col]]), negref = "1",method = "bin")
  pred_class <- ifelse(pred > 0.5, "1", "0")
  levels(pred_class)
  
  # 打印AUC值
  auc_value <- roc_result$AUC
  print(paste("AUC value:", auc_value))
  
  # 绘制ROC曲线
  if(i == 1) {
    plot(roc_result, col = i, add = TRUE)  # 第一个fold使用plot函数
  } else {
    lines(roc_result$FP, roc_result$TP, col = i)  # 后续的fold使用lines函数
  }
  
  #计算混淆矩阵
  pred_class <- factor(pred_class, levels = c("1", "0"))
  test_class <- factor(as.character(test_data[[target_col]]), levels = c("1", "0"))
  cm <- confusionMatrix(pred_class, test_class)
  
  # 从混淆矩阵中提取精确率和召回率
  precision <- cm$byClass['Pos Pred Value']
  recall <- cm$byClass['Sensitivity']
  
  # 计算F1 Score
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  # 保存这一折的F1 Score
  f1_scores[i] <- f1_score
  
  # 存储每个fold的评估指标
  evaluation_metrics[[i]] <- list(
    confusion_matrix = cm
  )
  
  # 保存这个fold的预测
  all_preds[[i]] <- pred
  
  #保存结果
  results[[i]] <- list(model = model)
}
# 输出每个fold的评估指标---------
print(evaluation_metrics)  
print(f1_scores)
