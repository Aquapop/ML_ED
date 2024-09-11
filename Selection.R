data4=read.csv('dataRF.csv')
#feseR
sum(is.na(data4))
str(data4)

# 准备数据
features <- data4[, -ncol(data4)]  # 所有特征列
sum(is.na(features))

class <- data4[, ncol(data4)]  # 目标变量列
sum(is.na(class)) 

# 数据标准化
variances <- apply(features, 2, var)
features <- features[, variances > 0]
features <- scale(features, center = TRUE, scale = TRUE)
rownames(features) <- 1:nrow(features)

results <- combineFS(features = features, class = class,
                     univariate = "gain", n.percent = 0.75, zero.gain.out = TRUE,
                     multivariate = "mcorr", maxcorr = 0.75,
                     wrapper = "rfe.rf", number.cv = 10,
                     group.sizes = seq(1,50,2), extfolds = 10,
                     partition = 2/3)

print(results)

# 不进行数据标准化
# 计算方差并筛选出方差大于0的列
variances <- apply(features, 2, var)
features <- features[, variances > 0]

# 将特征数据转换为矩阵
features_matrix <- as.matrix(features)
rownames(features_matrix) <- 1:nrow(features_matrix)

# 使用combineFS函数进行特征选择
#根据是否进行数据标准化设置features = features/features_matrix
# 设置单变量过滤为信息增益过滤 ('gain')，并设置n.percent参数控制返回的特征百分比
# 设置多变量过滤为矩阵相关性过滤 ('mcorr')，并使用默认的maxcorr阈值0.75
# 使用递归特征消除包装随机森林 ('rfe.rf') 作为包装方法
results <- combineFS(features = features_matrix, class = class,
                     univariate = "gain", n.percent = 0.75, zero.gain.out = TRUE,
                     multivariate = "mcorr", maxcorr = 0.75,
                     wrapper = "rfe.rf", number.cv = 10,
                     group.sizes = seq(1,50,2), extfolds = 10,
                     partition = 2/3)

print(results)

#Elastic Net
data4=read.csv('data307.csv')
y <- data4$KIQ400  # 确保目标变量是适当编码（0和1）
x <- as.matrix(data4[, -which(colnames(data4) == "KIQ400")])  # 特征矩阵

#通过设置alpha = 0.5来特定使用弹性网络
fit <- glmnet(x, y, family = "binomial", alpha = 0.5)  # 使用弹性网络

#通过交叉验证确定最佳的正则化参数lambda
cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 0.5)
plot(cv_fit)
best_lambda <- cv_fit$lambda.min  # 选择使交叉验证误差最小的lambda

#使用最优lambda查看保留下来的系数
coef(cv_fit, s = "lambda.min")
