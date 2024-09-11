data4=read.csv('dataRF.csv')
#feseR
sum(is.na(data4))
str(data4)

# Prepare data
features <- data4[, -ncol(data4)]  # All feature columns
sum(is.na(features))

class <- data4[, ncol(data4)]  # Target variable column
sum(is.na(class)) 

# Data standardization
variances <- apply(features, 2, var)
features <- features[, variances > 0]
features <- scale(features, center = TRUE, scale = TRUE)
rownames(features) <- 1:nrow(features)

# Use combineFS function for feature selection
# Depending on whether data is standardized, set features = features/features_matrix
# Set univariate filter to information gain filtering ('gain'), and control the percentage of features returned with the n.percent parameter
# Set multivariate filter to matrix correlation filtering ('mcorr'), using the default maxcorr threshold of 0.75
# Use recursive feature elimination with random forest ('rfe.rf') as the wrapper method
results <- combineFS(features = features, class = class,
                     univariate = "gain", n.percent = 0.75, zero.gain.out = TRUE,
                     multivariate = "mcorr", maxcorr = 0.75,
                     wrapper = "rfe.rf", number.cv = 10,
                     group.sizes = seq(1,50,2), extfolds = 10,
                     partition = 2/3)

print(results)

# Without data standardization
# Calculate variance and filter columns with variance greater than 0
variances <- apply(features, 2, var)
features <- features[, variances > 0]

# Convert feature data to matrix
features_matrix <- as.matrix(features)
rownames(features_matrix) <- 1:nrow(features_matrix)


results <- combineFS(features = features_matrix, class = class,
                     univariate = "gain", n.percent = 0.75, zero.gain.out = TRUE,
                     multivariate = "mcorr", maxcorr = 0.75,
                     wrapper = "rfe.rf", number.cv = 10,
                     group.sizes = seq(1,50,2), extfolds = 10,
                     partition = 2/3)

print(results)

# Elastic Net
data4=read.csv('data307.csv')
y <- data4$KIQ400  # Ensure the target variable is appropriately coded (0 and 1)
x <- as.matrix(data4[, -which(colnames(data4) == "KIQ400")])  # Feature matrix

# Specifically use elastic net by setting alpha = 0.5
fit <- glmnet(x, y, family = "binomial", alpha = 0.5)  # Use elastic net

# Determine the best regularization parameter lambda through cross-validation
cv_fit <- cv.glmnet(x, y, family = "binomial", alpha = 0.5)
plot(cv_fit)
best_lambda <- cv_fit$lambda.min  # Select the lambda that minimizes cross-validation error

# Examine retained coefficients using optimal lambda
coef(cv_fit, s = "lambda.min")
