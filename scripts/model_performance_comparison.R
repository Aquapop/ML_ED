# model_performance_comparison.R
# This script compares the performance of CatBoost, XGBoost, and LightGBM models using bootstrap results.
# It performs statistical tests to determine significant differences in performance metrics.

# Load necessary libraries
library(caret)           
library(dplyr)          
library(boot)            
library(car)            
library(rstatix)
library(ufs)
library(dunn.test)        

# ------------------------ Data Aggregation ------------------------

# Combine bootstrap results from CatBoost, XGBoost, and LightGBM
data123 <- data.frame(
  AUC = c(results_metricsCAT_RF$t[,1], results_metricsRF$t[,1], results_metricsLight_RF$t[,1]),
  Sensitivity = c(results_metricsCAT_RF$t[,2], results_metricsRF$t[,2], results_metricsLight_RF$t[,2]),
  Specificity = c(results_metricsCAT_RF$t[,3], results_metricsRF$t[,3], results_metricsLight_RF$t[,3]),
  PPV = c(results_metricsCAT_RF$t[,4], results_metricsRF$t[,4], results_metricsLight_RF$t[,4]),
  NPV = c(results_metricsCAT_RF$t[,5], results_metricsRF$t[,5], results_metricsLight_RF$t[,5]),
  F1_Score_Pos = c(results_metricsCAT_RF$t[,6], results_metricsRF$t[,6], results_metricsLight_RF$t[,6]),
  F1_Score_Neg = c(results_metricsCAT_RF$t[,7], results_metricsRF$t[,7], results_metricsLight_RF$t[,7]),
  Accuracy = c(results_metricsCAT_RF$t[,8], results_metricsRF$t[,8], results_metricsLight_RF$t[,8]),
  Kappa = c(results_metricsCAT_RF$t[,9], results_metricsRF$t[,9], results_metricsLight_RF$t[,9]),
  Model = factor(rep(c("CatBoost", "XGBoost", "LightGBM"), each = 1000))
)

# Write the aggregated data to CSV
write.csv(data123, "outputs/model_performance_comparison.csv", row.names = FALSE)

# ------------------------ Statistical Testing ------------------------

# Perform Normality Tests
shapiro_test_CatBoost <- shapiro.test(data123$Kappa[data123$Model == "CatBoost"])
shapiro_test_LIGHT <- shapiro.test(data123$Kappa[data123$Model == "LightGBM"]) 
shapiro_test_XG <- shapiro.test(data123$Kappa[data123$Model == "XGBoost"]) 

# Perform Levene's Test for Homogeneity of Variance
levene_test <- car::leveneTest("metrics" ~ Model, data = data123)

# Check if all groups pass normality
normality_passed <- all(
  shapiro_test_XG$p.value > 0.05,
  shapiro_test_LIGHT$p.value > 0.05,
  shapiro_test_CatBoost$p.value > 0.05
)

# Check if variance homogeneity is passed
variance_homogeneity_passed <- !is.na(levene_test$`Pr(>F)`[1]) && levene_test$`Pr(>F)`[1] > 0.05

# Decide which statistical test to perform based on normality and variance
if (normality_passed && variance_homogeneity_passed) {
  # If data is normally distributed and variances are homogeneous, perform ANOVA
  anova_result <- aov("metrics" ~ Model, data = data123)
} else {
  # If data is normally distributed but variances are not homogeneous, perform Welch's ANOVA
  oneway_test_result <- oneway.test("metrics" ~ Model, data = data123, var.equal = FALSE)
}

# If data is not normally distributed, perform Kruskal-Wallis Test
if (!normality_passed) {
  kruskal_test_result <- kruskal.test("metrics" ~ Model, data = data123)
}

# Print p-values of normality tests and variance homogeneity
print(shapiro_test_XG$p.value)
print(shapiro_test_LIGHT$p.value)
print(shapiro_test_CatBoost$p.value)
print(variance_homogeneity_passed)

# Perform and print statistical test results
if (normality_passed && variance_homogeneity_passed) {
  # Perform ANOVA and Tukey's HSD post-hoc test
  print(summary(anova_result))
  
  # Post-hoc analysis with Tukey's HSD
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
} else if (normality_passed && !variance_homogeneity_passed) {
  # Perform Welch's ANOVA and Games-Howell post-hoc test
  print(oneway_test_result)
  
  # Perform Games-Howell post-hoc test if ANOVA is significant
  res <- gamesHowellTest(x = data123$"metrics", g = data123$Model)
  print(res)
} else {
  # Perform Kruskal-Wallis Test and Dunn's Test with Bonferroni correction
  print(kruskal_test_result)
  
  # Perform Dunn's Test for post-hoc analysis
  dunn_result <- dunn.test(x = data123$"metrics", g = data123$Model, method = "bonferroni")
  print(dunn_result)
}

# ------------------------ Summary Statistics ------------------------

# Calculate and print mean and standard deviation for each model
summary_stats <- data123 %>%
  group_by(Model) %>%
  summarise(
    Mean_Kappa = mean("metrics", na.rm = TRUE),
    SD_Kappa = sd("metrics", na.rm = TRUE)
  )

print(summary_stats)
