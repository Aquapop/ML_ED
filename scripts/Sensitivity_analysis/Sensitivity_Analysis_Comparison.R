# Sensitivity_Analysis_Comparison.R
# This script performs sensitivity analysis by comparing different data preprocessing and modeling approaches.
# It includes data preprocessing, imputation using missRanger, Logarithmic Transformation， SMOTE




# -----------------------------------
# Part 1: Sensitivity Analysis SMOTE RF NOIMP
# -----------------------------------

# Combine results from different models
data123 <- data.frame(
  AUC = c(results_metricsSMOTE_RF$t[,1], results_metrics1$t[,1], results_metrics_XG_RF$t[,1]),
  Sensitivity = c(results_metricsSMOTE_RF$t[,2], results_metrics1$t[,2], results_metrics_XG_RF$t[,2]),
  Specificity = c(results_metricsSMOTE_RF$t[,3], results_metrics1$t[,3], results_metrics_XG_RF$t[,3]),
  PPV = c(results_metricsSMOTE_RF$t[,4], results_metrics1$t[,4], results_metrics_XG_RF$t[,4]),
  NPV = c(results_metricsSMOTE_RF$t[,5], results_metrics1$t[,5], results_metrics_XG_RF$t[,5]),
  F1_Score_Pos = c(results_metricsSMOTE_RF$t[,6], results_metrics1$t[,6], results_metrics_XG_RF$t[,6]),
  F1_Score_Neg = c(results_metricsSMOTE_RF$t[,7], results_metrics1$t[,7], results_metrics_XG_RF$t[,7]),
  Accuracy = c(results_metricsSMOTE_RF$t[,8], results_metrics1$t[,8], results_metrics_XG_RF$t[,8]),
  KAPPA = c(results_metricsSMOTE_RF$t[,9], results_metrics1$t[,9], results_metrics_XG_RF$t[,9]),
  Model = factor(rep(c("SMOTE", "XGBoost", "RF"), each = 1000))
)

# Save the combined data
write.csv(data123, "dataSMOTE_RF_NOIMP.csv", row.names = FALSE)

# Select the metric to analyze
# Normality Test
shapiro_test_SMOTE <- shapiro.test(data123$"metrics"[data123$Model == "SMOTE"])
shapiro_test_RF <- shapiro.test(data123$"metrics"[data123$Model == "RF"]) 
shapiro_test_XG <- shapiro.test(data123$"metrics"[data123$Model == "XGBoost"]) 

# Homogeneity of Variance Test
levene_test <- car::leveneTest("metrics" ~ Model, data = data123)

# Check normality for each group
normality_passed <- all(
  shapiro_test_XG$p.value > 0.05,
  shapiro_test_RF$p.value > 0.05,
  shapiro_test_SMOTE$p.value > 0.05
)

# Check homogeneity of variance
variance_homogeneity_passed <- !is.na(levene_test$`Pr(>F)`[1]) && levene_test$`Pr(>F)`[1] > 0.05

# Perform appropriate statistical test based on assumptions
if (normality_passed && variance_homogeneity_passed) {
  # If data is normally distributed and variances are equal, perform Fisher's ANOVA
  anova_result <- aov("metrics" ~ Model, data = data123)
} else {
  # If data is normally distributed but variances are not equal, perform Welch's ANOVA
  oneway_test_result <- oneway.test("metrics" ~ Model, data = data123, var.equal = FALSE)
}

# If data is not normally distributed, perform Kruskal-Wallis Test
kruskal_test_result <- kruskal.test("metrics" ~ Model, data = data123)

# Print p-values from normality and variance tests
print(shapiro_test_XG$p.value)
print(shapiro_test_RF$p.value)
print(shapiro_test_SMOTE$p.value)
print(variance_homogeneity_passed)

# Output statistical test results (Fisher's ANOVA)
if (exists("anova_result")) {
  print(summary(anova_result))
  # Post-hoc analysis using Tukey's HSD
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

# Output statistical test results (Welch's ANOVA)
if (exists("oneway_test_result")) {
  print(oneway_test_result)
  # If Welch's ANOVA is significant, perform Games-Howell post-hoc test
  res <- GamesHowellTest(x = data123$"metrics", g = data123$Model)
  print(res)
}

# Output statistical test results (Kruskal-Wallis)
print(kruskal_test_result)
# If Kruskal-Wallis is significant, perform Dunn's Test with Bonferroni correction
dunn_result <- dunn.test(x = data123$"metrics", g = data123$Model, method = "bonferroni")
print(dunn_result)

# Observe mean ± standard deviation for each group
data123 %>%
  group_by(Model) %>%
  summarise(
    Mean_metrics = mean("metrics", na.rm = TRUE),
    SD_metrics = sd("metrics", na.rm = TRUE)
  ) %>%
  print()

# -----------------------------------
# Part 2: Sensitivity Analysis PMM RF NOIMP
# -----------------------------------

# Combine results from different models
data123_pmm <- data.frame(
  AUC = c(results_metrics_XG_RF$t[,1], results_metrics1$t[,1], results_metricsPMM$t[,1]),
  Sensitivity = c(results_metrics_XG_RF$t[,2], results_metrics1$t[,2], results_metricsPMM$t[,2]),
  Specificity = c(results_metrics_XG_RF$t[,3], results_metrics1$t[,3], results_metricsPMM$t[,3]),
  PPV = c(results_metrics_XG_RF$t[,4], results_metrics1$t[,4], results_metricsPMM$t[,4]),
  NPV = c(results_metrics_XG_RF$t[,5], results_metrics1$t[,5], results_metricsPMM$t[,5]),
  F1_Score_Pos = c(results_metrics_XG_RF$t[,6], results_metrics1$t[,6], results_metricsPMM$t[,6]),
  F1_Score_Neg = c(results_metrics_XG_RF$t[,7], results_metrics1$t[,7], results_metricsPMM$t[,7]),
  Accuracy = c(results_metrics_XG_RF$t[,8], results_metrics1$t[,8], results_metricsPMM$t[,8]),
  KAPPA = c(results_metrics_XG_RF$t[,9], results_metrics1$t[,9], results_metricsPMM$t[,9]),
  Model = factor(rep(c("RF", "XGBoost", "PMM"), each = 1000))
)

# Save the combined PMM data
write.csv(data123_pmm, "dataRF_PMM_NOIMP.csv", row.names = FALSE)

# Read the combined PMM data
data123_pmm <- read.csv("dataRF_PMM_NOIMP.csv")
data123_pmm$Model <- as.factor(data123_pmm$Model)

# Select the metric to analyze
# Normality Test
shapiro_test_RF_pmm <- shapiro.test(data123_pmm$"metrics"[data123_pmm$Model == "RF"])
shapiro_test_PMM <- shapiro.test(data123_pmm$"metrics"[data123_pmm$Model == "PMM"]) 
shapiro_test_XG_pmm <- shapiro.test(data123_pmm$"metrics"[data123_pmm$Model == "XGBoost"]) 

# Homogeneity of Variance Test
levene_test_pmm <- car::leveneTest("metrics" ~ Model, data = data123_pmm)

# Check normality for each group
normality_passed_pmm <- all(
  shapiro_test_XG_pmm$p.value > 0.05,
  shapiro_test_PMM$p.value > 0.05,
  shapiro_test_RF_pmm$p.value > 0.05
)

# Check homogeneity of variance
variance_homogeneity_passed_pmm <- !is.na(levene_test_pmm$`Pr(>F)`[1]) && levene_test_pmm$`Pr(>F)`[1] > 0.05

# Perform appropriate statistical test based on assumptions
if (normality_passed_pmm && variance_homogeneity_passed_pmm) {
  # If data is normally distributed and variances are equal, perform Fisher's ANOVA
  anova_result_pmm <- aov("metrics" ~ Model, data = data123_pmm)
} else {
  # If data is normally distributed but variances are not equal, perform Welch's ANOVA
  oneway_test_result_pmm <- oneway.test("metrics" ~ Model, data = data123_pmm, var.equal = FALSE)
}

# If data is not normally distributed, perform Kruskal-Wallis Test
kruskal_test_result_pmm <- kruskal.test("metrics" ~ Model, data = data123_pmm)

# Print p-values from normality and variance tests
print(shapiro_test_XG_pmm$p.value)
print(shapiro_test_PMM$p.value)
print(shapiro_test_RF_pmm$p.value)
print(variance_homogeneity_passed_pmm)

# Output statistical test results (Fisher's ANOVA)
if (exists("anova_result_pmm")) {
  print(summary(anova_result_pmm))
  # Post-hoc analysis using Tukey's HSD
  tukey_result_pmm <- TukeyHSD(anova_result_pmm)
  print(tukey_result_pmm)
}

# Output statistical test results (Welch's ANOVA)
if (exists("oneway_test_result_pmm")) {
  print(oneway_test_result_pmm)
  # If Welch's ANOVA is significant, perform Games-Howell post-hoc test
  res_pmm <- GamesHowellTest(x = data123_pmm$"metrics", g = data123_pmm$Model)
  print(res_pmm)
}

# Output statistical test results (Kruskal-Wallis)
print(kruskal_test_result_pmm)
# If Kruskal-Wallis is significant, perform Dunn's Test with Bonferroni correction
dunn_result_pmm <- dunn.test(x = data123_pmm$"metrics", g = data123_pmm$Model, method = "bonferroni")
print(dunn_result_pmm)

# Observe mean ± standard deviation for each group
data123_pmm %>%
  group_by(Model) %>%
  summarise(
    Mean_"metrics" = mean("metrics", na.rm = TRUE),
    SD_"metrics" = sd("metrics", na.rm = TRUE)
  ) %>%
  print()
