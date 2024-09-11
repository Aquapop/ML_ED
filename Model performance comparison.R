data123 <- read.csv("datax.csv")
data123$group <- as.factor(data123$group)

# Select metrics to analyze
# Normality test
shapiro_test_XG <- shapiro.test(data123$KAPPA[data123$group == "XG"])
shapiro_test_LIGHT <- shapiro.test(data123$KAPPA[data123$group == "LIGHT"])
shapiro_test_CatBoost <- shapiro.test(data123$KAPPA[data123$group == "CatBoost"])

# Test for homogeneity of variances
levene_test <- car::leveneTest(KAPPA ~ group, data = data123)

# Check the normality for each group
normality_passed <- all(shapiro_test_XG$p.value > 0.05, shapiro_test_LIGHT$p.value > 0.05, shapiro_test_CatBoost$p.value > 0.05)

# Check for homogeneity of variances
variance_homogeneity_passed <- !is.na(levene_test$`Pr(>F)`[1]) && levene_test$`Pr(>F)`[1] > 0.05

if (normality_passed && variance_homogeneity_passed) {
  # If data are normally distributed and variances are homogeneous, perform Fisher's ANOVA test
  anova_result <- aov(KAPPA  ~ group, data = data123)
} else {
  # If data are normally distributed but variances are not equal, perform Welch's ANOVA test
  oneway_test_result <- oneway.test(KAPPA ~ group, data = data123, var.equal = FALSE)
}

# If data do not meet normality and variances are not homogeneous, perform Kruskal-Wallis test
kruskal_test_result <- kruskal.test(KAPPA ~ group, data = data123)

# If Fisher's ANOVA test is not applicable, check normality and homogeneity of variances, then decide the subsequent test (choose between Welch's ANOVA test and Kruskal-Wallis test)
print(shapiro_test_XG$p.value)
print(shapiro_test_LIGHT$p.value)
print(shapiro_test_CatBoost$p.value)
print(variance_homogeneity_passed)

# Output the statistical test results (Fisher's ANOVA)
print(summary(anova_result))
# Post-hoc analysis
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Or Welch's ANOVA
print(oneway_test_result)
# If Welch's ANOVA test shows significance, perform Games-Howell post-hoc comparison
res <- gamesHowellTest(x = data123$Sensitivity, g = data123$group)
# View the results of the Games-Howell test
print(res)

# Or (Kruskal-Wallis)
print(kruskal_test_result)
# Dunn's post-hoc analysis (with Bonferroni correction)
dunn_result <- dunn.test(x = data123$Sensitivity, g = data123$group, method = "bonferroni")

# Observe the mean ± standard deviation for each group
data123 %>%
  group_by(group) %>%
  summarise(
    mean = mean(KAPPA, na.rm = TRUE),
    sd = sd(KAPPA, na.rm = TRUE)
  ) %>%
  print()
