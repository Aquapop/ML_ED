data123 <- read.csv("LIght.csv")
data123$group <- as.factor(data123$group)

# Normality test
shapiro_test_rf <- shapiro.test(data123$AUC[data123$group == "RF"])
shapiro_test_pmm <- shapiro.test(data123$AUC[data123$group == "PMM"])

# Homogeneity of variances test
levene_test <- car::leveneTest(AUC ~ group, data = data123)

if (!is.na(levene_test$`Pr(>F)`[1])) {
  if (shapiro_test_rf$p.value > 0.05 && shapiro_test_pmm$p.value > 0.05 && levene_test$`Pr(>F)`[1] > 0.05) {
    # If data are normally distributed and variances are equal, perform a t-test
    t_test_result <- t.test(AUC ~ group, data = data123)
  } else {
    # If data do not follow a normal distribution, perform a Wilcoxon rank-sum test
    wilcox_test_result <- wilcox.test(AUC ~ group, data = data123)
  }
} else {
  print("One or more p-values are NA. Cannot proceed with the test.")
}

# Output statistical test results
print(t_test_result)
# or
print(wilcox_test_result)

data123 %>%
  group_by(group) %>%
  summarise(
    mean = mean(AUC, na.rm = TRUE),
    sd = sd(AUC, na.rm = TRUE)
  ) %>%
  print()
