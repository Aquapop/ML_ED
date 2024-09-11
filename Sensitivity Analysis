data123 <- read.csv("LIght.csv")
data123$group<- as.factor(data123$group)

# 正态性检验
shapiro_test_rf <- shapiro.test(data123$AUC[data123$group == "RF"])
shapiro_test_pmm <- shapiro.test(data123$AUC[data123$group == "PMM"])



# 方差齐性检验
levene_test <- car::leveneTest(AUC ~ group, data = data123)

if (!is.na(levene_test$`Pr(>F)`[1])) {
  if (shapiro_test_rf$p.value > 0.05 && shapiro_test_pmm$p.value > 0.05 && levene_test$`Pr(>F)`[1] > 0.05) {
    # 如果数据符合正态分布且方差齐性，进行t检验
    t_test_result <- t.test(AUC ~ group, data = data123)
  } else {
    # 如果数据不符合正态分布，进行Wilcoxon秩和检验
    wilcox_test_result <- wilcox.test(AUC ~ group, data = data123)
  }
} else {
  print("One or more p-values are NA. Cannot proceed with the test.")
}


# 输出统计检验结果
print(t_test_result)
# 或
print(wilcox_test_result)

data123 %>%
  group_by(group) %>%
  summarise(
    mean = mean(AUC, na.rm = TRUE),
    sd = sd(AUC, na.rm = TRUE)
  ) %>%
  print()
