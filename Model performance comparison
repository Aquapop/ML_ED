data123 <- read.csv("datax.csv")
data123$group<- as.factor(data123$group)


#选择要分析的指标
# 正态性检验
shapiro_test_XG <- shapiro.test(data123$KAPPA[data123$group == "XG"])
shapiro_test_LIGHT <- shapiro.test(data123$KAPPA[data123$group == "LIGHT"]) 
shapiro_test_CatBoost <- shapiro.test(data123$KAPPA[data123$group == "CatBoost"]) 




# 方差齐性检验
levene_test <- car::leveneTest(KAPPA ~ group, data = data123)

# 检验每组的正态性
normality_passed <- all(shapiro_test_XG$p.value > 0.05, shapiro_test_LIGHT$p.value > 0.05, shapiro_test_CatBoost$p.value > 0.05)

# 检验方差齐性
variance_homogeneity_passed <- !is.na(levene_test$`Pr(>F)`[1]) && levene_test$`Pr(>F)`[1] > 0.05

if (normality_passed && variance_homogeneity_passed) {
  # 如果数据符合正态分布且方差齐性，进行Fisher's ANOVA检验
  anova_result <- aov(KAPPA  ~ group, data = data123)
} else {
  #如果数据符合正态分布但方差不齐，进行Welch's ANOVA检验
  oneway_test_result <- oneway.test(KAPPA ~ group, data = data123, var.equal = FALSE)
}

# 如果数据不符合正态分布且方差不齐，进行Kruskal-Wallis检验
kruskal_test_result <- kruskal.test(KAPPA ~ group, data = data123)

#如果不能进行Fisher's ANOVA检验，那就检查正态性和方差齐性，再决定后续用什么检验（Welch's ANOVA检验，Kruskal-Wallis检验2选1）
print(shapiro_test_XG$p.value)
print(shapiro_test_LIGHT$p.value)
print(shapiro_test_CatBoost$p.value)
print(variance_homogeneity_passed)


# 输出统计检验结果（fisher's anova）
print(summary(anova_result))
#事后分析
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

#或Welch‘s anova
print(oneway_test_result)
# Welch's ANOVA检验具有显著性，进行Games-Howell事后比较
res <- gamesHowellTest(x = data123$Sensitivity, g = data123$group)
# 查看Games-Howell测试的结果
print(res)

# 或(Kruskal-Wallis)
print(kruskal_test_result)
#Dunn事后分析（Bonferroni校正）
dunn_result <- dunn.test(x = data123$Sensitivity, g = data123$group, method = "bonferroni")

#观察每组的均数±标准差
data123 %>%
  group_by(group) %>%
  summarise(
    mean = mean(KAPPA, na.rm = TRUE),
    sd = sd(KAPPA, na.rm = TRUE)
  ) %>%
  print()
