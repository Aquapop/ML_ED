#ROC
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "1-Specificity(False Positive Rate)", ylab = "Sensitivity(True Positive Rate)")
abline(a = 0, b = 1, col = "gray", lty = 5, lwd = 2)  # 添加对角线
abline(h = seq(0, 1, by = 0.2), v = seq(0, 1, by = 0.2), lty = "dotted", col = "gray")

title(main = "ROC curve in train group")

# 使用 rocit 函数计算LightGBM训练集ROC数据
# 如果绘制测试集ROC数据采用all_preds_Light，以下模型同
roc_result1 <- rocit(score = train_preds_Light, class = as.factor(train_actuals_Light), negref = "0", method = "bin")
lines(roc_result1$FP, roc_result1$TP, col = "#82B29A",lwd = 2)


# 使用 rocit 函数计算XGBoost测试集ROC数据
roc_result2 <- rocit(score = train_preds_XG, class = as.factor(train_actuals_XG), negref = "0", method = "bin")
lines(roc_result2$FP, roc_result2$TP, col = "#3C405B",lwd = 2)

# 使用 rocit 函数计算CatBoost测试集ROC数据
roc_result3 <- rocit(score = train_preds_CAT, class = as.factor(train_actuals_CAT), negref = "0", method = "bin")
lines(roc_result3$FP, roc_result3$TP, col = "#DF7A5E",lwd = 2)

legend("bottomright",          # 图例位置
       legend = c("LightGBM(Area=0.930)","XGBoost(Area=0.904)", "CatBoost(Area=0.902)", "Chance line"),  # 图例文本
       col = c("#82B29A", "#3C405B", "#DF7A5E", "gray"),  # 线条颜色
       lty = c(1, 1, 1, 2),  # 线条类型
       lwd = c(2, 1))  # 线条宽度

#DCA
#针对3个模型创建3个概率和实际结果一一对应的数据集
data_XG <- data.frame(
  true_outcome = all_actuals_XG,
  xgb_prob = all_preds_XG
)
write.csv(data_XG,"data_XG.csv")

data_LIGHT <- data.frame(
  true_outcome = all_actuals_Light,
  xgb_prob = all_preds_Light
)
#write.csv(data_LIGHT,"data_LIGHT.csv")


data_CAT <- data.frame(
  true_outcome = all_actuals_CAT,
  xgb_prob = all_preds_CAT
)
write.csv(data_CAT,"data_CAT.csv")

#将3个表用EXCEL合并为data_DCA.csv

datadca = read.csv("data_DCA.csv")

# 计算XGBoost模型的决策曲线
dc_xgb <- decision_curve(true_outcome ~ xgb_prob, 
                         data = datadca,
                         study.design = 'cohort', 
                         policy = 'opt-in',
                         thresholds = seq(0, 1, by = 0.01))

# 计算LightGBM模型的决策曲线
dc_lgb <- decision_curve(true_outcome ~ light_prob, 
                         data = datadca,
                         study.design = 'cohort', 
                         policy = 'opt-in',
                         thresholds = seq(0, 1, by = 0.01))

# 计算CatBoost模型的决策曲线
dc_cat <- decision_curve(true_outcome ~ cat_prob, 
                         data = datadca, 
                         study.design = 'cohort', 
                         policy = 'opt-in',
                         thresholds = seq(0, 1, by = 0.01))

# 绘制决策曲线
str(dc_xgb)

plot_decision_curve(list(dc_lgb, dc_xgb, dc_cat),
                    curve.names = c("LightGBM", "XGBoost","CatBoost"), 
                    xlim = c(0, 1), # 可以设置x轴范围
                    legend.position = "topright",          # 图例位置
                    col = c("#82B29A", "#3C405B", "#DF7A5E"),  # 线条颜色
                    confidence.intervals = "none",
                    lty = c(1, 2, 3, 4, 1),  # 线条类型
                    lwd = c(3, 3, 3, 2, 1))  # 线条宽度
title(main = "DCA curve")

#校准曲线
#首先为不同的机器学习模型（XGBoost、LightGBM、CatBoost）创建了包含实际标签和预测概率的数据框：
predictions_xgb  <- data.frame(
  actual = all_actuals_XG,
 .pred_pass = all_preds_XG
)
#write.csv(data_XG,"data_XG.csv")

predictions_lgb  <- data.frame(
  actual = all_actuals_Light,
.pred_pass = all_preds_Light
)
#write.csv(data_LIGHT,"data_LIGHT.csv")

predictions_cat  <- data.frame(
  actual = all_actuals_CAT,
 .pred_pass = all_preds_CAT
)
#将catboost模型对数几率转换为预测概率
predictions_cat <- predictions_cat %>%
  mutate(.pred_pass = 1 / (1 + exp(-.pred_pass)))
#write.csv(data_CAT,"data_CAT.csv")

#合并预测结果
combined_data <- data.frame(
  actual = predictions_xgb$actual, # 假设所有数据框的actual列是相同的
  pred_xgb = predictions_xgb$.pred_pass,
  pred_lgb = predictions_lgb$.pred_pass,
  pred_cat = predictions_cat$.pred_pass
)

#计算
brier_score <- mean((predictions_lgb$actual - predictions_lgb$.pred_pass)^2)

# 为XGBoost模型评估性能和生成校准曲线
score_xgb <- Score(list(fit = predictions_xgb$.pred_pass),
                   formula = actual ~ 1,
                   data = predictions_xgb,
                   metrics = c("auc", "brier"),
                   plots = "calibration",  # 确保这里是"calibration"
                   B = 200)
plot_xgb <- plotCalibration(score_xgb, times = 1, method = "nne")


# 为LightGBM模型评估性能和生成校准曲线
score_lgb <- Score(list(fit = predictions_lgb$.pred_pass),
                   formula = actual ~ 1,
                   data = predictions_lgb,
                   metrics = c("auc", "brier"),
                   plots = "calibration",  # 确保这里是"calibration"
                   B = 200)
# 接下来，使用plotCalibration函数绘制校准图
plot_lgb <- plotCalibration(score_lgb, method = "nne")


# 为CatBoost模型评估性能和生成校准曲线
score_cat <- Score(list(fit = predictions_cat$.pred_pass),
                   formula = actual ~ 1,
                   data = predictions_cat,
                   metrics = c("auc", "brier"),
                   plots = "calibration",  # 确保这里是"calibration"
                   B = 200)
plot_cat <- plotCalibration(score_cat, method = "nne")

plotdata <- plotCalibration(score_cat,plot = F,method = "nne")

fit_all <- Score(list("XGBoost"=combined_data$pred_xgb,
                      "LightGBM"=combined_data$pred_lgb,
                      "CatBoost"=combined_data$pred_cat),
formula = actual ~ 1,
data = combined_data,
metrics = c("auc","brier"),
summary = c("risks","IPA","riskQuantile","ibs"),
plots = "calibration",
null.model = T,
conf.int = T,
B = 500,
M = 50
)

data_all <- plotCalibration(fit_all,plot = F, method = "nne")

#合并曲线
plot_df <- bind_rows(data_all$plotFrames) %>% 
  mutate(fits = rep(c("XGBoost", "LightGBM", "CatBoost"), times=c(99, 101, 101)))

ggplot(plot_df, aes(Pred,Obs))+
  geom_line(aes(group=fits,color=fits),linewidth=1.2)+
  scale_color_manual(values = c("#DF7A5E","#82B29A","#3C405B"),
                     name=NULL,
                     labels = c("LightGBM (Brier=0.111)", "XGBoost (Brier=0.135)", "CatBoost (Brier=0.131)")
  )+
  scale_x_continuous(limits = c(0,1),name = "Predicted probabilities")+
  scale_y_continuous(limits = c(0,1),name = "Fraction of positives")+
  geom_abline(slope = 1,intercept = 0,lty=2)+
  theme_bw()+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))

