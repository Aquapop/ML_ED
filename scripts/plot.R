
#After determining which fold is the best model, retrain that fold model to get train_preds, validation_preds, and make predictions on the test set to get test_preds, see xgboost_model_training.R; lightgbm_model_ training.R; CatBoost_model_training.

target_col <- "ED" 
test_data_final = read.csv("dataRF_test_flitered.csv")
actuals <-  c(test_data_final[[target_col]])

# ------------------------ train Set auc plot ------------------------
# AUC ROC Curve
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1-Specificity (False Positive Rate)",
     ylab = "Sensitivity (True Positive Rate)")
abline(a = 0, b = 1, col = "gray", lty = 5, lwd = 2)  # Add diagonal line
abline(h = seq(0, 1, by = 0.2), v = seq(0, 1, by = 0.2), lty = "dotted", col = "gray")

title(main = "ROC Curve in Training Groups")

# Calculate ROC data for LightGBM using rocit
roc_result1 <- rocit(score = train_preds_Light, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result1$FP, roc_result1$TP, col = "#82B29A", lwd = 2)

# Calculate ROC data for XGBoost using rocit
roc_result2 <- rocit(score = train_preds_XG, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result2$FP, roc_result2$TP, col = "#3C405B", lwd = 2)

# Calculate ROC data for CatBoost using rocit
roc_result3 <- rocit(score = train_preds_CAT, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result3$FP, roc_result3$TP, col = "#DF7A5E", lwd = 2)

# Add legend to the ROC plot
legend("bottomright",
       legend = c("LightGBM (AUC = )", "XGBoost (AUC = )", 
                  "CatBoost (AUC = )", "Chance Line"),  # Legend text
       col = c("#82B29A", "#3C405B", "#DF7A5E", "gray"),  # Line colors
       lty = c(1, 1, 1, 2),  # Line types
       lwd = c(2, 2, 2, 2))  # Line widths

# ------------------------ validation Set auc plot ------------------------
# AUC ROC Curve
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1-Specificity (False Positive Rate)",
     ylab = "Sensitivity (True Positive Rate)")
abline(a = 0, b = 1, col = "gray", lty = 5, lwd = 2)  # Add diagonal line
abline(h = seq(0, 1, by = 0.2), v = seq(0, 1, by = 0.2), lty = "dotted", col = "gray")

title(main = "ROC Curve in validation Groups")
# Calculate ROC data for LightGBM using rocit
roc_result1 <- rocit(score = validation_preds_Light, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result1$FP, roc_result1$TP, col = "#82B29A", lwd = 2)

# Calculate ROC data for XGBoost using rocit
roc_result2 <- rocit(score = validation_preds_XG, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result2$FP, roc_result2$TP, col = "#3C405B", lwd = 2)

# Calculate ROC data for CatBoost using rocit
roc_result3 <- rocit(score = validation_preds_CAT, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result3$FP, roc_result3$TP, col = "#DF7A5E", lwd = 2)

# Add legend to the ROC plot
legend("bottomright",
       legend = c("LightGBM (AUC = )", "XGBoost (AUC = )", 
                  "CatBoost (AUC = )", "Chance Line"),  # Legend text
       col = c("#82B29A", "#3C405B", "#DF7A5E", "gray"),  # Line colors
       lty = c(1, 1, 1, 2),  # Line types
       lwd = c(2, 2, 2, 2))  # Line widths

# ------------------------ test Set auc plot ------------------------
# AUC ROC Curve
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "1-Specificity (False Positive Rate)",
     ylab = "Sensitivity (True Positive Rate)")
abline(a = 0, b = 1, col = "gray", lty = 5, lwd = 2)  # Add diagonal line
abline(h = seq(0, 1, by = 0.2), v = seq(0, 1, by = 0.2), lty = "dotted", col = "gray")

title(main = "ROC Curve in test Groups")
# Calculate ROC data for LightGBM using rocit
roc_result1 <- rocit(score = test_preds_Light, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result1$FP, roc_result1$TP, col = "#82B29A", lwd = 2)

# Calculate ROC data for XGBoost using rocit
roc_result2 <- rocit(score = test_preds_XG, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result2$FP, roc_result2$TP, col = "#3C405B", lwd = 2)

# Calculate ROC data for CatBoost using rocit
roc_result3 <- rocit(score = test_preds_CAT, 
                     class = as.factor(actuals), 
                     negref = "0", method = "bin")
lines(roc_result3$FP, roc_result3$TP, col = "#DF7A5E", lwd = 2)

# Add legend to the ROC plot
legend("bottomright",
       legend = c("LightGBM (AUC = )", "XGBoost (AUC = )", 
                  "CatBoost (AUC = )", "Chance Line"),  # Legend text
       col = c("#82B29A", "#3C405B", "#DF7A5E", "gray"),  # Line colors
       lty = c(1, 1, 1, 2),  # Line types
       lwd = c(2, 2, 2, 2))  # Line widths


# ----------------------------Calibration Curves--------------------------

# Create data.frames for predictions
predictions_xgb <- data.frame(
  actual = actual,
  pred_pass = test_preds_XG
)
write.csv(predictions_xgb, "data_XG.csv")

predictions_lgb <- data.frame(
  actual = actual,
  pred_pass = test_preds_Light
)
write.csv(predictions_lgb, "data_LIGHT.csv")

predictions_cat <- data.frame(
  actual = actual,
  pred_pass = test_preds_CAT
) %>%
  mutate(pred_pass = 1 / (1 + exp(-pred_pass)))  # Apply logistic transformation
write.csv(predictions_cat, "data_CAT.csv")

# Combine all predictions into one data.frame
combined_data <- data.frame(
  actual = predictions_xgb$actual,  # Assuming all 'actual' columns are the same
  pred_xgb = predictions_xgb$pred_pass,
  pred_lgb = predictions_lgb$pred_pass,
  pred_cat = predictions_cat$pred_pass
)
write.csv(combined_data, "combined_data.csv")
combined_data <- read.csv("combined_data.csv")

# Calculate Brier scores for each model
brier_score_light <- mean((predictions_lgb$actual - predictions_lgb$pred_pass)^2)
brier_score_XG <- mean((predictions_xgb$actual - predictions_xgb$pred_pass)^2)
brier_score_CAT <- mean((predictions_cat$actual - predictions_cat$pred_pass)^2)

# Logistic Regression Models for Calibration

# XGBoost
dd_XG <- datadist(predictions_xgb)
options(datadist = "dd_XG")
fit.xgb <- lrm(actual ~ pred_pass, data = predictions_xgb, x = TRUE, y = TRUE)
cal.xgb <- calibrate(fit.xgb, method = "boot", B = 1000)  # Calibration model

# LightGBM
dd_Light <- datadist(predictions_lgb)
options(datadist = "dd_Light")
fit.lgb <- lrm(actual ~ pred_pass, data = predictions_lgb, x = TRUE, y = TRUE)
cal.lgb <- calibrate(fit.lgb, method = "boot", B = 1000)  # Calibration model

# CatBoost
dd_cat <- datadist(predictions_cat)
options(datadist = "dd_cat")
fit.cat <- lrm(actual ~ pred_pass, data = predictions_cat, x = TRUE, y = TRUE)
cal.cat <- calibrate(fit.cat, method = "boot", B = 1000)  # Calibration model

# Combine Calibration Data
cal_data <- data.frame(
  Pred = c(cal.xgb[, "predy"], cal.lgb[, "predy"], cal.cat[, "predy"]),
  Obs_corrected = c(cal.xgb[, "calibrated.corrected"], 
                    cal.lgb[, "calibrated.corrected"], 
                    cal.cat[, "calibrated.corrected"]),
  Model = rep(c("XGBoost", "LightGBM", "CatBoost"), each = length(cal.xgb[, "predy"]))
)

# Plot Calibration Curves using ggplot2
ggplot(cal_data, aes(x = Pred, y = Obs_corrected, color = Model)) +
  geom_line(aes(group = Model, color = Model), linewidth = 1.2) +
  scale_color_manual(values = c("#3C405B", "#82B29A", "#DF7A5E"), name = NULL,
                     labels = c("XGBoost (Brier = 0.144)", 
                                "LightGBM (Brier = 0.139)", 
                                "CatBoost (Brier = 0.176)")) +
  scale_x_continuous(limits = c(0, 1), name = "Predicted Probabilities") +
  scale_y_continuous(limits = c(0, 1), name = "Observed Probabilities") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme_bw() +
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))

# Decision Curve Analysis (DCA)

# Calculate Decision Curves for each model
dc_xgb <- decision_curve(actual ~ pred_pass, 
                         data = predictions_xgb,
                         study.design = 'cohort', 
                         policy = 'opt-in',
                         thresholds = seq(0, 1, by = 0.01))

dc_lgb <- decision_curve(actual ~ pred_pass, 
                         data = predictions_lgb,
                         study.design = 'cohort', 
                         policy = 'opt-in',
                         thresholds = seq(0, 1, by = 0.01))

dc_cat <- decision_curve(actual ~ pred_pass, 
                         data = predictions_cat, 
                         study.design = 'cohort', 
                         policy = 'opt-in',
                         thresholds = seq(0, 1, by = 0.01))

# Plot Decision Curves
plot_decision_curve(list(dc_lgb, dc_xgb, dc_cat),
                   curve.names = c("LightGBM", "XGBoost", "CatBoost"), 
                   xlim = c(0, 1),  # Set x-axis range
                   legend.position = "topright",          # Legend position
                   col = c("#82B29A", "#3C405B", "#DF7A5E"),  # Line colors
                   confidence.intervals = "none",
                   lty = c(1, 2, 3),  # Line types
                   lwd = c(3, 3, 3))  # Line widths
title(main = "Decision Curve Analysis (DCA)")
