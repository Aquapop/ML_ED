# XGBoost_SHAP_importance.R
# This script analyzes feature importance and SHAP values for an XGBoost model.
# It generates feature importance plots and SHAP value visualizations to interpret model predictions.


# ------------------------ Feature Importance Analysis ------------------------

# Load the best XGBoost model
best_model_XG_RF <- readRDS("outputs/xg_model_fold_rf3.rds")

# Load training and testing data
train_data_final <- read.csv("outputs/dataRF_train_flitered.csv")
test_data_final <- read.csv("outputs/dataRF_test_flitered.csv")

# Define the target column
target_col <- "ED"

# Define feature columns by excluding the target variable
features <- setdiff(names(train_data_final), target_col)

# Calculate feature importance using Gain
importance <- xgb.importance(model = best_model_XG_RF)
print(importance)

# Order importance by Gain in descending order
importance <- importance[order(-importance$Gain), ]

# Plot feature importance using ggplot2
ggplot(importance, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#82B29A") +
  coord_flip() +  # Flip coordinates for better readability of feature names
  labs(x = "Feature", y = "Importance (Gain)", title = "Feature Importance") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    panel.background = element_blank(),       # Set transparent background
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border with no fill
    plot.background = element_rect(fill = "transparent", colour = NA)     # Set plot background to transparent
  )

# ------------------------ SHAP Value Analysis ------------------------

# Prepare the test data for SHAP value computation
positive_test_data <- test_data_final

# Create a DMatrix for the test data (excluding the target variable)
positive_dtest <- xgb.DMatrix(
  data = as.matrix(positive_test_data[, -which(names(positive_test_data) == target_col)]),
  label = positive_test_data[[target_col]]
)

# Compute SHAP values
# Note: Ensure that the 'shapviz' package is properly installed and configured
sv <- shapviz(
  object = best_model_XG_RF,
  X_pred = as.matrix(positive_test_data[, -which(names(positive_test_data) == target_col)]),
  X = as.matrix(positive_test_data[, -which(names(positive_test_data) == target_col)])
)

# Generate SHAP summary plot
sv_plot <- sv_importance(sv, kind = "beeswarm", max_display = 25)

# Display the SHAP summary plot
print(sv_plot)

# ------------------------ Data Transformation for SHAP Plots ------------------------

# Define categorical feature columns
factorCols <- c(
  "DMDEDUC",
  "DIQ010",
  "PAD590",
  "CDQ010",
  "PAD320",
  "KIQ081"
)

# Get all variable names
all_vars <- names(test_data_final)

# Exclude 'AGE' and other specific columns if necessary
vars <- setdiff(all_vars, c(factorCols, "RIDAGEEX", "ED"))

# Convert specified columns to factors
sv$X <- sv$X %>%
  mutate(across(all_of(factorCols), factor))

# Apply inverse transformations to continuous variables
sv$X <- sv$X %>%
  mutate(across(all_of(vars), ~ exp(.) - 1))

# Specifically transform 'RIDAGEEX'
sv$X <- sv$X %>%
  mutate(RIDAGEEX = (exp(RIDAGEEX) - 1) / 12)

# ------------------------ SHAP Dependence Plots ------------------------

# Define variables for SHAP dependence plots (excluding the target variable)
vars1 <- setdiff(all_vars, "ED")

# Loop through each variable to generate and save SHAP dependence plots
for (var in vars1) {
  # Generate SHAP dependence plot
  plot <- sv_dependence(sv, c(var), color_var = NULL, jitter_width = 0.05)
  
  # Display the plot
  print(plot)
  
  # Optional: Save the plot as a PNG file
  ggsave(
    filename = paste0("outputs/shap_dependence_", var, ".png"),
    plot = plot,
    width = 8,
    height = 6
  )
}
