# A Study on Predicting Erectile Dysfunction (ED) Using Machine Learning Models

## Overview

This project aims to utilize machine learning algorithms such as XGBoost, LightGBM, and CatBoost to build models that predict erectile dysfunction (ED). The data is sourced from the National Health and Nutrition Examination Survey (NHANES). This README provides detailed instructions on how to reproduce this study, including data preprocessing, feature selection, model training, evaluation, and sensitivity analysis.

## Contents and Code Running Order

1. **Environment Dependencies**  
   - [install_packages.R](scripts/install_packages.R)

2. **Data Preparation**  
   - [data_collection.R](scripts/data_collection.R)

3. **Data Preprocessing**
   - [Outlier Treatment](scripts/outlier_analysis.R)
   - [Basic Characteristics of Respondents](scripts/univariate_analysis_and_logarithmic_transformation.R)
   - [Logarithmic Transformation](scripts/univariate_analysis_and_logarithmic_transformation.R)
   - [Dataset Splitting](scripts/data_split_randomforest_imputation.R)
   - [Handling Missing Values](scripts/data_split_randomforest_imputation.R)

4. **Feature Selection**
   - [feature_selection.R](scripts/feature_selection.R)

5. **Model Training and Validation**
   - [Hyperparameter Tuning](scripts/hyperparameter_tuning.R)
   - [XGBoost](scripts/xgboost_model_training.R)
   - [LightGBM](scripts/lightgbm_model_training.R)
   - [CatBoost](scripts/CatBoost_model_training.R)

6. **Model Performance Comparison on Train Set**
   - [bootstrap_in_test_set.R](scripts/bootstrap_in_test_set.R)

7. **Results Analysis**
   - [XGBoost SHAP Importance](scripts/XGBoost_SHAP_importance.R)

8. **Sensitivity Analysis**
   - [Sensitivity Analysis SMOTE](scripts/Sensitivity_analysis/Sensitivity_Analysis_SMOTE.R)
   - [Sensitivity Analysis PMM](scripts/Sensitivity_analysis/Sensitivity_Analysis_PMM.R)
   - [Sensitivity Analysis NOIMP](scripts/Sensitivity_analysis/Sensitivity_Analysis_NOIMP.R)
   - [Sensitivity Analysis LT](scripts/Sensitivity_analysis/Sensitivity_Analysis_LT.R)
   - [Sensitivity Analysis Comparison](scripts/Sensitivity_analysis/Sensitivity_Analysis_Comparison.R)

8. **Plots**
   - [plots.R](scripts/plot.R)






## Prerequisites

- **R:** Ensure that R (version 4.4 or higher) is installed on your system. You can download R from [CRAN](https://cran.r-project.org/).
- **RStudio (Optional):** While not mandatory, RStudio provides a user-friendly interface for working with R.

## Installation Steps

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/yourusername/your-repo-name.git
