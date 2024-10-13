# A Study on Predicting Erectile Dysfunction (ED) Using Machine Learning Models

## Overview

This project aims to utilize machine learning algorithms such as XGBoost, LightGBM, and CatBoost to build models that predict erectile dysfunction (ED). The data is sourced from the National Health and Nutrition Examination Survey (NHANES). This README provides detailed instructions on how to reproduce this study, including data preprocessing, feature selection, model training, evaluation, and sensitivity analysis.

## Contents and Code Running Order

1. **Environment Dependencies**  
   - [install_packages.R](.scripts/install_packages.R)

2. **Data Preparation**  
   - [data_collection.R](./data_collection.R)

3. **Data Preprocessing**
   - [Outlier Treatment](./outlier_analysis.R)
   - [Basic Characteristics of Respondents](./univariate_analysis_and_logarithmic_transformation.R)
   - [Logarithmic Transformation](./univariate_analysis_and_logarithmic_transformation.R)
   - [Dataset Splitting](./data_split_randomforest_imputation.R)
   - [Handling Missing Values](./data_split_randomforest_imputation.R)

4. **Feature Selection**
   - [feature_selection.R](./feature_selection.R)

5. **Model Training and Validation**
   - [Hyperparameter Tuning](./hyperparameter_tuning.R)
   - [XGBoost](./xgboost_model_training.R)
   - [LightGBM](./lightgbm_model_training.R)
   - [CatBoost](./catboost_model_training.R)

6. **Model Performance Comparison on Train Set**
   - [bootstrap_in_test_set.R](./bootstrap_in_test_set.R)

7. **Results Analysis**
   - [XGBoost SHAP Importance](./XGBoost_SHAP_importance.R)

8. **Sensitivity Analysis**
   - [Sensitivity Analysis SMOTE](./Sensitivity_Analysis_SMOTE.R)
   - [Sensitivity Analysis PMM](./Sensitivity_Analysis_PMM.R)
   - [Sensitivity Analysis NOIMP](./Sensitivity_Analysis_NOIMP.R)
   - [Sensitivity Analysis LT](./Sensitivity_Analysis_LT.R)
   - [Sensitivity Analysis Comparison](./Sensitivity_Analysis_Comparison.R)






## Prerequisites

- **R:** Ensure that R (version 4.0 or higher) is installed on your system. You can download R from [CRAN](https://cran.r-project.org/).
- **RStudio (Optional):** While not mandatory, RStudio provides a user-friendly interface for working with R.

## Installation Steps

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/yourusername/your-repo-name.git
