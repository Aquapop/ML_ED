# A Study on Predicting Erectile Dysfunction (ED) Using Machine Learning Models

## Overview

This project aims to utilize machine learning algorithms such as XGBoost, LightGBM, and CatBoost to build models that predict erectile dysfunction (ED). The data is sourced from the National Health and Nutrition Examination Survey (NHANES). This README provides detailed instructions on how to reproduce this study, including data preprocessing, feature selection, model training, evaluation, and sensitivity analysis.

## Contents and Code Running Order

1. **Environment Dependencies**  
   - [install_packages.R](scripts/install_packages.R)

2. **Data Preparation**  
   - [data_collection.R](scripts/data_collection.R)
   - outputs：[data01_04.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data01_04.csv);[test5.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/test5.csv)

3. **Data Preprocessing**
   - [Outlier Treatment](scripts/outlier_analysis.R) output:[data104.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data104.csv)
     
   - [Basic Characteristics of Respondents](scripts/univariate_analysis_and_logarithmic_transformation.R) outputs:[data105.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data105.csv);[chi_square_results.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/chi_square_results.csv);[t_test_results.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/t_test_results.csv)
     
   - [Logarithmic Transformation](scripts/univariate_analysis_and_logarithmic_transformation.R) output:[data107.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data107.csv)
     
   - [Dataset Splitting](scripts/data_split_randomforest_imputation.R) outputs:[data107_train.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data107_train.csv);[data107_test.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data107_test.csv)
     
   - [Handling Missing Values](scripts/data_split_randomforest_imputation.R) outputs:[dataRF_test.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataRF_test.csv);[dataRF_train.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataRF_train.csv)

4. **Feature Selection**
   - [feature_selection.R](scripts/feature_selection.R) outputs:[dataRF_train_filtered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataRF_train_flitered.csv);[dataRF_test_filtered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataRF_test_flitered.csv)
     
5. **Model Training and Validation**
   - [Hyperparameter Tuning](scripts/hyperparameter_tuning.R)
   - [XGBoost](scripts/xgboost_model_training.R)
   - [LightGBM](scripts/lightgbm_model_training.R)
   - [CatBoost](scripts/CatBoost_model_training.R)

6. **Model Performance Comparison on Train Set**
   - [bootstrap_in_test_set.R](scripts/bootstrap_in_test_set.R)
   - [model_performance_comparison.R](scripts/model_performance_comparison.R) output:[model_performance_comparison.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/model_performance_comparison.csv)

7. **Results Analysis**
   - [XGBoost SHAP Importance](scripts/XGBoost_SHAP_importance.R) output:[Figure 5.png](https://github.com/Aquapop/ML_ED/blob/main/Data/output/Figure%205.png); [Figure 6.png](https://github.com/Aquapop/ML_ED/blob/main/Data/output/Figure%206.png)

8. **Sensitivity Analysis**
   - [Sensitivity Analysis SMOTE](scripts/Sensitivity_analysis/Sensitivity_Analysis_SMOTE.R)
   - [Sensitivity Analysis PMM](scripts/Sensitivity_analysis/Sensitivity_Analysis_PMM.R) output:[dataPMM_test_flitered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataPMM_test_flitered.csv); [dataPMM_train_flitered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataPMM_train_flitered.csv)
   - [Sensitivity Analysis NOIMP](scripts/Sensitivity_analysis/Sensitivity_Analysis_NOIMP.R) output:[data107_test_filtered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data107_test_flitered.csv);[data107_train_filtered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data107_train_flitered.csv)
   - [Sensitivity Analysis LT](scripts/Sensitivity_analysis/Sensitivity_Analysis_LT.R) output:[data105RF_train_filtered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data105RF_train_flitered.csv);[data105RF_test_filtered.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/data105RF_test_flitered.csv)
   - [Sensitivity Analysis RF15](scripts/Sensitivity_analysis/Sensitivity_Analysis_RF15.R) output:
   - [Sensitivity Analysis RF15](scripts/Sensitivity_analysis/Sensitivity_Analysis_RF20.R) 
   - [Sensitivity Analysis Comparison](scripts/Sensitivity_analysis/Sensitivity_Analysis_Comparison.R) output:[dataRF_PMM_NOIMP.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataRF_PMM_NOIMP.csv);[dataSMOTE_RF_NOIMP.csv](https://github.com/Aquapop/ML_ED/blob/main/Data/output/dataSMOTE_RF_NOIMP.csv)

8. **Plots**
   - [plots.R](scripts/plot.R) output:[Figure 2.PNG](https://github.com/Aquapop/ML_ED/blob/main/Data/output/Figure%202.PNG); [Figure 3.png](https://github.com/Aquapop/ML_ED/blob/main/Data/output/Figure%203.png)
; [Figure 4.png](https://github.com/Aquapop/ML_ED/blob/main/Data/output/Figure%204.png)
## Flowchart
![image](https://github.com/Aquapop/ML_ED/blob/main/flowchart/Figure%201.png) 




## Prerequisites

- **R:** Ensure that R (version 4.4 or higher) is installed on your system. You can download R from [CRAN](https://cran.r-project.org/).
- **RStudio (Optional):** While not mandatory, RStudio provides a user-friendly interface for working with R.

