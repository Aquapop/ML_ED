# univariate_analysis.R
# This script performs univariate analysis, including Chi-square tests and t-tests,
# to evaluate the association between various variables and the target variable "KIQ400".
# It also generates data distributions and statistical summaries, and exports the results as CSV files.

# Load necessary libraries
library(survey)
library(dplyr)
library(haven)  # For reading .XPT files

# Read the cleaned data
datat <- read.csv("outputs/data105.csv")

# Define survey design
design <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTMEC2YR, data = datat, nest = TRUE)

# Define target variable
target_variable <- "KIQ400"

# List of all categorical variables
factorCols <- c(
  "DMDEDUC",
  "SMQ020",
  "ALQ110",
  "PAD590",
  "PAD320",
  "BPQ040A",
  "BPQ090D",
  "DIQ010",
  "MCQ220",
  "KIQ081",
  "KIQ101",
  "KIQ106",
  "KIQ121",
  "KIQ141",
  "KIQ182",
  "KIQ321",
  "KIQ341",
  "KIQ115",
  "SXQ280",
  "SXQ260",
  "SXQ265",
  "SXQ270",
  "SXQ272",
  "STI",
  "CDQ001",
  "CDQ010",
  "CVDFITLV",
  "ProstateExam"
)

# Get all variable names
all_vars <- names(datat)

# Define list of continuous variables by excluding categorical and target variables
var_list <- setdiff(all_vars, c(factorCols, "KIQ400")) 

# Batch perform Chi-square tests
chi_tests_results <- lapply(factorCols, function(var) {
  formula <- as.formula(paste0("~ ", var, " + ", "KIQ400"))
  chi_test <- svychisq(formula, subset(design, KIQ400 %in% c("0","1")))
  print(chi_test)
})

# Batch process categorical variable distributions
# Use lapply to generate distribution for each variable and store in a list
distribution_results <- lapply(factorCols, function(var) {
  data_distribution <- datat %>%
    filter(!is.na(.data[[var]])) %>%
    count(KIQ400, !!sym(var)) %>%
    group_by(KIQ400) %>%
    mutate(percentage = n / sum(n) * 100)
  return(data_distribution)
})

# Create an empty dataframe to store the organized results
final_results <- data.frame(
  variable = character(),
  value = integer(),
  `KIQ400_1_n(percentage%)` = character(),
  `KIQ400_0_n(percentage%)` = character(),
  stringsAsFactors = FALSE
)

# Iterate over each distribution result, extract and organize data
for (data in distribution_results) {
  # Get the current variable name
  variable_name <- names(data)[2]
  
  # Split data into KIQ400 == 0 and KIQ400 == 1
  data_0 <- filter(data, KIQ400 == 0)
  data_1 <- filter(data, KIQ400 == 1)
  
  # Merge the two datasets
  merged_data <- full_join(
    data_0 %>% select(value = 2, n_0 = n, percentage_0 = percentage),
    data_1 %>% select(value = 2, n_1 = n, percentage_1 = percentage),
    by = "value"
  )
  
  # Add variable name and format the results
  merged_data <- merged_data %>%
    mutate(
      variable = variable_name,
      `KIQ400_0_n(percentage%)` = paste(n_0, "(", round(percentage_0, 1), "%)", sep = ""),
      `KIQ400_1_n(percentage%)` = paste(n_1, "(", round(percentage_1, 1), "%)", sep = "")
    ) %>%
    select(variable, value, `KIQ400_1_n(percentage%)`, `KIQ400_0_n(percentage%)`)
  
  # Append to the final results dataframe
  final_results <- bind_rows(final_results, merged_data)
}

# View the organized Chi-square test results
print(final_results)

# Batch perform t-tests on continuous variables
t_test_results <- lapply(var_list, function(var) {
  formula <- as.formula(paste0(var, "~ ", "KIQ400"))
  t_test <- svyttest(formula, design)
  print(t_test)
})

# Batch process continuous variable summaries
# Use lapply to generate mean and SD for each variable and store in a list
summary_results <- lapply(var_list, function(var) {
  data_summary <- datat %>%
    group_by(KIQ400) %>%
    summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE),
      min = min(.data[[var]], na.rm = TRUE),
      max = max(.data[[var]], na.rm = TRUE)
    ) %>%
    mutate(variable = var)
  return(data_summary)
})

# Create an empty dataframe to store the organized summaries
final_summary <- data.frame(
  variable = character(),
  range = character(),
  `KIQ400_0_mean±sd` = character(),
  `KIQ400_1_mean±sd` = character(),
  stringsAsFactors = FALSE
)

# Iterate over each summary result, extract and organize data
for (data in summary_results) {
  # Get the current variable name
  variable_name <- unique(data$variable)
  
  # Split data into KIQ400 == 0 and KIQ400 == 1
  data_0 <- filter(data, KIQ400 == 0)
  data_1 <- filter(data, KIQ400 == 1)
  
  # Check if there is valid data to avoid errors with empty dataframes
  if (nrow(data_0) > 0 && nrow(data_1) > 0) {
    # Organize data and add range information
    summary_data <- data.frame(
      variable = variable_name,
      range = paste(data$min[1], "-", data$max[1], sep = ""),
      `KIQ400_0_mean±sd` = paste(round(data_0$mean, 2), "±", round(data_0$sd, 2), sep = ""),
      `KIQ400_1_mean±sd` = paste(round(data_1$mean, 2), "±", round(data_1$sd, 2), sep = "")
    )
    
    # Append to the final summary dataframe
    final_summary <- bind_rows(final_summary, summary_data)
  }
}

# View the organized summary results
print(final_summary)

# Based on the above final_summary, t_test_results, chi_tests_results, and final_results,
# create the baseline analysis result Table 1 
# And remove features without statistical differences from the univariate analysis to get data105

# Export the final results as CSV files
write.csv(final_results, "outputs/chi_square_results.csv", row.names = FALSE)
write.csv(final_summary, "outputs/t_test_results.csv", row.names = FALSE)
