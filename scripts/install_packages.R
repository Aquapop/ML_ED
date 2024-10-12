# install_packages.R
# This script installs and loads all necessary R packages required for the project.
# It checks if each package is installed; if not, it installs the package and then loads it.

# List of all required packages
required_packages <- c(
  "dplyr",
  "lightgbm",
  "xgboost",
  "catboost",
  "ROCit",
  "caret",
  "mice",
  "missRanger",
  "doParallel",
  "foreach",
  "ggplot2",
  "car",
  "dunn.test",
  "rms",
  "boot",
  "shapviz",
  "progress",
  "rmda",
  "ufs",
  "UBL",
  "Matrix",
  "data.table",
  "rlang"
)

# Function to check, install, and load packages
install_and_load <- function(packages) {
  # Get the list of already installed packages
  installed_packages <- rownames(installed.packages())
  
  # Iterate over each package
  for (pkg in packages) {
    if (!(pkg %in% installed_packages)) {
      # Install the package if not already installed
      message(paste("Installing package:", pkg))
      install.packages(pkg, dependencies = TRUE)
    } else {
      message(paste("Package already installed:", pkg))
    }
    
    # Load the package
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    message(paste("Loaded package:", pkg))
  }
}

# Call the function with the required packages
install_and_load(required_packages)

# Inform the user that all packages have been installed and loaded
message("All required packages have been installed and loaded successfully.")
