# Source the updated function
source("R/Utilities/Analysis/run_anova.R")

# Load required packages
library(readxl)
library(dplyr)

# Load data
phenotypic_physiologic <- read_excel("Data/clean_compiled_data.xlsx", sheet = "phenotypic_physiologic")

# Run fat mass ANOVA with updated function
FM_ANOVA <- run_anova(data = phenotypic_physiologic, response = "FM")

# Check what method was used
cat("\n\n=== FINAL METHOD USED ===\n")
cat("Method:", FM_ANOVA$anova_method, "\n")
cat("Transformation:", FM_ANOVA$transformation, "\n")
cat("Assumptions met:", FM_ANOVA$assumptions_met, "\n")
