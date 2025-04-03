setwd("C:/Users/Sayan/Desktop/NFHS/")

# Load haven package
library(haven)
library(vroom)
library(sf)
library(tidyverse)   # For data manipulation and visualization
library(MatchIt)     # For propensity score matching
library(lmtest)      # For robust standard errors
library(sandwich)    # For robust standard errors
library(plm)         # For panel data models
library(broom)       # For tidy model output
library(cobalt)      # For assessing balance
library(ggplot2)     # For visualization
library(fixest)      # For fixed effects models

# Read the Stata .dta file
nfhs_data <- read_dta("./DATA/IAHR7EFL.DTA",n_max = 5)
var_labels <- sapply(nfhs_data, attr, "label")
var_labels_df <- data.frame(Variable = names(var_labels), Description = unname(var_labels))
write.csv(var_labels_df, "./DATA/nfhs_variable_labels.csv", row.names = FALSE)

# Matching
treat_clusters <- c("93142", "93128", "93127", "93110")

nfhs_data$TREAT <- ifelse(as.character(nfhs_data$hv001) %in% treat_clusters, 1, 0)

# 2. Matching Variables (Pre-treatment characteristics)
matching_vars <- c(
  # Household Characteristics
  "hv005",    # Household sample weight
  "hv009",    # Number of household members
  "hv270",    # Wealth index combined
  "hv219",    # Sex of head of household
  "hv220",    # Age of head of household
  
  # Demographic Variables
  "hv104_01", # Sex of household members
  "hv105_01", # Age of household members
  
  # Education Variables
  "hv106_01", # Highest educational level
  "hv107_01", # Highest year of education
  
  # Water and Sanitation
  "hv201",    # Source of drinking water
  "hv204",    # Time to get to water source (minutes)
  "sh44",     # Access to toilet facility
  
  # Economic Indicators
  "sh71"      # Household health insurance coverage
)

# 3. Potential Outcome Variables
outcome_vars <- c(
  # Health-related Outcomes
  "shb20_01", # High blood pressure
  "shb56_01", # High blood glucose
  
  # Water and Sanitation Outcomes
  "sh37b",    # Water availability
  "sh45",     # Type of toilet facility
  
  # Economic Outcomes
  "hv270"     # Wealth index (can be used as an outcome)
)

# 4. DiD Estimation Function
did_estimation <- function(data) {
  # Propensity Score Matching to create comparable groups
  
  # Parallel Trends Check
  
  # Difference-in-Differences Regression
  did_model <- lm(
    outcome ~ treat + post + treat:post + 
      hv005 + hv009 + hv270 + hv219 + hv220,
    data = data
  )
  
  return(did_model)
}

# 5. Recommended Data Preparation Steps
prepare_did_data <- function(data) {
  data %>%
    # Create post-treatment indicator
    mutate(
      post = ifelse(DHSYEAR > 2019, 1, 0),
      # Ensure treatment is binary
      treat = as.numeric(as.character(DHSCLUST) %in% c("93142", "93128", "93127", "93110"))
    ) %>%
    # Select key variables
    select(
      hhid,       # Household ID
      DHSCLUST,   # Cluster ID
      DHSYEAR,    # Survey Year
      treat,      # Treatment indicator
      post,       # Post-treatment period
      all_of(matching_vars),
      all_of(outcome_vars)
    ) %>%
    # Handle missing values
    drop_na()
}

# Example Usage (Placeholder - replace with actual data loading)
# nfhs_data <- read_csv("your_nfhs_data.csv")
# prepared_data <- prepare_did_data(nfhs_data)
# model_results <- did_estimation(prepared_data)

# Diagnostic Checks
diagnostic_checks <- function(model) {
  # Check parallel trends assumption
  # Check balance of covariates
  # Placebo tests
  # Robustness checks
}

# Notes for Implementation:
# 1. Ensure you have pre and post-treatment data
# 2. Verify the specific clusters you want to use as treatment
# 3. Select appropriate outcome variables based on your research question
# 4. Conduct balance tests before main analysis
