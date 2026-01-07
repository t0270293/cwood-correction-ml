# ==============================================================================
# Script: 03a_FeatureSelection_UB.R
# Author: I S Wong
# Purpose: Merges AQ and Weather data, splits into Morning/Evening, 
#          and performs Recursive Feature Elimination (RFE) to find best predictors.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(tidyverse)
library(caret)       # For Feature Selection (RFE)
library(lubridate)
library(randomForest) # Required for the RFE functions

# 2. Load & Merge Data ----------------------------------------------------
message("Loading and merging data...")

# Load the processed outputs from step 01a and 02a
aq_file      <- "data/processed/UB_AirQuality_Processed.rds"
weather_file <- "data/processed/UB_Weather_Processed.rds"

if(!file.exists(aq_file) | !file.exists(weather_file)) {
  stop("Error: Input files not found. Please run scripts 01a and 02a first.")
}

Data_AQ      <- readRDS(aq_file)
Data_Weather <- readRDS(weather_file)

# Merge Air Quality and Weather data by Site and Date
# We use inner_join to ensure we only keep rows where we have BOTH types of data
Data_Combined <- inner_join(Data_AQ, Data_Weather, by = c("site", "date_end"))

# 3. Data Cleaning & Preparation ------------------------------------------
message("Preparing datasets for analysis...")

# Define Morning (05:00 - 14:00) vs Evening (15:00 - 04:00)
# Note: 0:4 means 00:00 to 04:00
Data_Combined <- Data_Combined %>%
  mutate(
    Time_of_Day = case_when(
      Hour %in% 5:14 ~ "Morning",
      Hour %in% c(0:4, 15:23) ~ "Evening",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Time_of_Day)) %>% 
  # Filter data until Nov 2019 as per your original script
  filter(date_end < as.POSIXct("2019-12-01"))

# Select candidate predictors (features)
# IMPORTANT: I selected these based on your previous 'c(7,9,10...)' logic but using NAMES.
# Please check this list: 
candidate_features <- c(
  "no2", "nox", "pm10", "pm2.5", "o3", # Pollutants
  "t2m", "d2m", "blh", "slhf", "sshf", "sp", "tp", "u10", "v10", "tcc", "e", # Weather
  "Weekday", "Month", "no" # Temporal / Derived
)

# Target variable (The thing we want to predict)
target_var <- "bc" # Assuming Black Carbon is the target. Change to "cwood" if needed.

# 4. Feature Selection Function -------------------------------------------
run_rfe_analysis <- function(data, subset_name) {
  
  message(paste0("\n--- Running RFE for: ", subset_name, " ---"))
  
  # Filter for the specific time of day
  df_subset <- data %>% filter(Time_of_Day == subset_name)
  
  # Select only columns needed + remove rows with NAs in these specific columns
  df_model <- df_subset %>%
    select(all_of(c(candidate_features, target_var))) %>%
    na.omit()
  
  # Check if we have enough data
  if(nrow(df_model) < 100) {
    warning("Not enough data to run RFE.")
    return(NULL)
  }
  
  # Subsample: RFE is slow, so we sample 3000 rows (as in your original code)
  set.seed(123)
  if(nrow(df_model) > 3000) {
    df_sample <- df_model[sample(nrow(df_model), 3000), ]
  } else {
    df_sample <- df_model
  }
  
  # Define RFE Controls (Random Forest selection)
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
  
  # Run RFE
  # x = predictors, y = target
  results <- rfe(
    x = df_sample[, candidate_features], 
    y = df_sample[[target_var]],
    sizes = c(1:15), # Try keeping top 1 to 15 features
    rfeControl = control
  )
  
  # Print & Plot Results
  print(results)
  print(predictors(results))
  
  # Save a quick plot
  plot_file <- paste0("plots/RFE_Result_", subset_name, "_UB.png")
  if(!dir.exists("plots")) dir.create("plots")
  png(plot_file)
  print(ggplot(results, metric = "Rsquared") + 
          ggtitle(paste("Feature Selection (RFE) -", subset_name)))
  dev.off()
  message(paste("Plot saved to:", plot_file))
  
  return(results)
}

# 5. Execute Analysis -----------------------------------------------------

# Run for Morning
rfe_morning <- run_rfe_analysis(Data_Combined, "Morning")

# Run for Evening
rfe_evening <- run_rfe_analysis(Data_Combined, "Evening")

# 6. Save Final Selected Features -----------------------------------------
# You can save these objects to load in the next script
save(rfe_morning, rfe_evening, file = "data/processed/FeatureSelection_Results_UB.RData")