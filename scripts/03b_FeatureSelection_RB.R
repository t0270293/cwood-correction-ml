# ==============================================================================
# Script: 03b_FeatureSelection_RB.R
# Author: I S Wong
# Purpose: Merges AQ and Weather data for Rural Background (RB) sites, 
#          splits into Morning/Evening, and runs RFE Feature Selection.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(tidyverse)
library(caret)       # For Feature Selection (RFE)
library(lubridate)
library(randomForest) # Required for the RFE functions

# 2. Load & Merge Data ----------------------------------------------------
message("Loading and merging RB data...")

# Load the processed outputs from step 01b and 02b
aq_file      <- "data/processed/RB_AirQuality_Processed.rds"
weather_file <- "data/processed/RB_Weather_Processed.rds"

if(!file.exists(aq_file) | !file.exists(weather_file)) {
  stop("Error: Input files not found. Please run scripts 01b and 02b first.")
}

Data_AQ      <- readRDS(aq_file)
Data_Weather <- readRDS(weather_file)

# Merge Air Quality and Weather data
# inner_join ensures we only use rows with complete data
Data_Combined <- inner_join(Data_AQ, Data_Weather, by = c("site", "date_end"))

# 3. Data Cleaning & Preparation ------------------------------------------
message("Preparing datasets for analysis...")

Data_Combined <- Data_Combined %>%
  mutate(
    Time_of_Day = case_when(
      Hour %in% 5:14 ~ "Morning",
      Hour %in% c(0:4, 15:23) ~ "Evening",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Time_of_Day)) %>% 
  # Filter data until Nov 2019 (Consistency with AE22 data availability)
  filter(date_end < as.POSIXct("2019-12-01"))

# Select candidate predictors (Standardized list)
# Note: 'co' and 'so2' are excluded due to high missing values, just like in your original script
candidate_features <- c(
  "no2", "nox", "pm10", "pm2.5", "o3", # Pollutants
  "t2m", "d2m", "blh", "slhf", "sshf", "sp", "tp", "u10", "v10", "tcc", "e", # Weather
  "Weekday", "Month", "no" # Temporal / Derived
)

# Target variable
target_var <- "bc" # Change to "cwood" if that is your target for this paper

# 4. Feature Selection Function -------------------------------------------
run_rfe_analysis <- function(data, subset_name) {
  
  message(paste0("\n--- Running RFE for: ", subset_name, " ---"))
  
  df_subset <- data %>% filter(Time_of_Day == subset_name)
  
  # Select columns and remove NAs
  df_model <- df_subset %>%
    select(all_of(c(candidate_features, target_var))) %>%
    na.omit()
  
  if(nrow(df_model) < 100) {
    warning("Not enough data to run RFE.")
    return(NULL)
  }
  
  # Subsample 3000 rows for computational efficiency
  set.seed(123)
  if(nrow(df_model) > 3000) {
    df_sample <- df_model[sample(nrow(df_model), 3000), ]
  } else {
    df_sample <- df_model
  }
  
  # Define RFE Controls
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
  
  # Run RFE
  results <- rfe(
    x = df_sample[, candidate_features], 
    y = df_sample[[target_var]],
    sizes = c(1:15), 
    rfeControl = control
  )
  
  # Print & Plot
  print(results)
  print(predictors(results))
  
  # Save plot
  plot_file <- paste0("plots/RFE_Result_", subset_name, "_RB.png")
  if(!dir.exists("plots")) dir.create("plots")
  png(plot_file)
  print(ggplot(results, metric = "Rsquared") + 
          ggtitle(paste("Feature Selection (RFE) -", subset_name, "(Rural)")))
  dev.off()
  message(paste("Plot saved to:", plot_file))
  
  return(results)
}

# 5. Execute Analysis -----------------------------------------------------

# Run for Morning
rfe_morning_rb <- run_rfe_analysis(Data_Combined, "Morning")

# Run for Evening
rfe_evening_rb <- run_rfe_analysis(Data_Combined, "Evening")

# 6. Save Final Selected Features -----------------------------------------
save(rfe_morning_rb, rfe_evening_rb, file = "data/processed/FeatureSelection_Results_RB.RData")