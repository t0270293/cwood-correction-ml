# ==============================================================================
# Script: 04b_Cwood_Estimation_RB.R
# Author: I S Wong
# Purpose: Estimates Wood Burning contribution (Cwood) at Rural Background sites.
#          1. Trains ML models (RF, XGB, Ensemble) on Morning data.
#          2. Predicts 'Traffic Cwood' in the Evening.
#          3. Calculates Excess Cwood (Residuals).
#          4. Performs Sensitivity Test (Morning Cwood reduced by 29%).
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(tidyverse)
library(lubridate)
library(h2o)
library(openair)
library(data.table)

# Initialize H2O
h2o.init(nthreads = -1, max_mem_size = "12G")

# 2. Load & Prepare Data --------------------------------------------------
message("Loading and merging RB data...")

# Load processed data
aq_file      <- "data/processed/RB_AirQuality_Processed.rds"
weather_file <- "data/processed/RB_Weather_Processed.rds"

if(!file.exists(aq_file) | !file.exists(weather_file)) {
  stop("Error: Processed files not found. Run scripts 01b and 02b first.")
}

Data_AQ      <- readRDS(aq_file)
Data_Weather <- readRDS(weather_file)

# Merge datasets
Data_Full <- inner_join(Data_AQ, Data_Weather, by = c("site", "date_end")) %>%
  mutate(
    Year = year(date),
    Month = month(date),
    Weekday = wday(date, week_start = 1),
    dec_day = decimal_date(date)
  )

# Define Time Windows
Data_Full <- Data_Full %>%
  mutate(
    Time_of_Day = case_when(
      Hour %in% 5:14 ~ "Morning",
      Hour %in% c(0:4, 15:23) ~ "Evening",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Time_of_Day))

# 3. Define Features & Target ---------------------------------------------
target_var <- "cwood"

# PREDICTORS FOR RURAL SITES (Based on your script 04b)
# Note: These differ slightly from Urban sites (e.g., includes wd, d2m)
features   <- c("no2", "pm2.5", "dec_day", "no", "d2m", "wd", "o3", "t2m", "blh")

message(paste("Target Variable:", target_var))
message(paste("Predictors:", paste(features, collapse=", ")))

# 4. Create Training & Prediction Sets ------------------------------------

# Training Set: Morning data where the Target (cwood) is NOT missing
Data_Train_Morning <- Data_Full %>%
  filter(Time_of_Day == "Morning") %>%
  drop_na(all_of(target_var)) 

# Prediction Set: Evening data for ALL sites
Data_Predict_Evening <- Data_Full %>%
  filter(Time_of_Day == "Evening")

# Convert to H2O Frames
h2o_train <- as.h2o(Data_Train_Morning)
h2o_predict_evening <- as.h2o(Data_Predict_Evening)

# Split Training Data
splits <- h2o.splitFrame(h2o_train, ratios = 0.8, seed = 123)
train  <- splits[[1]]
valid  <- splits[[2]]

# 5. Baseline Model Training (Ensemble) -----------------------------------
message("Training Baseline Models (RB)...")

# A. Random Forest
rf_model <- h2o.randomForest(
  x = features, y = target_var,
  training_frame = train, validation_frame = valid,
  model_id = "rf_RB_baseline",
  ntrees = 50, stopping_rounds = 3, stopping_metric = "rmse",
  keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

# B. XGBoost
xgb_model <- h2o.xgboost(
  x = features, y = target_var,
  training_frame = train, validation_frame = valid,
  model_id = "xgb_RB_baseline",
  stopping_rounds = 3, stopping_metric = "rmse",
  keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

# C. Stacked Ensemble
ensemble_model <- h2o.stackedEnsemble(
  x = features, y = target_var,
  training_frame = train,
  base_models = list(rf_model, xgb_model),
  model_id = "ensemble_RB_baseline"
)

# Evaluate Performance
perf <- h2o.performance(ensemble_model, valid)
print(perf)

# 6. Predict & Calculate Residuals (Baseline) -----------------------------
message("Generating Baseline Predictions...")

preds <- h2o.predict(ensemble_model, h2o_predict_evening)
preds_df <- as.data.frame(preds)

Data_Predict_Evening$cwood_predicted_baseline <- preds_df$predict

Data_Predict_Evening <- Data_Predict_Evening %>%
  mutate(
    cwood_excess_baseline = get(target_var) - cwood_predicted_baseline
  )

# 7. SENSITIVITY TEST (Defra Scenario) ------------------------------------
message("--- Starting Sensitivity Test (Morning * 0.71) ---")

# Adjust Training Target
Data_Train_Morning$cwood_sens <- Data_Train_Morning[[target_var]] * 0.71

# Create new H2O Frame
h2o_train_sens <- as.h2o(Data_Train_Morning)
splits_sens <- h2o.splitFrame(h2o_train_sens, ratios = 0.8, seed = 123)
train_sens  <- splits_sens[[1]]
valid_sens  <- splits_sens[[2]]

target_sens <- "cwood_sens"

# Train Sensitivity Models
rf_sens <- h2o.randomForest(
  x = features, y = target_sens,
  training_frame = train_sens, validation_frame = valid_sens,
  model_id = "rf_RB_sens",
  ntrees = 50, keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

xgb_sens <- h2o.xgboost(
  x = features, y = target_sens,
  training_frame = train_sens, validation_frame = valid_sens,
  model_id = "xgb_RB_sens",
  keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

ensemble_sens <- h2o.stackedEnsemble(
  x = features, y = target_sens,
  training_frame = train_sens,
  base_models = list(rf_sens, xgb_sens),
  model_id = "ensemble_RB_sens"
)

# Predict Sensitivity Scenario
preds_sens <- h2o.predict(ensemble_sens, h2o_predict_evening)
preds_sens_df <- as.data.frame(preds_sens)

Data_Predict_Evening$cwood_predicted_sens <- preds_sens_df$predict
Data_Predict_Evening <- Data_Predict_Evening %>%
  mutate(
    cwood_excess_sens = get(target_var) - cwood_predicted_sens
  )

# 8. Compare & Save Results -----------------------------------------------
message("Calculating Statistics & Saving...")

stats <- Data_Predict_Evening %>%
  summarise(
    Mean_Excess_Baseline = mean(cwood_excess_baseline, na.rm=TRUE),
    Mean_Excess_Sens     = mean(cwood_excess_sens, na.rm=TRUE)
  )
print(stats)

# Save the Results
if(!dir.exists("data/results")) dir.create("data/results")

saveRDS(Data_Predict_Evening, "data/results/RB_Cwood_Estimates_WithSensitivity.rds")

message("Done! Results saved to 'data/results/RB_Cwood_Estimates_WithSensitivity.rds'")