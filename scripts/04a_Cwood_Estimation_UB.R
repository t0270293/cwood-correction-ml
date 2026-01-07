# ==============================================================================
# Script: 04a_Cwood_Estimation_UB.R
# Author: I S Wong
# Purpose: Estimates Wood Burning contribution (Cwood) at Urban Background sites.
#          1. Trains ML models (RF, XGB, Ensemble) on Morning data (Traffic dominated).
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
message("Loading and merging data...")

# Load processed data from previous steps
aq_file      <- "data/processed/UB_AirQuality_Processed.rds"
weather_file <- "data/processed/UB_Weather_Processed.rds"

if(!file.exists(aq_file) | !file.exists(weather_file)) {
  stop("Error: Processed files not found. Run scripts 01a and 02a first.")
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
# Morning: 05:00 - 14:00 (Training Period - Assumed Traffic Dominated)
# Evening: 15:00 - 04:00 (Prediction Period - Wood Burning active)
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
# Predictors selected from Feature Selection (Script 03a) or manual list
# Note: Ensure 'cwood' exists in your input data. If the column is named 'bc', change target below.
target_var <- "cwood" 
features   <- c("pm2.5", "no", "dec_day", "longitude", "t2m", "no2", "blh", "pm10", "o3")

message(paste("Target Variable:", target_var))
message(paste("Predictors:", paste(features, collapse=", ")))

# 4. Create Training & Prediction Sets ------------------------------------

# Training Set: Morning data where the Target (cwood) is NOT missing
# (This corresponds to your "6 sites" with data)
Data_Train_Morning <- Data_Full %>%
  filter(Time_of_Day == "Morning") %>%
  drop_na(all_of(target_var)) 

# Prediction Set: Evening data for ALL sites (even those missing cwood)
# (This corresponds to your "10 sites" prediction target)
Data_Predict_Evening <- Data_Full %>%
  filter(Time_of_Day == "Evening")

# Convert to H2O Frames
h2o_train <- as.h2o(Data_Train_Morning)
h2o_predict_evening <- as.h2o(Data_Predict_Evening)

# Split Training Data for Validation
splits <- h2o.splitFrame(h2o_train, ratios = 0.8, seed = 123)
train  <- splits[[1]]
valid  <- splits[[2]]

# 5. Baseline Model Training (Ensemble) -----------------------------------
message("Training Baseline Models (RF + XGBoost + Stacked Ensemble)...")

# A. Random Forest
rf_model <- h2o.randomForest(
  x = features, y = target_var,
  training_frame = train, validation_frame = valid,
  model_id = "rf_UB_baseline",
  ntrees = 50, stopping_rounds = 3, stopping_metric = "rmse",
  keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

# B. XGBoost
xgb_model <- h2o.xgboost(
  x = features, y = target_var,
  training_frame = train, validation_frame = valid,
  model_id = "xgb_UB_baseline",
  stopping_rounds = 3, stopping_metric = "rmse",
  keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

# C. Stacked Ensemble
ensemble_model <- h2o.stackedEnsemble(
  x = features, y = target_var,
  training_frame = train,
  base_models = list(rf_model, xgb_model),
  model_id = "ensemble_UB_baseline"
)

# Evaluate Performance
perf <- h2o.performance(ensemble_model, valid)
print(perf)

# 6. Predict & Calculate Residuals (Baseline) -----------------------------
message("Generating Baseline Predictions...")

# Predict on Evening Data
preds <- h2o.predict(ensemble_model, h2o_predict_evening)
preds_df <- as.data.frame(preds)

# Attach predictions back to the main dataframe
Data_Predict_Evening$cwood_predicted_baseline <- preds_df$predict

# Calculate Residual (Observed - Predicted)
# This 'Excess' is the estimated Wood Burning contribution
Data_Predict_Evening <- Data_Predict_Evening %>%
  mutate(
    cwood_excess_baseline = get(target_var) - cwood_predicted_baseline
  )

# 7. SENSITIVITY TEST (Defra Scenario) ------------------------------------
message("--- Starting Sensitivity Test (Morning * 0.71) ---")

# Adjust Training Target: Assume Morning cwood is only 71% traffic (29% is wood)
# We want to train the model to learn ONLY the traffic part.
Data_Train_Morning$cwood_sens <- Data_Train_Morning[[target_var]] * 0.71

# Create new H2O Frame for Sensitivity Training
h2o_train_sens <- as.h2o(Data_Train_Morning)
splits_sens <- h2o.splitFrame(h2o_train_sens, ratios = 0.8, seed = 123)
train_sens  <- splits_sens[[1]]
valid_sens  <- splits_sens[[2]]

# Train Sensitivity Models (Same structure, new target)
target_sens <- "cwood_sens"

rf_sens <- h2o.randomForest(
  x = features, y = target_sens,
  training_frame = train_sens, validation_frame = valid_sens,
  model_id = "rf_UB_sens",
  ntrees = 50, keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

xgb_sens <- h2o.xgboost(
  x = features, y = target_sens,
  training_frame = train_sens, validation_frame = valid_sens,
  model_id = "xgb_UB_sens",
  keep_cross_validation_predictions = TRUE, nfolds = 5, seed = 123
)

ensemble_sens <- h2o.stackedEnsemble(
  x = features, y = target_sens,
  training_frame = train_sens,
  base_models = list(rf_sens, xgb_sens),
  model_id = "ensemble_UB_sens"
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

# Calculate Mean Differences
stats <- Data_Predict_Evening %>%
  summarise(
    Mean_Excess_Baseline = mean(cwood_excess_baseline, na.rm=TRUE),
    Mean_Excess_Sens     = mean(cwood_excess_sens, na.rm=TRUE),
    Diff_Percentage      = (mean(cwood_excess_sens, na.rm=TRUE) - mean(cwood_excess_baseline, na.rm=TRUE)) / mean(cwood_excess_baseline, na.rm=TRUE) * 100
  )
print(stats)

# Save the Results
if(!dir.exists("data/results")) dir.create("data/results")

# Save the Dataframe containing Predictions and Residuals
saveRDS(Data_Predict_Evening, "data/results/UB_Cwood_Estimates_WithSensitivity.rds")

# Save Models (Optional, but good practice)
# h2o.saveModel(ensemble_model, path = "data/results/models/UB_Baseline_Ensemble")
# h2o.saveModel(ensemble_sens, path = "data/results/models/UB_Sens_Ensemble")

message("Done! Results saved to 'data/results/UB_Cwood_Estimates_WithSensitivity.rds'")