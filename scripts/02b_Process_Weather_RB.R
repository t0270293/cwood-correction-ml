# ==============================================================================
# Script: 02b_Process_Weather_RB.R
# Author: I S Wong
# Purpose: Extracts meteorological variables from NetCDF files (ERA5) for RB sites.
#          Replaces the manual year-by-year scripts (weather_2009_RB.R, etc.)
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(raster)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(data.table)

# 2. Setup & Load Sites ---------------------------------------------------
# Load the processed air quality data from Step 01b to get site coordinates
rb_data_file <- "data/processed/RB_AirQuality_Processed.rds"

if (!file.exists(rb_data_file)) {
  stop("Error: Input file 'RB_AirQuality_Processed.rds' not found. Please run script 01b first.")
}

Data_RB <- readRDS(rb_data_file)

# Extract unique site metadata (coordinates are needed for extraction)
Data_sites_RB <- Data_RB %>%
  dplyr::select(site, site_name, latitude, longitude) %>%
  distinct() %>%
  as.data.frame() # Raster package prefers data.frames over tibbles

# Define the weather variables to extract
weather_vars <- c("t2m", "d2m", "blh", "slhf", "sshf", "sp", "tp", "ptype", 
                  "u10", "v10", "i10fg", "tcc", "e")

# Define raw weather folder
weather_raw_dir <- "data/weather_raw"

# 3. Define Extraction Function -------------------------------------------
# This function processes ONE year of data for ALL variables
process_weather_year <- function(year, sites_df, var_list, input_dir) {
  
  nc_file <- file.path(input_dir, paste0("weather_", year, ".nc"))
  message(paste("Processing file:", nc_file))
  
  if (!file.exists(nc_file)) {
    warning(paste("File not found:", nc_file, "- Skipping year"))
    return(NULL)
  }
  
  # List to store results for this year
  year_data_list <- list()
  
  # Loop through each weather variable (t2m, d2m, etc.)
  for (var in var_list) {
    message(paste("  - Extracting variable:", var))
    
    # Load the specific variable layer from NetCDF
    # suppressWarnings to avoid "CRS not found" noise
    r_brick <- suppressWarnings(brick(nc_file, varname = var))
    
    # Extract values at site locations
    extracted_values <- raster::extract(r_brick, cbind(sites_df$longitude, sites_df$latitude))
    
    # Convert to Data Frame
    df_var <- as.data.frame(extracted_values)
    
    # Clean Column Names (Raster converts timestamps to X2009.01.01...)
    names(df_var) <- sub('^.', '', names(df_var))
    
    # Bind site info
    df_var <- cbind(sites_df[, c("site", "site_name")], df_var)
    
    # Pivot to Long Format
    df_long <- df_var %>%
      pivot_longer(
        cols = -c(site, site_name),
        names_to = "date_str",
        values_to = var
      )
    
    year_data_list[[var]] <- df_long
  }
  
  # Merge all variable dataframes for this year into one
  full_year_df <- Reduce(function(x, y) merge(x, y, by = c("site", "site_name", "date_str")), year_data_list)
  
  return(full_year_df)
}

# 4. Main Processing Loop (2009-2020) -------------------------------------
years_to_process <- 2009:2020

# Loop through years and combine into one big dataframe
cwood_weather_RB <- map_dfr(years_to_process, function(y) {
  process_weather_year(y, Data_sites_RB, weather_vars, weather_raw_dir)
})

# 5. Final Cleaning & Saving ----------------------------------------------
message("Finalizing data structure...")

cwood_weather_RB <- cwood_weather_RB %>%
  mutate(
    # Convert string date to actual POSIXct date
    date_end = parse_date_time(date_str, orders = "ymdHMS"),
    date_end = as.character(date_end) 
  ) %>%
  dplyr::select(-date_str) %>%
  arrange(site, date_end)

# Save the final file
outfile <- "data/processed/RB_Weather_Processed.rds"
saveRDS(cwood_weather_RB, outfile)

message(paste("Success! All weather data processed and saved to:", outfile))