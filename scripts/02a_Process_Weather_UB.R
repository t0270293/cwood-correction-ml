# ==============================================================================
# Script: 02a_Process_Weather_UB.R
# Author: I S Wong
# Purpose: Extracts meteorological variables from NetCDF files (ERA5) for UB sites.
#          Replaces the manual year-by-year scripts (weather_2009_UB.R, etc.)
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(raster)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(data.table)

# 2. Setup & Load Sites ---------------------------------------------------
# Load the processed air quality data from Step 01a to get site coordinates
ub_data_file <- "data/processed/UB_AirQuality_Processed.rds"

if (!file.exists(ub_data_file)) {
  stop("Error: Input file 'UB_AirQuality_Processed.rds' not found. Please run script 01a first.")
}

Data_UB <- readRDS(ub_data_file)

# Extract unique site metadata (coordinates are needed for extraction)
Data_sites_UB <- Data_UB %>%
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
    # We remove the 'X' and convert to datetime
    names(df_var) <- sub('^.', '', names(df_var))
    
    # Bind site info
    df_var <- cbind(sites_df[, c("site", "site_name")], df_var)
    
    # Pivot to Long Format
    # This replaces the complex 'melt' and 'rowid' logic
    df_long <- df_var %>%
      pivot_longer(
        cols = -c(site, site_name),
        names_to = "date_str",
        values_to = var
      )
    
    year_data_list[[var]] <- df_long
  }
  
  # Merge all variable dataframes for this year into one
  # Reduce joins them by site, site_name, and date_str
  full_year_df <- Reduce(function(x, y) merge(x, y, by = c("site", "site_name", "date_str")), year_data_list)
  
  return(full_year_df)
}

# 4. Main Processing Loop (2009-2020) -------------------------------------
years_to_process <- 2009:2020

# Use map_dfr or lapply to loop through years and combine
# This creates one big dataframe for all years
cwood_weather_UB <- map_dfr(years_to_process, function(y) {
  process_weather_year(y, Data_sites_UB, weather_vars, weather_raw_dir)
})

# 5. Final Cleaning & Saving ----------------------------------------------
message("Finalizing data structure...")

cwood_weather_UB <- cwood_weather_UB %>%
  mutate(
    # Convert the string date (2009.01.01.00.00.00) to actual POSIXct date
    date_end = parse_date_time(date_str, orders = "ymdHMS"),
    date_end = as.character(date_end) # Keeping as character to match user format preference
  ) %>%
  dplyr::select(-date_str) %>% # Remove the temp column
  arrange(site, date_end)

# Save the final file
outfile <- "data/processed/UB_Weather_Processed.rds"
saveRDS(cwood_weather_UB, outfile)

message(paste("Success! All weather data processed and saved to:", outfile))