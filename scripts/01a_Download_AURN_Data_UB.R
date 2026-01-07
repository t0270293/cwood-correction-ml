# ==============================================================================
# Script: 01a_Download_AURN_Data_UB.R
# Author: I S Wong
# Purpose: Downloads Urban Background (UB) air quality data from AURN, 
#          merges it with Cwood site data, and fills missing time gaps.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(saqgetr)
library(tidyverse)
library(lubridate)
library(h2o)

# Initialize H2O (if needed later, though mostly used in modeling scripts)
# h2o.init(nthreads = -1, max_mem_size = "12G") 

# 2. Define Sites & Parameters --------------------------------------------
UBsites <- c("gb1028a", "gb0620a", "gb0851a", "gb0580a", "gb0839a",
             "gb0995a", "gb0613a", "gb0658a", "gb0567a", "gb0864a")

start_date <- "2009-01-01"
end_date   <- "2020-12-31"

# 3. Load Local Cwood Site Data -------------------------------------------
# NOTE: Ensure this file is placed in the 'data' folder
cwood_file <- "data/CwoodAir_2009to2020_UB.txt"

if(!file.exists(cwood_file)) {
  stop("Error: File not found. Please place 'CwoodAir_2009to2020_UB.txt' inside the 'data/' folder.")
}

CwoodSitesData_UB <- read.delim(cwood_file, header = TRUE) %>%
  as_tibble()

# 4. Download AURN Data ---------------------------------------------------
message("Downloading AURN data for ", length(UBsites), " sites...")

Data_AirPollutants_UB <- get_saq_observations(
  site = UBsites,
  start = start_date,
  end = end_date,
  variable = c("o3", "no2", "nox", "so2", "co", "pm10", "pm2.5", "bc"),
  verbose = TRUE
) %>%
  saq_clean_observations(summary = "hour", valid_only = FALSE, spread = TRUE)

# Fix date_end (saqgetr sometimes returns date_end as the start of the next hour)
# We ensure strictly hourly intervals
Data_AirPollutants_UB <- Data_AirPollutants_UB %>%
  mutate(
    date = as.POSIXct(date, tz = "Europe/London"),
    date_end = date + hours(1)
  )

# 5. Get Site Metadata ----------------------------------------------------
Data_sites_UB <- get_saq_sites() %>% 
  filter(site %in% UBsites) %>%
  select(site, site_name, latitude, longitude, elevation, site_type, site_area) %>%
  mutate(type = paste(site_area, site_type))

# 6. Merge Datasets -------------------------------------------------------
# Merge AURN data with local Cwood data
Data_Combined <- Data_AirPollutants_UB %>%
  left_join(CwoodSitesData_UB, by = c("date", "site")) %>% 
  arrange(site, date)

# 7. Fill Missing Gaps (The "Vectorized" Way) -----------------------------
# Instead of repeating code 10 times, we use 'complete' to fill missing hours for ALL sites at once.

message("Filling missing time gaps...")

# Create the full time sequence
full_time_seq <- seq(
  from = as.POSIXct(paste(start_date, "00:00:00"), tz = "Europe/London"),
  to   = as.POSIXct(paste(end_date, "23:00:00"),   tz = "Europe/London"),
  by   = "hour"
)

Data_UB <- Data_Combined %>%
  group_by(site) %>%
  # This one line replaces the 10 chunks of repeated code!
  complete(date = full_time_seq) %>% 
  ungroup() %>%
  # Fill in the static site metadata for the newly created 'empty' rows
  left_join(Data_sites_UB, by = "site") %>%
  mutate(
    date_end = date + hours(1),
    Date = as.Date(date),
    Hour = hour(date),
    Month = month(date),
    Weekday = wday(date, week_start = 1), # 1 = Monday
    Time = format(date, "%H:%M:%S"),
    no = nox - no2 # Calculate NO
  ) %>%
  arrange(site, date)

# 8. Save Output ----------------------------------------------------------
# Create folder if it doesn't exist
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

saveRDS(Data_UB, "data/processed/UB_AirQuality_Processed.rds")

message("Done! Processed data saved to 'data/processed/UB_AirQuality_Processed.rds'")