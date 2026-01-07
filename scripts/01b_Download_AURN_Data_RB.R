# ==============================================================================
# Script: 01b_Download_AURN_Data_RB.R
# Author: I S Wong
# Purpose: Downloads Rural Background (RB) air quality data from AURN,
#          merges it with local Detling & Cwood site data, and fills missing gaps.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(saqgetr)
library(tidyverse)
library(lubridate)
library(h2o)

# Initialize H2O (if needed later)
# h2o.init(nthreads = -1, max_mem_size = "12G")

# 2. Define Sites & Parameters --------------------------------------------
RBsites <- c("gb0048r", "gb1055r", "gb0886a", "gb0617a", "gb0036r",
             "gb0043r", "gb0754a", "gb0031r", "gb0033r", "gb0038r")

start_date <- "2009-01-01"
end_date   <- "2020-12-31"

# 3. Load Local Cwood & Detling Data --------------------------------------
# NOTE: Ensure these files are placed in the 'data' folder
cwood_file   <- "data/CwoodAir_2009to2020_RB.txt"
detling_file <- "data/Detling_Air_2009to2020.txt"

# Check if files exist
if(!file.exists(cwood_file)) stop("Error: 'CwoodAir_2009to2020_RB.txt' not found in data/ folder.")
if(!file.exists(detling_file)) stop("Error: 'Detling_Air_2009to2020.txt' not found in data/ folder.")

CwoodSitesData_RB <- read.delim(cwood_file, header = TRUE) %>% as_tibble()
Detling_APdata    <- read.delim(detling_file, header = TRUE) %>% as_tibble()

# 4. Download AURN Data ---------------------------------------------------
message("Downloading AURN data for ", length(RBsites), " sites...")

Data_AirPollutants_RB <- get_saq_observations(
  site = RBsites,
  start = start_date,
  end = end_date,
  variable = c("o3", "no2", "nox", "so2", "co", "pm10", "pm2.5"),
  verbose = TRUE
) %>%
  saq_clean_observations(summary = "hour", valid_only = FALSE, spread = TRUE)

# Fix date_end (saqgetr output adjustment)
Data_AirPollutants_RB <- Data_AirPollutants_RB %>%
  mutate(
    date = as.POSIXct(date, tz = "Europe/London"),
    date_end = date + hours(1)
  )

# 5. Append Detling Data --------------------------------------------------
# Detling (Maidstone Rural) is not in Defra, so we append the manual file here
# Ensure columns match before binding
message("Appending Detling (Maidstone Rural) data...")

# Convert Detling dates to POSIXct to match AURN format
Detling_APdata <- Detling_APdata %>%
  mutate(
    date = as.POSIXct(date, tz = "Europe/London"),
    date_end = as.POSIXct(date_end, tz = "Europe/London")
  )

Data_AirPollutants_RB <- bind_rows(Detling_APdata, Data_AirPollutants_RB) %>%
  arrange(site, date)

# 6. Get Site Metadata ----------------------------------------------------
Data_sites_RB <- get_saq_sites() %>%
  filter(site %in% RBsites) %>%
  select(site, site_name, latitude, longitude, elevation, site_type, site_area) %>%
  mutate(type = paste(site_area, site_type))

# 7. Merge Datasets -------------------------------------------------------
# Merge Air Quality data with local Cwood data
Data_Combined <- Data_AirPollutants_RB %>%
  left_join(CwoodSitesData_RB, by = c("date", "site")) %>%
  arrange(site, date)

# 8. Fill Missing Gaps (The "Vectorized" Way) -----------------------------
message("Filling missing time gaps...")

full_time_seq <- seq(
  from = as.POSIXct(paste(start_date, "00:00:00"), tz = "Europe/London"),
  to   = as.POSIXct(paste(end_date, "23:00:00"),   tz = "Europe/London"),
  by   = "hour"
)

Data_RB <- Data_Combined %>%
  group_by(site) %>%
  complete(date = full_time_seq) %>%
  ungroup() %>%
  left_join(Data_sites_RB, by = "site") %>%
  mutate(
    date_end = date + hours(1),
    Date = as.Date(date),
    Hour = hour(date),
    Month = month(date),
    Weekday = wday(date, week_start = 1),
    Time = format(date, "%H:%M:%S"),
    no = nox - no2
  ) %>%
  arrange(site, date)

# 9. Save Output ----------------------------------------------------------
if(!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

saveRDS(Data_RB, "data/processed/RB_AirQuality_Processed.rds")

message("Done! Processed RB data saved to 'data/processed/RB_AirQuality_Processed.rds'")