# ==============================================================================
# Script: 06_Plot_TimeSeries.R
# Author: I S Wong
# Purpose: Generates time-series plots, ratio analysis (Cwood/PM), and 
#          diurnal profiles for Urban and Rural background sites.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(tidyverse)
library(lubridate)
library(openair)
library(ggpubr)
library(scales)

# 2. Load Data ------------------------------------------------------------
message("Loading results from Step 04...")

ub_file <- "data/results/UB_Cwood_Estimates_WithSensitivity.rds"
rb_file <- "data/results/RB_Cwood_Estimates_WithSensitivity.rds"

if(!file.exists(ub_file) | !file.exists(rb_file)) {
  stop("Error: Result files not found. Run scripts 04a and 04b first.")
}

# Load and standardize column names for plotting
# Mapping: 'residual' in your old script is 'cwood_excess_baseline' here
Res_UB <- readRDS(ub_file) %>% 
  mutate(residual = cwood_excess_baseline, Type = "Urban Background")

Res_RB <- readRDS(rb_file) %>% 
  mutate(residual = cwood_excess_baseline, Type = "Rural Background")

# 3. Helper Function: Calculate Ratios & Aggregates -----------------------
process_ratios_and_aggregate <- function(df) {
  
  # A. Calculate Hourly Ratios
  df <- df %>%
    mutate(
      cwood_pm10_ratio = residual / pm10,
      cwood_pm25_ratio = residual / pm2.5
    ) %>%
    # Remove infinite values (div by zero)
    mutate(
      cwood_pm10_ratio = ifelse(is.infinite(cwood_pm10_ratio), NA, cwood_pm10_ratio),
      cwood_pm25_ratio = ifelse(is.infinite(cwood_pm25_ratio), NA, cwood_pm25_ratio)
    )
  
  # B. Daily Aggregation
  df_daily <- df %>%
    mutate(date_day = as.Date(date)) %>%
    group_by(Type, site, date_day) %>%
    summarise(across(c(residual, cwood, cwood_pm10_ratio, cwood_pm25_ratio, no2), mean, na.rm=TRUE), .groups="drop")
  
  # C. Monthly Aggregation
  df_monthly <- df_daily %>%
    mutate(date_month = floor_date(date_day, "month")) %>%
    group_by(Type, site, date_month) %>%
    summarise(across(c(residual, cwood, cwood_pm10_ratio, cwood_pm25_ratio, no2), mean, na.rm=TRUE), .groups="drop")
  
  # D. Yearly Aggregation
  df_yearly <- df_monthly %>%
    mutate(date_year = floor_date(date_month, "year")) %>%
    group_by(Type, site, date_year) %>%
    summarise(across(c(residual, cwood, cwood_pm10_ratio, cwood_pm25_ratio, no2), mean, na.rm=TRUE), .groups="drop")
  
  list(hourly = df, daily = df_daily, monthly = df_monthly, yearly = df_yearly)
}

# Process both datasets
Data_UB <- process_ratios_and_aggregate(Res_UB)
Data_RB <- process_ratios_and_aggregate(Res_RB)

# Create output folder
if(!dir.exists("plots")) dir.create("plots")

# 4. Plot 1: Monthly Ratios (Combined UB & RB) ----------------------------
message("Generating Ratio Plots...")

# Combine Monthly Data for plotting
Monthly_Combined <- bind_rows(
  Data_UB$monthly %>% group_by(Type, date_month) %>% summarise(ratio_pm10 = mean(cwood_pm10_ratio, na.rm=TRUE), ratio_pm25 = mean(cwood_pm25_ratio, na.rm=TRUE)),
  Data_RB$monthly %>% group_by(Type, date_month) %>% summarise(ratio_pm10 = mean(cwood_pm10_ratio, na.rm=TRUE), ratio_pm25 = mean(cwood_pm25_ratio, na.rm=TRUE))
)

p1 <- ggplot(Monthly_Combined, aes(x = date_month, y = ratio_pm25, color = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Urban Background" = "red", "Rural Background" = "darkgoldenrod")) +
  labs(title = "Monthly Mean Cwood/PM2.5 Ratio", x = "Year", y = "Ratio") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("plots/01_Monthly_Ratio_Cwood_PM25.png", p1, width = 8, height = 5)


# 5. Plot 2: Diurnal Profiles (OpenAir) -----------------------------------
message("Generating Diurnal Profiles...")

# Prepare data for openair (needs 'date' column)
UB_OpenAir <- Data_UB$hourly %>% select(date, cwood) 
RB_OpenAir <- Data_RB$hourly %>% select(date, cwood)

png("plots/02_Diurnal_Profile_UB.png", width = 800, height = 500)
timeVariation(UB_OpenAir, pollutant = "cwood", main = "Diurnal Profile: Urban Background", cols = "red")
dev.off()

png("plots/03_Diurnal_Profile_RB.png", width = 800, height = 500)
timeVariation(RB_OpenAir, pollutant = "cwood", main = "Diurnal Profile: Rural Background", cols = "gold")
dev.off()


# 6. Plot 3: Yearly Averaged Time Series (The Multi-Line Plot) ------------
message("Generating Yearly Site Trends...")

# Filter for the 6 specific Urban sites you highlighted
target_sites_ub <- c("gb0567a", "gb0851a", "gb0580a", "gb1028a", "gb0620a", "gb0995a")
labels_ub <- c("Belfast", "Birmingham", "Cardiff", "Glasgow", "London NK", "Norwich")

Plot_Data_UB <- Data_UB$yearly %>% filter(site %in% target_sites_ub)

p3 <- ggplot(Plot_Data_UB, aes(x = date_year, y = cwood, color = site)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_hline(yintercept = 0, color = "grey") +
  scale_color_brewer(palette = "Dark2", labels = paste(target_sites_ub, labels_ub, sep=" - ")) +
  labs(title = "Yearly Averaged Cwood Concentration (Urban)", 
       y = expression("Concentration ("*mu*g/m^3*")"), x = "") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("plots/04_Yearly_Trends_UB.png", p3, width = 10, height = 6)


# 7. Plot 4: Representative Hourly Time Series (2012 & 2016) --------------
message("Generating Representative Time Series...")

# Urban: Year 2012
Data_UB_2012 <- Data_UB$hourly %>% 
  filter(year(date) == 2012, site %in% target_sites_ub)

p4 <- ggplot(Data_UB_2012, aes(x = date, y = cwood, color = site)) +
  geom_line(alpha = 0.6) +
  labs(title = "Hourly Cwood Concentration (2012) - Urban Sites", 
       y = expression("Concentration ("*mu*g/m^3*")"), x = "") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("plots/05_Representative_Series_2012_UB.png", p4, width = 10, height = 5)

# Rural: Year 2016 (Sites: Auchencorth, Detling, Chilbolton)
target_sites_rb <- c("gb0048r", "gb0886a", "gb1055r")
Data_RB_2016 <- Data_RB$hourly %>% 
  filter(year(date) == 2016, site %in% target_sites_rb)

p5 <- ggplot(Data_RB_2016, aes(x = date, y = cwood, color = site)) +
  geom_line(alpha = 0.6) +
  labs(title = "Hourly Cwood Concentration (2016) - Rural Sites", 
       y = expression("Concentration ("*mu*g/m^3*")"), x = "") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("plots/06_Representative_Series_2016_RB.png", p5, width = 10, height = 5)


# 8. Plot 5: NO2 Comparison (4 Sites) -------------------------------------
message("Generating NO2 Comparison...")

# Combine UB and RB hourly data for NO2
NO2_Data <- bind_rows(
  Data_UB$hourly %>% select(site, date, no2),
  Data_RB$hourly %>% select(site, date, no2)
) %>%
  filter(site %in% c("gb0620a", "gb0048r", "gb0886a", "gb1055r"))

p6 <- ggplot(NO2_Data, aes(x = date, y = no2, color = site)) +
  geom_line(alpha = 0.7) +
  labs(title = "Hourly NO2 Concentration (Selected Sites)",
       y = expression("NO"[2] ~ " Concentration ("*mu*g/m^3*")"), x = "") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("plots/07_NO2_Comparison.png", p6, width = 12, height = 6)

message("All plots generated successfully in 'plots/' folder.")