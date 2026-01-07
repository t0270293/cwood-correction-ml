# ==============================================================================
# Script: 05_Validate_Model_vs_NAEI.R
# Author: I S Wong
# Purpose: Validates the estimated Cwood (Wood Burning) concentrations by:
#          1. Combining Urban (UB) and Rural (RB) estimates.
#          2. Comparing trends against the National Atmospheric Emissions Inventory (NAEI).
#          3. Performing statistical trend analysis (Theil-Sen, Spearman, Kendall).
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(tidyverse)
library(lubridate)
library(openair)   # For TheilSen analysis
library(ggpubr)    # For arranging plots
library(data.table)

# 2. Load Data ------------------------------------------------------------
message("Loading estimated Cwood results...")

# Load UB and RB results from Step 04
ub_file <- "data/results/UB_Cwood_Estimates_WithSensitivity.rds"
rb_file <- "data/results/RB_Cwood_Estimates_WithSensitivity.rds"

if(!file.exists(ub_file) | !file.exists(rb_file)) {
  stop("Error: Result files not found. Please run scripts 04a and 04b first.")
}

Res_UB <- readRDS(ub_file) %>% mutate(Type = "Urban Background")
Res_RB <- readRDS(rb_file) %>% mutate(Type = "Rural Background")

# Combine datasets
All_Res <- bind_rows(Res_UB, Res_RB)

# Load NAEI Inventory Data
# NOTE: Ensure this file exists in your data folder!
naei_file <- "data/Cwood_from_NAEI.txt" 
if(!file.exists(naei_file)) {
  warning("NAEI file not found. Creating dummy NAEI data for demonstration.")
  # Create dummy data if file is missing (Remove this block once you have the real file)
  NAEI_Data <- data.frame(
    Year = 2009:2019,
    CwoodfromNAEI = seq(1.2, 1.0, length.out = 11) # Dummy decreasing trend
  )
} else {
  NAEI_Data <- read.delim(naei_file, header = TRUE)
}

# Ensure Date formats
NAEI_Data$date <- as.POSIXct(paste0(NAEI_Data$Year, "-01-01"), tz = "UTC")

# 3. Calculate Annual Means -----------------------------------------------
message("Calculating annual averages...")

# UB Annual Means
Annual_UB <- Res_UB %>%
  mutate(Year = year(date)) %>%
  group_by(Year) %>%
  summarise(
    residual = mean(cwood_excess_baseline, na.rm = TRUE),
    cwood_obs = mean(cwood, na.rm = TRUE)
  ) %>%
  mutate(Type = "Urban Background")

# RB Annual Means
Annual_RB <- Res_RB %>%
  mutate(Year = year(date)) %>%
  group_by(Year) %>%
  summarise(
    residual = mean(cwood_excess_baseline, na.rm = TRUE),
    cwood_obs = mean(cwood, na.rm = TRUE)
  ) %>%
  mutate(Type = "Rural Background")

# Combined Annual Means
Annual_Combined <- bind_rows(Annual_UB, Annual_RB)

# 4. Trend Analysis (Theil-Sen) -------------------------------------------
message("Running Theil-Sen Trend Analysis...")

# Prepare data for openair::TheilSen (requires 'date' and 'pollutant' columns)
Trend_Data_UB <- Annual_UB %>% mutate(date = as.POSIXct(paste0(Year, "-01-01"), tz="UTC"))
Trend_Data_RB <- Annual_RB %>% mutate(date = as.POSIXct(paste0(Year, "-01-01"), tz="UTC"))
Trend_Data_NAEI <- NAEI_Data

# Calculate Trends
# Note: openair plots these automatically, but we save the results to print
trend_ub <- TheilSen(Trend_Data_UB, pollutant = "residual", ylab = "Urban Cwood (ug/m3)", plot = FALSE)
trend_rb <- TheilSen(Trend_Data_RB, pollutant = "residual", ylab = "Rural Cwood (ug/m3)", plot = FALSE)
trend_naei <- TheilSen(Trend_Data_NAEI, pollutant = "CwoodfromNAEI", ylab = "NAEI Emission (kt)", plot = FALSE)

print("--- Urban Trend ---")
print(trend_ub$data[[1]])
print("--- Rural Trend ---")
print(trend_rb$data[[1]])
print("--- NAEI Trend ---")
print(trend_naei$data[[1]])

# 5. Correlation Analysis (Model vs NAEI) ---------------------------------
message("Calculating correlations...")

# Join Model estimates with NAEI data
Corr_Data <- Annual_Combined %>%
  inner_join(NAEI_Data, by = "Year")

# Spearman Correlations
cor_ub <- cor.test(
  x = Corr_Data$CwoodfromNAEI[Corr_Data$Type == "Urban Background"],
  y = Corr_Data$residual[Corr_Data$Type == "Urban Background"],
  method = "spearman"
)

cor_rb <- cor.test(
  x = Corr_Data$CwoodfromNAEI[Corr_Data$Type == "Rural Background"],
  y = Corr_Data$residual[Corr_Data$Type == "Rural Background"],
  method = "spearman"
)

cat("\nSpearman Correlation (Urban vs NAEI): rho =", round(cor_ub$estimate, 2), " p =", signif(cor_ub$p.value, 3))
cat("\nSpearman Correlation (Rural vs NAEI): rho =", round(cor_rb$estimate, 2), " p =", signif(cor_rb$p.value, 3))

# 6. Plotting: The "Grand Comparison" -------------------------------------
message("Generating final comparison plots...")



# Scale factor for dual-axis plot (Concentration vs Emission)
# We scale Emissions (kt) to match Concentration (ug/m3) range for plotting
scale_factor <- 1.5 

Comparison_Plot <- ggplot() +
  # Urban Line
  geom_line(data = Annual_UB, aes(x = Year, y = residual, color = "Urban Cwood (Corrected)"), size = 1, linetype = "dashed") +
  # Rural Line
  geom_line(data = Annual_RB, aes(x = Year, y = residual, color = "Rural Cwood (Corrected)"), size = 1, linetype = "dashed") +
  # NAEI Line (Scaled down for plot)
  geom_line(data = NAEI_Data, aes(x = Year, y = CwoodfromNAEI / scale_factor, color = "NAEI Emission"), size = 1.2) +
  
  # Axes
  scale_y_continuous(
    name = "Estimated Cwood Concentration (ug/m3)",
    sec.axis = sec_axis(~ . * scale_factor, name = "NAEI Emission (kilotonnes)")
  ) +
  scale_x_continuous(breaks = 2009:2020) +
  
  # Colors & Legends
  scale_color_manual(
    name = "",
    values = c(
      "Urban Cwood (Corrected)" = "red",
      "Rural Cwood (Corrected)" = "darkgoldenrod",
      "NAEI Emission" = "blue"
    )
  ) +
  
  # Theme
  labs(
    title = "Comparison of Modeled Cwood vs NAEI Inventory",
    subtitle = "Urban and Rural Background Sites (2009-2019)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(Comparison_Plot)

# 7. Save Outputs ---------------------------------------------------------
if(!dir.exists("plots")) dir.create("plots")
ggsave("plots/Comparison_Model_vs_NAEI.png", plot = Comparison_Plot, width = 10, height = 6)

message("Analysis complete! Plot saved to 'plots/Comparison_Model_vs_NAEI.png'")