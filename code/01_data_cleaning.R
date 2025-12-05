# ==============================================================================
# Subway Ridership and Crime in New York City
# Script 1: Data Loading and Cleaning
# Author: Alberto Miranda
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(data.table)
library(janitor)
library(lubridate)
library(arrow)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Load and Clean Crime Data (NYPD Complaints)
# ------------------------------------------------------------------------------

# Columns to keep from raw data
keep_crime <- c("CMPLNT_FR_DT", "OFNS_DESC", "BORO_NM",
                "Latitude", "Longitude", "LAW_CAT_CD",
                "PD_DESC", "CRM_ATPT_CPTD_CD", "PREM_TYP_DESC")

# Load raw crime data
crime_raw <- fread(
  "data_raw/NYPD_Complaint_Data_Historic_20250910.csv",
  select = keep_crime,
  showProgress = TRUE
) |> clean_names()

# Drop rows with missing coordinates
crime_raw <- crime_raw[!is.na(latitude) & !is.na(longitude)]

# Parse dates and extract year
crime_raw[, occur_date := suppressWarnings(mdy(cmplnt_fr_dt))]
crime_raw[, year := year(occur_date)]

# Filter to 2020-2024 and drop missing dates
crime_clean <- crime_raw[!is.na(occur_date) & year >= 2020 & year <= 2024]

# Save cleaned crime data
write_parquet(crime_clean, "data_clean/crime_clean_2020_2024.parquet")

# Free memory
rm(crime_raw)
gc()

# ------------------------------------------------------------------------------
# 2. Load and Clean Ridership Data (MTA Subway)
# ------------------------------------------------------------------------------

# Columns to keep from raw data
keep_rider <- c("transit_timestamp", "transit_mode",
                "station_complex_id", "station_complex", "borough",
                "ridership", "latitude", "longitude",
                "payment_method", "fare_class_category", "transfers")

# Load raw ridership data
rider_raw <- fread(
  "data_raw/wujg-7c2s_version_63926.csv",
  select = keep_rider,
  showProgress = TRUE
) |> clean_names()

# Filter to subway only and 2020-2024
rider_clean <- rider_raw[
  transit_mode == "subway" &
    !is.na(year(transit_timestamp)) &
    year(transit_timestamp) >= 2020 &
    year(transit_timestamp) <= 2024,
  .(station_complex_id, station_complex, borough,
    latitude, longitude, transit_timestamp, ridership)
]

# Save cleaned ridership data
write_parquet(rider_clean, "data_clean/ridership_clean_2020_2024.parquet")

# Free memory
rm(rider_raw)
gc()

# ------------------------------------------------------------------------------
# 3. Aggregate Ridership by Station and Year
# ------------------------------------------------------------------------------

rider_annual <- rider_clean[, .(
  annual_ridership = sum(ridership, na.rm = TRUE)
), by = .(
  station_complex_id,
  station_complex,
  borough,
  latitude,
  longitude,
  year = year(transit_timestamp)
)]

# Save annual ridership
write_parquet(rider_annual, "data_clean/ridership_annual_2020_2024.parquet")

# Free memory
rm(rider_clean)
gc()

# ------------------------------------------------------------------------------
# 4. Quick Visualization Check
# ------------------------------------------------------------------------------

# Reload crime data for plotting
crime_clean <- read_parquet("data_clean/crime_clean_2020_2024.parquet")

# Sample for visualization
set.seed(123)
crime_sample <- crime_clean[sample(.N, min(.N, 50000))]

ggplot(crime_sample, aes(x = longitude, y = latitude, color = boro_nm)) +
  geom_point(alpha = 0.3, size = 0.5) +
  labs(title = "NYC Crime Incidents (Sample, 2020-2024)",
       x = "Longitude", y = "Latitude", color = "Borough") +
  theme_minimal()

# ==============================================================================
# Output Files:
#   - data_clean/crime_clean_2020_2024.parquet
#   - data_clean/ridership_clean_2020_2024.parquet
#   - data_clean/ridership_annual_2020_2024.parquet
# ==============================================================================