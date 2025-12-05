# ==============================================================================
# Subway Ridership and Crime in New York City
# Script 2: Egohood Construction and Panel Building
# Author: Alberto Miranda
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(data.table)
library(sf)
library(units)
library(tigris)
library(arrow)
library(tidycensus)
library(dplyr)

options(tigris_use_cache = TRUE)

# ------------------------------------------------------------------------------
# 1. Load Cleaned Data from Script 1
# ------------------------------------------------------------------------------

crime_clean <- read_parquet("data_clean/crime_clean_2020_2024.parquet")
rider_annual <- read_parquet("data_clean/ridership_annual_2020_2024.parquet")

# ------------------------------------------------------------------------------
# 2. Download Census Blocks and Create Egohoods
# ------------------------------------------------------------------------------

# Download 2020 Census Blocks for NYC (5 boroughs)
nyc_blocks <- blocks(
  state = "NY",
  county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
  year = 2020
) |> st_transform(4326)

# Convert block polygons to centroids
block_centroids <- st_centroid(nyc_blocks)

# Create quarter-mile (~402m) buffer around each centroid
egohoods_full <- block_centroids |>
  st_transform(2263) |>
  st_buffer(dist = set_units(402, "m")) |>
  st_transform(4326)

# Add unique ID from Census Block GEOID
egohoods_full$egohood_id <- nyc_blocks$GEOID20

# Save egohoods
st_write(egohoods_full, "data_clean/egohoods_full_402m.geojson", delete_dsn = TRUE)

# ------------------------------------------------------------------------------
# 3. Load or Download ACS Demographics
# ------------------------------------------------------------------------------

census_file <- "data_clean/acs_bg_2020_nyc.geojson"

if (file.exists(census_file)) {
  acs_bg <- st_read(census_file, quiet = TRUE)
} else {
  vars <- c(
    population = "B01003_001E",
    median_income = "B19013_001E",
    poverty_total = "B17001_001E",
    poverty_below = "B17001_002E",
    white = "B03002_003E",
    black = "B03002_004E",
    hispanic = "B03002_012E"
  )
  acs_bg <- get_acs(
    geography = "block group",
    variables = vars,
    state = "NY",
    year = 2020,
    geometry = TRUE,
    output = "wide"
  ) |> st_transform(4326)
  st_write(acs_bg, census_file, delete_dsn = TRUE)
}

# Harmonize column names and derive percentages
name_or <- function(n) {
  if (paste0(n, "E") %in% names(acs_bg)) paste0(n, "E") else n
}

acs_bg <- acs_bg |>
  mutate(
    population = suppressWarnings(as.numeric(.data[[name_or("population")]])),
    median_income = suppressWarnings(as.numeric(.data[[name_or("median_income")]])),
    poverty_total = suppressWarnings(as.numeric(.data[[name_or("poverty_total")]])),
    poverty_below = suppressWarnings(as.numeric(.data[[name_or("poverty_below")]])),
    white = suppressWarnings(as.numeric(.data[[name_or("white")]])),
    black = suppressWarnings(as.numeric(.data[[name_or("black")]])),
    hispanic = suppressWarnings(as.numeric(.data[[name_or("hispanic")]]))
  ) |>
  mutate(
    pct_poverty = if_else(poverty_total > 0, poverty_below / poverty_total, NA_real_),
    pct_white = if_else(population > 0, white / population, NA_real_),
    pct_black = if_else(population > 0, black / population, NA_real_),
    pct_hispanic = if_else(population > 0, hispanic / population, NA_real_)
  ) |>
  select(GEOID, population, median_income, pct_poverty, pct_white, pct_black, pct_hispanic, geometry)

# ------------------------------------------------------------------------------
# 4. Join Demographics to Egohoods (Centroid-in-Buffer Method)
# ------------------------------------------------------------------------------

sf_use_s2(FALSE)

# Get centroids of ACS block groups
acs_centroids <- st_centroid(acs_bg) |>
  select(GEOID, population, median_income, pct_poverty, pct_white, pct_black, pct_hispanic)

# Assign centroids to containing egohoods
cent_join <- st_join(acs_centroids, egohoods_full["egohood_id"], join = st_within, left = FALSE)

# Aggregate with population weights
cent_dt <- as.data.table(st_drop_geometry(cent_join))
cent_dt[, population := fifelse(is.finite(population), population, 0)]

egohoods_demo <- cent_dt[, .(
  population = sum(population, na.rm = TRUE),
  median_income = weighted.mean(median_income, w = population, na.rm = TRUE),
  pct_poverty = weighted.mean(pct_poverty, w = population, na.rm = TRUE),
  pct_white = weighted.mean(pct_white, w = population, na.rm = TRUE),
  pct_black = weighted.mean(pct_black, w = population, na.rm = TRUE),
  pct_hispanic = weighted.mean(pct_hispanic, w = population, na.rm = TRUE)
), by = egohood_id]

# Merge demographics back to egohoods
egohoods_full <- left_join(egohoods_full, egohoods_demo, by = "egohood_id")
st_write(egohoods_full, "data_clean/egohoods_full_402m_enriched.geojson", delete_dsn = TRUE)

# ------------------------------------------------------------------------------
# 5. Spatial Join: Crime and Ridership to Egohoods
# ------------------------------------------------------------------------------

sf_use_s2(TRUE)

# Reload enriched egohoods
egohoods_full <- st_read("data_clean/egohoods_full_402m_enriched.geojson", quiet = TRUE)

# Convert crime and ridership to spatial points
crime_sf <- st_as_sf(crime_clean, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
rider_sf <- st_as_sf(rider_annual, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# Join crime points to egohoods and aggregate
crime_dt <- st_join(crime_sf, egohoods_full["egohood_id"], join = st_intersects) |>
  st_drop_geometry() |>
  as.data.table()

crime_agg <- crime_dt[, .(
  crime_count = .N,
  crime_felony = sum(law_cat_cd == "FELONY", na.rm = TRUE),
  crime_misdemeanor = sum(law_cat_cd == "MISDEMEANOR", na.rm = TRUE),
  crime_violation = sum(law_cat_cd == "VIOLATION", na.rm = TRUE)
), by = .(egohood_id, year)]
crime_agg[, major_crime_share := fifelse(crime_count > 0, crime_felony / crime_count, NA_real_)]

# Join ridership points to egohoods and aggregate
rider_dt <- st_join(rider_sf, egohoods_full["egohood_id"], join = st_intersects) |>
  st_drop_geometry() |>
  as.data.table()

rider_agg <- rider_dt[, .(
  station_count = uniqueN(station_complex_id),
  annual_ridership = sum(annual_ridership, na.rm = TRUE)
), by = .(egohood_id, year)]
rider_agg[, has_station := station_count > 0]

# ------------------------------------------------------------------------------
# 6. Build Panel Dataset
# ------------------------------------------------------------------------------

# Merge crime and ridership aggregates
panel_core <- merge(crime_agg, rider_agg, by = c("egohood_id", "year"), all = TRUE)

# Add demographics
demo_cols <- c("egohood_id", "population", "median_income", "pct_poverty",
               "pct_white", "pct_black", "pct_hispanic")
panel_core <- panel_core |>
  left_join(egohoods_full |> st_drop_geometry() |> select(all_of(demo_cols)), by = "egohood_id")

# Attach geometry
egohood_panel_sf <- egohoods_full[, c("egohood_id", "geometry")] |>
  left_join(panel_core, by = "egohood_id")

# ------------------------------------------------------------------------------
# 7. Fill Missing Egohood-Years with Zeros
# ------------------------------------------------------------------------------

yrs <- 2020:2024
full_ix <- CJ(egohood_id = unique(egohood_panel_sf$egohood_id), year = yrs)
ep_dt <- as.data.table(st_drop_geometry(egohood_panel_sf))
ep_full <- merge(full_ix, ep_dt, by = c("egohood_id", "year"), all.x = TRUE)

# Fill zeros for count columns
zero_cols <- c("crime_count", "crime_felony", "crime_misdemeanor",
               "crime_violation", "annual_ridership", "station_count")
for (cc in zero_cols) {
  if (!cc %in% names(ep_full)) ep_full[, (cc) := 0L]
  ep_full[is.na(get(cc)), (cc) := 0L]
}

# Recalculate flags
if (!"major_crime_share" %in% names(ep_full)) ep_full[, major_crime_share := NA_real_]
ep_full[is.na(major_crime_share) & crime_count == 0, major_crime_share := NA_real_]
ep_full[, has_station := station_count > 0]

# Reattach geometry
geom_sf <- egohoods_full[, c("egohood_id", "geometry")]
egohood_panel_sf <- merge(geom_sf, ep_full, by = "egohood_id", all.y = TRUE)

# ------------------------------------------------------------------------------
# 8. Save Final Panel Outputs
# ------------------------------------------------------------------------------

# GeoPackage (with geometry)
write_sf(egohood_panel_sf, "data_clean/egohood_panel_2020_2024.gpkg", delete_dsn = TRUE)

# GeoJSON (for visualization)
write_sf(egohood_panel_sf, "data_clean/egohood_panel_2020_2024.geojson", delete_dsn = TRUE)

# Parquet (flat, no geometry, for modeling)
write_parquet(st_drop_geometry(egohood_panel_sf), "data_clean/egohood_panel_flat_2020_2024.parquet")

# ==============================================================================
# Output Files:
#   - data_clean/egohoods_full_402m.geojson
#   - data_clean/egohoods_full_402m_enriched.geojson
#   - data_clean/egohood_panel_2020_2024.gpkg
#   - data_clean/egohood_panel_2020_2024.geojson
#   - data_clean/egohood_panel_flat_2020_2024.parquet
# ==============================================================================