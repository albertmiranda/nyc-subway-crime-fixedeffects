# ==============================================================================
# Subway Ridership and Crime in New York City
# Script 4: Regression Modeling and Spatial Diagnostics
# Author: Alberto Miranda
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(data.table)
library(arrow)
library(fixest)
library(modelsummary)
library(sf)
library(spdep)
library(spatialreg)
library(broom)

# Enable parallel processing
setDTthreads(parallel::detectCores())
setFixest_nthreads(0)

dir.create("results", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Load and Prepare Panel Dataset
# ------------------------------------------------------------------------------

panel <- read_parquet("data_clean/egohood_panel_flat_2020_2024.parquet") |> as.data.table()

# Data types and missing values
panel[, egohood_id := as.character(egohood_id)]
panel[, year := as.integer(year)]
panel[is.na(has_station), has_station := FALSE]
panel[is.na(annual_ridership) | annual_ridership < 0, annual_ridership := 0]

# Log transformations
if (!"log_crime" %in% names(panel)) panel[, log_crime := log1p(crime_count)]
if (!"log_ridership" %in% names(panel)) panel[, log_ridership := log1p(annual_ridership)]

# Create lagged ridership by egohood
setorder(panel, egohood_id, year)
panel[, lag_log_rider := shift(log_ridership, 1L), by = egohood_id]

# Define control variables
control_vars <- c("log_popdens", "log_income", "pct_white", "pct_black", "pct_hispanic")

# Create modeling subsets
panel_base <- panel[!is.na(log_crime) & !is.na(log_ridership)]
panel_ctl <- panel_base[complete.cases(panel_base[, ..control_vars])]

# ------------------------------------------------------------------------------
# 2. Estimate Regression Models
# ------------------------------------------------------------------------------

# Model 1: Pooled OLS (baseline)
m_ols <- feols(
  log_crime ~ log_ridership + has_station + i(year),
  data = panel_base,
  cluster = ~egohood_id
)

# Model 2: Pooled OLS with controls
m_ols_ctl <- feols(
  log_crime ~ log_ridership + has_station + log_popdens + log_income +
    pct_white + pct_black + pct_hispanic + i(year),
  data = panel_ctl,
  cluster = ~egohood_id
)

# Model 3: Two-way fixed effects (egohood + year)
m_fe <- feols(
  log_crime ~ log_ridership + has_station | egohood_id + year,
  data = panel_ctl,
  cluster = ~egohood_id
)

# Model 4: Fixed effects with lagged ridership
m_fe_lag <- feols(
  log_crime ~ log_ridership + lag_log_rider + has_station | egohood_id + year,
  data = panel_ctl[!is.na(lag_log_rider)],
  cluster = ~egohood_id
)

# Display results
modelsummary(
  list(
    "OLS (base)" = m_ols,
    "OLS + Ctrls" = m_ols_ctl,
    "FE 2-way + Ctrls" = m_fe,
    "FE + Lag + Ctrls" = m_fe_lag
  ),
  estimate = "{estimate} ({std.error})",
  statistic = NULL,
  gof_omit = "IC|Log|Adj|Within|Theta"
)

# ------------------------------------------------------------------------------
# 3. Spatial Autocorrelation of Fixed Effects Residuals (2024)
# ------------------------------------------------------------------------------

# Extract residuals
res_tbl <- broom::augment(m_fe)
setDT(res_tbl)
setnames(res_tbl, ".resid", "fe_resid")

# Load geometry and merge residuals for 2024
gdf <- st_read("data_clean/egohood_panel_2020_2024.gpkg", quiet = TRUE)
gdf <- st_make_valid(gdf)

gdf_2024 <- merge(
  gdf[gdf$year == 2024, c("egohood_id", "year", "geom")],
  unique(res_tbl[, .(egohood_id, year, fe_resid)]),
  by = c("egohood_id", "year"),
  all.x = FALSE, all.y = FALSE
)

# Drop empty geometries and build spatial weights
gdf_2024 <- gdf_2024[!st_is_empty(gdf_2024), ]
nb <- poly2nb(gdf_2024)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Moran's I test on residuals
mi <- moran.test(gdf_2024$fe_resid, lw, zero.policy = TRUE, na.action = na.omit)
print(mi)

# ------------------------------------------------------------------------------
# 4. Spatial Lag Model (2024 Cross-Section)
# ------------------------------------------------------------------------------

# Merge predictors for 2024
x24 <- unique(panel_ctl[year == 2024, .(egohood_id, log_ridership, has_station, crime_count)])
gdf_sar <- merge(gdf_2024, x24, by = "egohood_id", all.x = TRUE)
gdf_sar <- gdf_sar[is.finite(gdf_sar$log_ridership) & !is.na(gdf_sar$crime_count), ]

# Estimate spatial lag model
sar_fit <- lagsarlm(
  log1p(crime_count) ~ log_ridership + has_station,
  data = gdf_sar,
  listw = lw,
  zero.policy = TRUE,
  method = "eigen"
)

print(summary(sar_fit))

# ------------------------------------------------------------------------------
# 5. Save Models and Results
# ------------------------------------------------------------------------------

# Save model objects
saveRDS(m_ols, "results/m_ols.rds")
saveRDS(m_ols_ctl, "results/m_ols_ctl.rds")
saveRDS(m_fe, "results/m_fe_twoway.rds")
saveRDS(m_fe_lag, "results/m_fe_lag.rds")
saveRDS(sar_fit, "results/m_sar_2024.rds")

# Export summary table
msum <- modelsummary(
  list(
    "OLS (base)" = m_ols,
    "OLS + Ctrls" = m_ols_ctl,
    "FE 2-way" = m_fe,
    "FE + Lag" = m_fe_lag
  ),
  output = "data.frame",
  estimate = "{estimate}",
  statistic = "{std.error}"
)
fwrite(msum, "results/model_table.csv")

# ==============================================================================
# Output Files:
#   - results/m_ols.rds
#   - results/m_ols_ctl.rds
#   - results/m_fe_twoway.rds
#   - results/m_fe_lag.rds
#   - results/m_sar_2024.rds
#   - results/model_table.csv
# ==============================================================================
```