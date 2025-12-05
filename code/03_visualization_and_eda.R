# ==============================================================================
# Subway Ridership and Crime in New York City
# Script 3: Exploratory Analysis and Visualization
# Author: Alberto Miranda
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
library(viridis)
library(tigris)
library(spdep)
library(ggspatial)
library(arrow)
library(scales)
library(ggrepel)
library(gridExtra)
library(cowplot)
library(patchwork)
library(stringr)
library(ggplotify)

options(tigris_use_cache = TRUE)
dir.create("figs", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Load Panel Data and Attach Borough Information
# ------------------------------------------------------------------------------

egohood_panel <- st_read("data_clean/egohood_panel_2020_2024.gpkg", quiet = TRUE)

# Download borough polygons
nyc_boroughs <- counties(state = "NY", cb = TRUE, year = 2020) |>
  st_transform(4326) |>
  subset(NAME %in% c("Bronx", "Kings", "New York", "Queens", "Richmond"))

nyc_boroughs$borough <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")

# Spatial join to assign boroughs
egohood_panel <- st_join(
  egohood_panel,
  nyc_boroughs[, c("borough", "geometry")],
  join = st_intersects,
  left = TRUE
)

# ------------------------------------------------------------------------------
# 2. Figure 1: Hexagon Crime Map (2024)
# ------------------------------------------------------------------------------

ego_2024 <- egohood_panel |>
  filter(year == 2024) |>
  st_make_valid()

nyc_boundary_2263 <- ego_2024 |>
  st_transform(2263) |>
  st_union() |>
  st_make_valid()

# Create hexagon grid
hex_2263 <- st_make_grid(nyc_boundary_2263, cellsize = 700, square = FALSE) |>
  st_as_sf() |>
  mutate(hex_id = row_number())

# Assign egohoods to hexagons
centers_2263 <- ego_2024 |>
  st_transform(2263) |>
  st_point_on_surface() |>
  select(egohood_id, crime_count, annual_ridership)

hex_join <- st_join(centers_2263, hex_2263["hex_id"], join = st_intersects)

hex_sum <- hex_join |>
  st_drop_geometry() |>
  group_by(hex_id) |>
  summarise(crimes = sum(crime_count, na.rm = TRUE), .groups = "drop")

hex_map <- left_join(hex_2263, hex_sum, by = "hex_id") |>
  st_transform(3857)

# Plot
p_hex_crime <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0, alpha = 0.6) +
  geom_sf(data = hex_map, aes(fill = crimes), color = NA, alpha = 0.95) +
  scale_fill_distiller(
    palette = "OrRd", direction = 1, trans = "sqrt",
    labels = comma, na.value = NA
  ) +
  labs(fill = "Crimes") +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

ggsave("figs/fig_h1_hex_crime_2024.png", p_hex_crime, width = 7.5, height = 6.5, dpi = 900)

# ------------------------------------------------------------------------------
# 3. Figure 2: Representative Egohood Ridership (2024) with Top 5 Labels
# ------------------------------------------------------------------------------

# Load egohood polygons for 2024
ego_2024 <- st_read("data_clean/egohood_panel_2020_2024.gpkg", quiet = TRUE) |>
  filter(year == 2024) |>
  select(egohood_id) |>
  st_make_valid() |>
  st_transform(2263)

# Load station-level ridership
rider_annual <- read_parquet("data_clean/ridership_annual_2020_2024.parquet")

stations_2024 <- rider_annual |>
  filter(year == 2024, is.finite(latitude), is.finite(longitude)) |>
  group_by(station_complex_id, station_complex, borough, latitude, longitude) |>
  summarise(annual_entries = sum(annual_ridership, na.rm = TRUE), .groups = "drop") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_transform(2263)

# Map each station to nearest egohood
nearest_idx <- st_nearest_feature(stations_2024, ego_2024)
station2ego <- tibble(
  station_complex_id = stations_2024$station_complex_id,
  station_complex = stations_2024$station_complex,
  annual_entries = stations_2024$annual_entries,
  egohood_id = ego_2024$egohood_id[nearest_idx]
)

# Keep one station per egohood (highest ridership)
rep_map <- station2ego |>
  group_by(egohood_id) |>
  slice_max(order_by = annual_entries, n = 1, with_ties = FALSE) |>
  ungroup()

# Join back to geometry
rep_ego <- ego_2024 |>
  left_join(rep_map, by = "egohood_id") |>
  filter(!is.na(annual_entries)) |>
  mutate(annual_entries_m = annual_entries / 1e6) |>
  st_transform(3857)

# Base map
p_map <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0, alpha = 0.6) +
  geom_sf(data = rep_ego, aes(fill = annual_entries_m), color = "white", size = 0.08, alpha = 0.95) +
  scale_fill_distiller(
    palette = "Blues", direction = 1, trans = "sqrt",
    labels = label_number(accuracy = 0.1)
  ) +
  labs(fill = "Riders (millions)") +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "right"
  )

# Top 5 stations with labels
station_pts_3857 <- stations_2024 |> st_transform(3857)

top5_sf <- station_pts_3857 |>
  slice_max(order_by = annual_entries, n = 5) |>
  arrange(desc(annual_entries)) |>
  mutate(
    rank = row_number(),
    station_name_clean = sub("\\s*\\(.*$", "", station_complex),
    riders_m = annual_entries / 1e6
  )

top5_xy <- cbind(st_drop_geometry(top5_sf), st_coordinates(top5_sf))
names(top5_xy)[names(top5_xy) %in% c("X", "Y")] <- c("x", "y")

p_map_num <- p_map +
  geom_label_repel(
    data = top5_xy,
    aes(x = x, y = y, label = rank),
    size = 3.2,
    label.padding = unit(0.12, "lines"),
    segment.size = 0.3,
    box.padding = 0.25,
    max.overlaps = Inf,
    fill = "white"
  )

# Legend table
legend_df <- top5_sf |>
  st_drop_geometry() |>
  transmute(
    `#` = rank,
    Station = station_name_clean,
    Riders = paste0(number(riders_m, accuracy = 0.1), " M")
  )

tbl_grob <- tableGrob(
  as.data.frame(legend_df),
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 9,
    core = list(fg_params = list(hjust = 0, x = 0.02)),
    colhead = list(fg_params = list(fontface = 2))
  )
)

final_fig2 <- ggdraw(p_map_num) +
  draw_grob(tbl_grob, x = 0.72, y = 0.02, width = 0.26, height = 0.36)

ggsave("figs/fig2_rep_egohood_riders_2024_labeled_top5_nums.png", final_fig2,
       width = 7.5, height = 6.5, dpi = 600)

# ------------------------------------------------------------------------------
# 4. Figure 3: Combined Crime Map with Top 10 Stations Table
# ------------------------------------------------------------------------------

# Reload data for figure 3
ego_2024 <- st_read("data_clean/egohood_panel_2020_2024.gpkg", quiet = TRUE) |>
  filter(year == 2024) |>
  st_make_valid()

# Recreate hex grid
nyc_boundary_2263 <- ego_2024 |>
  st_transform(2263) |>
  st_union() |>
  st_make_valid()

hex_2263 <- st_make_grid(nyc_boundary_2263, cellsize = 700, square = FALSE) |>
  st_as_sf() |>
  mutate(hex_id = row_number())

centers_2263 <- ego_2024 |>
  st_transform(2263) |>
  st_point_on_surface() |>
  select(egohood_id, crime_count)

hex_join <- st_join(centers_2263, hex_2263["hex_id"], join = st_intersects)
hex_sum <- hex_join |>
  st_drop_geometry() |>
  group_by(hex_id) |>
  summarise(crimes = sum(crime_count, na.rm = TRUE), .groups = "drop")

hex_map_3857 <- left_join(hex_2263, hex_sum, by = "hex_id") |>
  st_transform(3857)

# Station points
stations_3857 <- rider_annual |>
  filter(year == 2024, is.finite(latitude), is.finite(longitude)) |>
  group_by(station_complex_id, station_complex, latitude, longitude) |>
  summarise(annual_entries = sum(annual_ridership, na.rm = TRUE), .groups = "drop") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_transform(3857) |>
  mutate(
    station_clean = str_trim(str_replace(station_complex, "\\s*\\(.*$", "")),
    riders_m = annual_entries / 1e6
  )

top10_3857 <- stations_3857 |>
  arrange(desc(annual_entries)) |>
  slice(1:10) |>
  mutate(rank = row_number())

# Zoom bounds
bb <- st_bbox(top10_3857)
pad_x <- 0.30 * (bb["xmax"] - bb["xmin"])
pad_y <- 0.30 * (bb["ymax"] - bb["ymin"])
xlim_zoom <- c(bb["xmin"] - pad_x, bb["xmax"] + pad_x)
ylim_zoom <- c(bb["ymin"] - pad_y, bb["ymax"] + pad_y)

# Map with legend
max_cr <- max(hex_map_3857$crimes, na.rm = TRUE)
brks <- pretty(c(0, max_cr), n = 7)

p_map_legend <- ggplot() +
  annotation_map_tile(type = "osm", zoomin = 0, alpha = 1) +
  geom_sf(data = hex_map_3857, aes(fill = crimes), color = NA, alpha = 0.65) +
  geom_sf(data = stations_3857, shape = 21, size = 1.1, color = "white", fill = NA, alpha = 0.7) +
  geom_sf(data = top10_3857, shape = 21, size = 2.2, color = "black", fill = "white") +
  geom_label_repel(
    data = top10_3857,
    aes(label = rank, geometry = geometry),
    stat = "sf_coordinates",
    seed = 123,
    size = 3.4,
    label.size = 0.25,
    label.padding = unit(2, "pt"),
    fill = "white",
    min.segment.length = 0,
    segment.size = 0.35,
    max.overlaps = Inf
  ) +
  scale_fill_distiller(
    palette = "OrRd", direction = 1, trans = "sqrt",
    breaks = brks, labels = label_comma(), name = "Crimes"
  ) +
  coord_sf(datum = NA, xlim = xlim_zoom, ylim = ylim_zoom, expand = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    label.position = "bottom",
    barheight = unit(3, "mm"),
    barwidth = unit(110, "mm")
  ))

p_leg <- as.ggplot(get_legend(p_map_legend))
p_map <- p_map_legend + theme(legend.position = "none")

# Top 10 table
top10_with_cr <- st_join(top10_3857, hex_map_3857[, c("hex_id", "crimes")]) |>
  st_drop_geometry()

legend_df <- top10_with_cr |>
  transmute(
    Rank = rank,
    Station = station_clean,
    Riders = paste0(round(riders_m, 1), " M"),
    Crimes = formatC(crimes, format = "d", big.mark = ",")
  )

p_table <- as.ggplot(
  tableGrob(
    legend_df,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 11,
      core = list(fg_params = list(hjust = 0, x = 0.02)),
      colhead = list(fg_params = list(fontface = 2))
    )
  )
)

# Combine: map + legend + table
p_fig3 <- p_map / p_leg / p_table + plot_layout(heights = c(7, 1.1, 5))

ggsave("figs/fig3_map_legend_table_2024.png", p_fig3, width = 8.25, height = 10.5, dpi = 600)

# ------------------------------------------------------------------------------
# 5. Spatial Autocorrelation Check (Moran's I)
# ------------------------------------------------------------------------------

egohood_valid <- egohood_panel |>
  st_make_valid() |>
  filter(!st_is_empty(geom)) |>
  st_transform(2263)

nb <- poly2nb(egohood_valid)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

crime_moran <- moran.test(
  egohood_valid$crime_count,
  lw,
  zero.policy = TRUE,
  na.action = na.omit
)

print(crime_moran)

# ------------------------------------------------------------------------------
# 6. Prepare Modeling Panel with Demographics and Area
# ------------------------------------------------------------------------------

# Compute egohood area (geometry is time-invariant)
g_area <- egohood_panel |>
  filter(year == max(year, na.rm = TRUE)) |>
  st_make_valid() |>
  st_transform(2263)

area_df <- data.table(
  egohood_id = g_area$egohood_id,
  area_km2 = as.numeric(st_area(g_area)) / 1e6
)[, .SD[1], by = egohood_id]

# Drop geometry and merge area
panel_df <- as.data.table(st_drop_geometry(egohood_panel))
panel_df <- merge(panel_df, area_df, by = "egohood_id", all.x = TRUE)

# Create modeling variables
panel_df[, `:=`(
  log_crime = log1p(crime_count),
  log_ridership = log1p(annual_ridership)
)]

if ("population" %in% names(panel_df) && "area_km2" %in% names(panel_df)) {
  panel_df[, pop_density := population / pmax(area_km2, 1e-6)]
  panel_df[, log_popdens := log1p(pop_density)]
}

if ("median_income" %in% names(panel_df)) {
  panel_df[, log_income := log1p(median_income)]
}

# Save modeling-ready panel
write_parquet(panel_df, "data_clean/egohood_panel_flat_2020_2024.parquet")

# ==============================================================================
# Output Files:
#   - figs/fig_h1_hex_crime_2024.png
#   - figs/fig2_rep_egohood_riders_2024_labeled_top5_nums.png
#   - figs/fig3_map_legend_table_2024.png
#   - data_clean/egohood_panel_flat_2020_2024.parquet
# ==============================================================================