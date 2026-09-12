# ============================================================
# Figure: Change in Manufactured Housing Prevalence, 2000–2020
#
# Description:
# Maps changes in manufactured housing community (MHC)
# prevalence across Indiana Census block groups between
# 2000 and 2020.
#
# Block groups are classified as:
#   - Decline of at least 5 percentage points
#   - Stable / small change
#   - Growth of at least 5 percentage points
#
# Urban area boundaries and selected Indiana cities are
# included for geographic context.
#
# Required inputs:
#   data/choropleth_basis.csv
#   geodatasets/cb_2020_18_bg_500k.shp
#   geodatasets/cb_2020_us_ua20_500k.shp
#
# Output:
#   figures/fig_6.png
#
# Map coordinates:
#   WGS84 (EPSG:4326)
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(readr)
library(sf)


# ------------------------------------------------------------
# 2. File paths
# ------------------------------------------------------------

data_file <- file.path(
  "data",
  "choropleth_basis.csv"
)

bg_file <- file.path(
  "geodatasets",
  "cb_2020_18_bg_500k.shp"
)

urban_file <- file.path(
  "geodatasets",
  "cb_2020_us_ua20_500k.shp"
)

output_dir <- "figures"
output_file <- file.path(output_dir, "fig_6.png")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# ------------------------------------------------------------
# 3. Register Google Maps API key
# ------------------------------------------------------------

# Store the API key in an environment variable named:
# GOOGLE_MAPS_API_KEY
#
# Do not place the API key directly in this script.

google_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

if (google_key == "") {
  stop(
    "Google Maps API key not found. ",
    "Set the GOOGLE_MAPS_API_KEY environment variable before running this script."
  )
}

register_google(key = google_key)


# ------------------------------------------------------------
# 4. Read MHC block-group data
# ------------------------------------------------------------

choropleth_basis <- read_csv(
  data_file,
  col_types = cols(
    GEOID_2020 = col_character()
  )
)

required_columns <- c(
  "GEOID_2020",
  "year",
  "percentage_mhc"
)

if (!all(required_columns %in% names(choropleth_basis))) {
  stop(
    "choropleth_basis.csv must contain: ",
    paste(required_columns, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 5. Reshape data and calculate MHC change
# ------------------------------------------------------------

choropleth_basis_wide <- choropleth_basis %>%
  select(
    GEOID_2020,
    year,
    percentage_mhc
  ) %>%
  mutate(
    year = as.character(year)
  ) %>%
  pivot_wider(
    id_cols = GEOID_2020,
    names_from = year,
    values_from = percentage_mhc,
    names_prefix = "percentage_mhc_"
  ) %>%
  mutate(
    mhc_growth =
      percentage_mhc_2020 -
      percentage_mhc_2000,
    
    growth_category = case_when(
      mhc_growth <= -5 ~ "Decline ≥ 5 pp",
      mhc_growth >=  5 ~ "Growth ≥ 5 pp",
      !is.na(mhc_growth) ~ "Stable / Small Change",
      TRUE ~ NA_character_
    )
  )


# ------------------------------------------------------------
# 6. Read 2020 Indiana Census block groups
# ------------------------------------------------------------

indiana_bg <- st_read(
  bg_file,
  quiet = TRUE
) %>%
  mutate(
    GEOID_2020 = as.character(GEOID)
  ) %>%
  select(
    GEOID_2020,
    geometry
  )


# Join longitudinal MHC data to 2020 block-group geography
indiana_bg_plot <- indiana_bg %>%
  left_join(
    choropleth_basis_wide,
    by = "GEOID_2020"
  )


# ------------------------------------------------------------
# 7. Create Indiana state boundary
# ------------------------------------------------------------

indiana_boundary <- indiana_bg %>%
  summarise(
    geometry = st_union(geometry)
  )


# ------------------------------------------------------------
# 8. Read and clip 2020 urban areas to Indiana
# ------------------------------------------------------------

urban_areas <- st_read(
  urban_file,
  quiet = TRUE
)

# Use a projected CRS for geometric operations
# EPSG:5070 = NAD83 / Conus Albers
indiana_boundary_projected <- indiana_boundary %>%
  st_transform(5070) %>%
  st_make_valid()

urban_areas_indiana <- urban_areas %>%
  st_transform(5070) %>%
  st_make_valid() %>%
  st_intersection(
    indiana_boundary_projected
  )


# ------------------------------------------------------------
# 9. Create representative points for block groups
# ------------------------------------------------------------

# Only block groups with observed MHC change are plotted.
# Representative points are calculated in a projected CRS
# and then transformed back to WGS84.

indiana_bg_points <- indiana_bg_plot %>%
  filter(
    !is.na(growth_category)
  ) %>%
  st_transform(5070) %>%
  st_point_on_surface() %>%
  st_transform(4326)


# ------------------------------------------------------------
# 10. Selected Indiana cities
# ------------------------------------------------------------

cities <- data.frame(
  city = c(
    "Indianapolis",
    "Fort Wayne",
    "South Bend",
    "Evansville",
    "Bloomington",
    "Lafayette"
  ),
  
  lon = c(
    -86.1581,
    -85.1394,
    -86.2510,
    -87.5711,
    -86.5264,
    -86.8753
  ),
  
  lat = c(
    39.7684,
    41.0793,
    41.6764,
    37.9716,
    39.1653,
    40.4167
  )
)


# ------------------------------------------------------------
# 11. Convert map layers to WGS84
# ------------------------------------------------------------

indiana_boundary <- indiana_boundary %>%
  st_transform(4326)

urban_areas_indiana <- urban_areas_indiana %>%
  st_transform(4326)


# ------------------------------------------------------------
# 12. Download Indiana-centered terrain basemap
# ------------------------------------------------------------

map_center <- c(
  lon = -86.1349,
  lat = 39.7684
)

base_map <- get_map(
  location = map_center,
  zoom = 7,
  maptype = "terrain",
  source = "google"
)


# ------------------------------------------------------------
# 13. Create figure
# ------------------------------------------------------------

change_plot <- ggmap(base_map) +
  
  # Indiana outline
  geom_sf(
    data = indiana_boundary,
    inherit.aes = FALSE,
    fill = "white",
    color = "black",
    linewidth = 0.3
  ) +
  
  # Block groups represented by points
  geom_sf(
    data = indiana_bg_points,
    aes(
      color = growth_category
    ),
    inherit.aes = FALSE,
    size = 3,
    alpha = 0.8
  ) +
  
  # Urban area boundaries
  geom_sf(
    data = urban_areas_indiana,
    aes(
      linetype = "Urban Area Boundary"
    ),
    inherit.aes = FALSE,
    fill = NA,
    color = "orange",
    linewidth = 0.2
  ) +
  
  # Selected cities
  geom_point(
    data = cities,
    aes(
      x = lon,
      y = lat
    ),
    color = "black",
    size = 1.6
  ) +
  
  geom_text(
    data = cities,
    aes(
      x = lon,
      y = lat,
      label = city
    ),
    nudge_y = 0.08,
    size = 3
  ) +
  
  # MHC change legend
  scale_color_manual(
    values = c(
      "Decline ≥ 5 pp" = "#d73027",
      "Stable / Small Change" = "grey70",
      "Growth ≥ 5 pp" = "#1a9850"
    ),
    name = "MHC Change"
  ) +
  
  # Urban-area legend
  scale_linetype_manual(
    values = c(
      "Urban Area Boundary" = "solid"
    ),
    name = NULL
  ) +
  
  labs(
    title = "Change in Manufactured Housing Prevalence (2000–2020)",
    x = "Longitude",
    y = "Latitude",
    caption = "Basemap: Google terrain. Coordinates shown in WGS84."
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "right",
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    
    plot.title.position = "plot",
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid = element_blank()
  )


# ------------------------------------------------------------
# 14. Display and export
# ------------------------------------------------------------

print(change_plot)

ggsave(
  filename = output_file,
  plot = change_plot,
  width = 10,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)