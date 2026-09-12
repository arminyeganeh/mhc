# ============================================================
# Figure: Detected Manufactured Housing Communities in Indiana
#
# Description:
# Creates a statewide map of detected manufactured housing
# communities (MHCs) using a Google terrain basemap.
#
# Required input:
#   data/in_file.csv
#
# Required columns:
#   Cent_x = longitude
#   Cent_y = latitude
#
# Output:
#   figures/fig_4.png
#
# Coordinate reference system:
#   WGS84 (EPSG:4326)
# ============================================================


# ------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------

library(ggplot2)
library(ggmap)


# ------------------------------------------------------------
# 2. File paths
# ------------------------------------------------------------

input_file  <- file.path("data", "in_file.csv")
output_dir  <- "figures"
output_file <- file.path(output_dir, "fig_4.png")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# ------------------------------------------------------------
# 3. Register Google Maps API key
# ------------------------------------------------------------

# Store your API key in an environment variable named:
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
# 4. Read MHC locations
# ------------------------------------------------------------

mhc_data <- read.csv(
  input_file,
  stringsAsFactors = FALSE
)

required_columns <- c("Cent_x", "Cent_y")

if (!all(required_columns %in% names(mhc_data))) {
  stop(
    "Input file must contain the following columns: ",
    paste(required_columns, collapse = ", ")
  )
}

mhc_data <- mhc_data[
  !is.na(mhc_data$Cent_x) & !is.na(mhc_data$Cent_y),
]


# ------------------------------------------------------------
# 5. Download Google terrain basemap
# ------------------------------------------------------------

map_center <- c(
  lon = -86.2816,
  lat = 39.8942
)

base_map <- get_map(
  location = map_center,
  zoom = 7,
  maptype = "terrain",
  source = "google"
)


# ------------------------------------------------------------
# 6. Create figure
# ------------------------------------------------------------

mhc_points_map <- ggmap(base_map) +
  
  geom_point(
    data = mhc_data,
    aes(
      x = Cent_x,
      y = Cent_y,
      fill = "Detected MHC"
    ),
    shape = 21,
    size = 2,
    color = "black",
    alpha = 0.85
  ) +
  
  scale_fill_manual(
    values = c("Detected MHC" = "orange"),
    name = NULL
  ) +
  
  labs(
    title = "Detected Manufactured Housing Communities in Indiana",
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
      hjust = 0.125
    ),
    
    plot.title.position = "plot",
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid = element_blank()
  )


# ------------------------------------------------------------
# 7. Display and export
# ------------------------------------------------------------

print(mhc_points_map)

ggsave(
  filename = output_file,
  plot = mhc_points_map,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)
