# ============================================================================
# 01_load_spatial_data.R
#
# Load the three conflict datasets (ACLED, UCDP, NRT) as sf point objects,
# load the WDPA Kenya protected area polygons, harmonise coordinate reference
# systems, and save clean versions to data/processed/ for downstream scripts.
#
# This script replaces the QGIS spatial layer loading step from the original
# thesis workflow.
# ============================================================================

# ---- Setup ------------------------------------------------------------------

library(sf)
library(tidyverse)
library(here)

# Kenya-appropriate projected CRS for area calculations (in metres)
# EPSG:21037 is Arc 1960 / UTM zone 37S, standard for Kenya
KENYA_CRS <- 21037

# Geographic CRS for point data (WGS84, standard lat/lon)
WGS84 <- 4326


# ---- Load ACLED conflict data -----------------------------------------------

message("Loading ACLED...")

acled_raw <- read_csv(
  here("data", "raw", "acled_kenya_1997_2024.csv"),
  show_col_types = FALSE
)

acled_sf <- acled_raw %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = WGS84, remove = FALSE)

message("  ACLED loaded: ", nrow(acled_sf), " events")


# ---- Load UCDP conflict data ------------------------------------------------

message("Loading UCDP...")

ucdp_raw <- read_csv(
  here("data", "raw", "ucdp_ged_2024.csv"),
  show_col_types = FALSE
)

# UCDP GED covers the world; filter to Kenya only
ucdp_sf <- ucdp_raw %>%
  filter(country == "Kenya") %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = WGS84, remove = FALSE)

message("  UCDP loaded: ", nrow(ucdp_sf), " events (Kenya only)")


# ---- Load NRT (Northern Rangelands Trust) conflict data ---------------------

message("Loading NRT...")

nrt_raw <- read_csv(
  here("data", "raw", "nrt_events_2022.csv"),
  show_col_types = FALSE
)

# Filter to conflict-type events only, per thesis methodology
nrt_conflicts <- nrt_raw %>%
  filter(Report_Type == "Conflict")

message("  NRT raw conflict events: ", nrow(nrt_conflicts))

# NRT column names for coordinates vary; detect them
nrt_lon_col <- grep("^Long", names(nrt_conflicts), value = TRUE, ignore.case = TRUE)[1]
nrt_lat_col <- grep("^Lat",  names(nrt_conflicts), value = TRUE, ignore.case = TRUE)[1]

message("  NRT coordinate columns detected: ", nrt_lon_col, " / ", nrt_lat_col)

nrt_sf <- nrt_conflicts %>%
  filter(!is.na(.data[[nrt_lon_col]]), !is.na(.data[[nrt_lat_col]])) %>%
  st_as_sf(
    coords = c(nrt_lon_col, nrt_lat_col),
    crs = WGS84,
    remove = FALSE
  )

message("  NRT loaded: ", nrow(nrt_sf), " geocoded conflict events (",
        nrow(nrt_conflicts) - nrow(nrt_sf), " dropped for missing coordinates)")


# ---- Load WDPA protected areas ----------------------------------------------

message("Loading WDPA protected areas...")

# WDPA splits the Kenya dataset across three subfolders; read each and combine
wdpa_folders <- c(
  here("data", "raw", "wdpa_kenya_jan2024", "WDPA_WDOECM_Jan2024_Public_KEN_shp_0"),
  here("data", "raw", "wdpa_kenya_jan2024", "WDPA_WDOECM_Jan2024_Public_KEN_shp_1"),
  here("data", "raw", "wdpa_kenya_jan2024", "WDPA_WDOECM_Jan2024_Public_KEN_shp_2")
)

wdpa_polygons_list <- map(wdpa_folders, function(folder) {
  shp_file <- list.files(folder, pattern = "polygons\\.shp$", full.names = TRUE)
  st_read(shp_file, quiet = TRUE)
})

wdpa <- bind_rows(wdpa_polygons_list) %>%
  st_transform(crs = WGS84)

message("  WDPA loaded: ", nrow(wdpa), " protected areas")


# ---- Sanity checks ----------------------------------------------------------

message("\n---- Sanity checks ----")

# All should be in WGS84
message("ACLED CRS: EPSG:", st_crs(acled_sf)$epsg)
message("UCDP CRS:  EPSG:", st_crs(ucdp_sf)$epsg)
message("NRT CRS:   EPSG:", st_crs(nrt_sf)$epsg)
message("WDPA CRS:  EPSG:", st_crs(wdpa)$epsg)

# Kenya bounding box roughly: lon 33.9 to 41.9, lat -4.7 to 5.5
kenya_bbox <- st_bbox(c(xmin = 33.9, ymin = -4.7, xmax = 41.9, ymax = 5.5), crs = WGS84)

acled_outside <- sum(!st_intersects(acled_sf, st_as_sfc(kenya_bbox), sparse = FALSE))
ucdp_outside  <- sum(!st_intersects(ucdp_sf,  st_as_sfc(kenya_bbox), sparse = FALSE))
nrt_outside   <- sum(!st_intersects(nrt_sf,   st_as_sfc(kenya_bbox), sparse = FALSE))

message("\nPoints outside Kenya bounding box:")
message("  ACLED: ", acled_outside, " of ", nrow(acled_sf))
message("  UCDP:  ", ucdp_outside,  " of ", nrow(ucdp_sf))
message("  NRT:   ", nrt_outside,   " of ", nrow(nrt_sf))


# ---- Save cleaned spatial objects -------------------------------------------

message("\n---- Saving processed spatial objects ----")

saveRDS(acled_sf, here("data", "processed", "acled_sf.rds"))
saveRDS(ucdp_sf,  here("data", "processed", "ucdp_sf.rds"))
saveRDS(nrt_sf,   here("data", "processed", "nrt_sf.rds"))
saveRDS(wdpa,     here("data", "processed", "wdpa_sf.rds"))

message("Done. Cleaned spatial objects saved to data/processed/")