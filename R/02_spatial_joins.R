# ============================================================================
# 02_spatial_joins.R
#
# Reproduces the QGIS spatial joins from the original thesis workflow in R:
#   1. Count ACLED, UCDP, and NRT conflict events inside each WDPA
#      protected area.
#   2. Compute polygon areas so we can derive conflicts-per-square-km.
#   3. Identify farmer-herder (pastoral) conflicts from ACLED and count
#      those inside protected areas.
#   4. Save the combined per-protected-area dataset to data/processed/.
#
# Depends on: R/01_load_spatial_data.R (must be run first)
# ============================================================================

# ---- Setup ------------------------------------------------------------------

library(sf)
library(tidyverse)
library(here)

WGS84     <- 4326
KENYA_CRS <- 21037  # Arc 1960 / UTM zone 37S — projected, in metres


# ---- Load cleaned spatial objects from script 01 ----------------------------

message("Loading spatial objects from 01_load_spatial_data.R output...")

acled_sf <- readRDS(here("data", "processed", "acled_sf.rds"))
ucdp_sf  <- readRDS(here("data", "processed", "ucdp_sf.rds"))
nrt_sf   <- readRDS(here("data", "processed", "nrt_sf.rds"))
wdpa     <- readRDS(here("data", "processed", "wdpa_sf.rds"))

message("  Loaded: ", nrow(acled_sf), " ACLED, ", nrow(ucdp_sf), " UCDP, ",
        nrow(nrt_sf), " NRT, ", nrow(wdpa), " protected areas")


# ---- Isolate farmer-herder (pastoral) conflicts from ACLED ------------------
# Replicates the regex-based filter from the original thesis code

message("\nIsolating farmer-herder conflicts in ACLED...")

pastoralist_pattern <- regex("herders|pastoralists", ignore_case = TRUE)

farmer_herder_sf <- acled_sf %>%
  filter(
    str_detect(assoc_actor_1, regex("Pastoralists", ignore_case = TRUE)) |
      str_detect(assoc_actor_2, regex("Pastoralists", ignore_case = TRUE)) |
      str_detect(notes, pastoralist_pattern)
  )

# str_detect returns NA when input is NA; drop those to avoid noise
farmer_herder_sf <- farmer_herder_sf %>% filter(!is.na(geometry))

message("  Farmer-herder events identified: ", nrow(farmer_herder_sf),
        " of ", nrow(acled_sf), " ACLED events")


# ---- Compute area of each protected area ------------------------------------
# Project to metres (KENYA_CRS) before computing areas. Area calculations
# on unprojected geographic coordinates return values in square degrees,
# which are not meaningful for cross-country comparisons.

message("\nComputing protected area geometries in projected CRS...")

wdpa_projected <- st_transform(wdpa, crs = KENYA_CRS)

# Fix any invalid geometries (common in WDPA data)
wdpa_projected <- st_make_valid(wdpa_projected)

# WDPA stores some protected areas as multiple polygons sharing one WDPAID.
# Dissolve to one geometry per WDPAID (union multipart polygons together)
# so spatial joins produce one count per protected area, not per polygon.
message("  Raw WDPA rows: ", nrow(wdpa_projected))

wdpa_projected <- wdpa_projected %>%
  group_by(WDPAID) %>%
  summarise(
    WDPA_PID   = first(WDPA_PID),
    NAME       = first(NAME),
    ORIG_NAME  = first(ORIG_NAME),
    DESIG      = first(DESIG),
    DESIG_ENG  = first(DESIG_ENG),
    DESIG_TYPE = first(DESIG_TYPE),
    IUCN_CAT   = first(IUCN_CAT),
    STATUS     = first(STATUS),
    STATUS_YR  = first(STATUS_YR),
    GOV_TYPE   = first(GOV_TYPE),
    OWN_TYPE   = first(OWN_TYPE),
    .groups    = "drop"
  ) %>%
  st_make_valid()

message("  After dissolving multipart polygons: ", nrow(wdpa_projected), " unique protected areas")

wdpa_areas <- wdpa_projected %>%
  mutate(area_sqkm = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  select(WDPAID, area_sqkm)

message("  Area computed for ", nrow(wdpa_areas), " protected areas")
message("  Median area: ", round(median(wdpa_areas$area_sqkm, na.rm = TRUE), 1),
        " sq km; total: ", round(sum(wdpa_areas$area_sqkm, na.rm = TRUE), 0), " sq km")


# ---- Reproject points to match polygons for spatial join --------------------

acled_proj         <- st_transform(acled_sf,         crs = KENYA_CRS)
ucdp_proj          <- st_transform(ucdp_sf,          crs = KENYA_CRS)
nrt_proj           <- st_transform(nrt_sf,           crs = KENYA_CRS)
farmer_herder_proj <- st_transform(farmer_herder_sf, crs = KENYA_CRS)


# ---- Helper: count how many points from a dataset fall in each polygon ------

count_points_in_polygons <- function(points_sf, polygons_sf, id_col = "WDPAID") {
  # st_intersects returns a list: for each polygon, which points intersect it
  hits <- st_intersects(polygons_sf, points_sf)
  tibble(
    !!id_col := polygons_sf[[id_col]],
    count    = lengths(hits)
  )
}


# ---- Count conflicts inside each protected area, by dataset -----------------

message("\nRunning spatial joins (points-in-polygons)...")

acled_counts <- count_points_in_polygons(acled_proj, wdpa_projected) %>%
  rename(ACLED = count)

ucdp_counts <- count_points_in_polygons(ucdp_proj, wdpa_projected) %>%
  rename(UCDP = count)

nrt_counts <- count_points_in_polygons(nrt_proj, wdpa_projected) %>%
  rename(NRT = count)

farmer_herder_counts <- count_points_in_polygons(farmer_herder_proj, wdpa_projected) %>%
  rename(pastoral_conflicts = count)

message("  ACLED events inside any protected area: ", sum(acled_counts$ACLED))
message("  UCDP events inside any protected area:  ", sum(ucdp_counts$UCDP))
message("  NRT events inside any protected area:   ", sum(nrt_counts$NRT))
message("  Farmer-herder events inside any PA:     ", sum(farmer_herder_counts$pastoral_conflicts))


# ---- Combine into one per-protected-area dataset ----------------------------

message("\nBuilding combined per-protected-area dataset...")

# Use the deduplicated wdpa_projected (one row per WDPAID) as our attribute source
wdpa_attrs <- wdpa_projected %>%
  st_drop_geometry() %>%
  select(WDPAID, WDPA_PID, NAME, ORIG_NAME, DESIG, DESIG_ENG, DESIG_TYPE,
         IUCN_CAT, STATUS, STATUS_YR, GOV_TYPE, OWN_TYPE)

conflicts_by_pa <- wdpa_attrs %>%
  left_join(wdpa_areas,          by = "WDPAID") %>%
  left_join(acled_counts,        by = "WDPAID") %>%
  left_join(ucdp_counts,         by = "WDPAID") %>%
  left_join(nrt_counts,          by = "WDPAID") %>%
  left_join(farmer_herder_counts, by = "WDPAID") %>%
  mutate(
    total_conflicts        = ACLED + UCDP + NRT,
    conflicts_per_sqkm     = if_else(area_sqkm > 0, total_conflicts / area_sqkm, NA_real_),
    conflicts_per_100_sqkm = conflicts_per_sqkm * 100,
    pastoral_per_100_sqkm  = if_else(area_sqkm > 0, pastoral_conflicts / area_sqkm * 100, NA_real_)
  )

message("  Combined dataset: ", nrow(conflicts_by_pa), " protected areas, ",
        ncol(conflicts_by_pa), " columns")


# ---- Sanity checks ----------------------------------------------------------

message("\n---- Sanity checks ----")

# Total conflicts inside PAs, by dataset — compare to thesis Table 4.2
message("Total events inside protected areas (thesis reports 824 / 140 / 710 for ACLED / UCDP / NRT):")
message("  ACLED: ", sum(conflicts_by_pa$ACLED))
message("  UCDP:  ", sum(conflicts_by_pa$UCDP))
message("  NRT:   ", sum(conflicts_by_pa$NRT))

# Top 5 protected areas by total conflict count
message("\nTop 5 protected areas by total conflicts:")
top5 <- conflicts_by_pa %>%
  arrange(desc(total_conflicts)) %>%
  head(5) %>%
  select(NAME, DESIG, total_conflicts, area_sqkm)
print(top5)

# Mean conflict density by designation type
message("\nMean conflicts per 100 sq km, by designation:")
by_desig <- conflicts_by_pa %>%
  filter(!is.na(conflicts_per_100_sqkm), area_sqkm > 0) %>%
  group_by(DESIG) %>%
  summarise(
    n_pas      = n(),
    mean_c100  = round(mean(conflicts_per_100_sqkm, na.rm = TRUE), 2),
    total      = sum(total_conflicts, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(desc(mean_c100))
print(by_desig)


# ---- Save outputs -----------------------------------------------------------

message("\n---- Saving processed outputs ----")

# Main output: per-PA conflict counts, ready for regression scripts
saveRDS(conflicts_by_pa, here("data", "processed", "conflicts_by_pa.rds"))
write_csv(conflicts_by_pa, here("data", "processed", "conflicts_by_pa.csv"))

# Farmer-herder conflict spatial object (used by later scripts)
saveRDS(farmer_herder_sf, here("data", "processed", "farmer_herder_sf.rds"))

# Also save a farmer-herder-only per-PA counts file for the pastoral analysis
pastoral_by_pa <- wdpa_attrs %>%
  left_join(wdpa_areas,           by = "WDPAID") %>%
  left_join(farmer_herder_counts, by = "WDPAID") %>%
  mutate(pastoral_per_100_sqkm =
           if_else(area_sqkm > 0, pastoral_conflicts / area_sqkm * 100, NA_real_))

saveRDS(pastoral_by_pa, here("data", "processed", "pastoral_by_pa.rds"))
write_csv(pastoral_by_pa, here("data", "processed", "pastoral_by_pa.csv"))

message("Done. Spatial join outputs saved to data/processed/")
message("  - conflicts_by_pa.rds / .csv  (main per-PA dataset)")
message("  - pastoral_by_pa.rds / .csv   (farmer-herder subset)")
message("  - farmer_herder_sf.rds        (spatial points for mapping)")