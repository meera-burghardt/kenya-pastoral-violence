# ============================================================================
# 03_regression_analysis.R
#
# Reproduces the statistical analyses from the thesis:
#   1. Climate–conflict regressions (rainfall value, drought index, NDVI)
#      with election-year controls and interactions
#   2. Protected-area designation regressions (conflict rate by designation)
#   3. Farmer-herder conflict share over time
#
# Depends on: R/01_load_spatial_data.R and R/02_spatial_joins.R
# ============================================================================

# ---- Setup ------------------------------------------------------------------

library(tidyverse)
library(here)
library(broom)      # tidy() to turn model output into data frames


# ---- Load data --------------------------------------------------------------

message("Loading data...")

# Annual climate + conflict + drought index dataset (hand-compiled)
annual <- read_csv(
  here("data", "processed", "annual_climate_conflict_index.csv"),
  show_col_types = FALSE
) %>%
  # Rename messy column names for cleaner formulas
  rename(
    precip_index = `precipitation index`,
    precip_value = `precipitation value`,
    election_yr  = `election year`
  ) %>%
  # Drop years with missing precipitation values (2023 in the current file)
  filter(!is.na(precip_value))

message("  Annual dataset: ", nrow(annual), " years (", min(annual$year), "-", max(annual$year), ")")

# Per-protected-area conflict counts (from script 02)
conflicts_by_pa <- readRDS(here("data", "processed", "conflicts_by_pa.rds"))

# Farmer-herder spatial points (from script 02) — used for time trend
farmer_herder_sf <- readRDS(here("data", "processed", "farmer_herder_sf.rds"))
acled_sf         <- readRDS(here("data", "processed", "acled_sf.rds"))


# ---- Helper: tidy print a model ---------------------------------------------

print_model <- function(mod, label) {
  cat("\n----", label, "----\n")
  print(summary(mod))
  cat("\nTidy coefficients:\n")
  print(tidy(mod, conf.int = TRUE))
  cat("R-squared: ", round(summary(mod)$r.squared, 3),
      " | Adj R-squared: ", round(summary(mod)$adj.r.squared, 3),
      " | N: ", nobs(mod), "\n")
}


# ============================================================================
# PART 1: CLIMATE AND CONFLICT REGRESSIONS
# ============================================================================
# NOTE: NRT time-series regressions are omitted from this repo because the
# available NRT snapshot (data/raw/nrt_events_2022.csv) covers only 2018-2022,
# giving too few observations for annual regression. The thesis used a later
# NRT export with more years; that export is not publicly redistributable.
# NRT is still used in the cross-sectional protected-area analysis (see Part 2),
# which does not depend on year coverage.

message("\n========== PART 1: Climate–conflict regressions ==========")

# ---- Precipitation value × election year, per dataset -----------------------

model_precip_acled <- lm(
  ACLED ~ precip_value + election_yr + precip_value:election_yr,
  data = annual
)

model_precip_ucdp <- lm(
  UCDP ~ precip_value + election_yr + precip_value:election_yr,
  data = annual
)

print_model(model_precip_acled, "ACLED ~ precipitation value × election year")
print_model(model_precip_ucdp,  "UCDP ~ precipitation value × election year")


# ---- Precipitation categorical index × election year ------------------------

# Ensure the drought index is a factor with meaningful reference level (low)
annual <- annual %>%
  mutate(precip_index = factor(precip_index, levels = c("low", "medium", "high")))

model_index_acled <- lm(
  ACLED ~ precip_index + election_yr + precip_index:election_yr,
  data = annual
)

model_index_ucdp <- lm(
  UCDP ~ precip_index + election_yr + precip_index:election_yr,
  data = annual
)

print_model(model_index_acled, "ACLED ~ drought index × election year")
print_model(model_index_ucdp,  "UCDP ~ drought index × election year")


# ---- NDVI × election year ---------------------------------------------------
# Compute yearly-average NDVI from monthly county-level data

message("\nComputing yearly NDVI averages...")

ndvi <- read_csv(here("data", "raw", "ndvi_kenya_adm2.csv"), show_col_types = FALSE)

# Try to find the date and value columns robustly
ndvi_date_col  <- grep("date", names(ndvi), value = TRUE, ignore.case = TRUE)[1]
ndvi_value_col <- grep("^vim$|^ndvi$|value", names(ndvi), value = TRUE, ignore.case = TRUE)[1]

message("  NDVI date column: ", ndvi_date_col, " | value column: ", ndvi_value_col)

ndvi_yearly <- ndvi %>%
  mutate(
    date_parsed = suppressWarnings(lubridate::ymd(.data[[ndvi_date_col]])),
    year        = lubridate::year(date_parsed),
    ndvi_val    = suppressWarnings(as.numeric(.data[[ndvi_value_col]]))
  ) %>%
  filter(!is.na(year), !is.na(ndvi_val)) %>%
  group_by(year) %>%
  summarise(ndvi_mean = mean(ndvi_val, na.rm = TRUE), .groups = "drop")

message("  NDVI yearly averages: ", nrow(ndvi_yearly), " years (",
        min(ndvi_yearly$year), "-", max(ndvi_yearly$year), ")")

# Merge NDVI with annual conflict data
annual_ndvi <- annual %>%
  left_join(ndvi_yearly, by = "year") %>%
  filter(!is.na(ndvi_mean))

message("  Years with NDVI available: ", nrow(annual_ndvi))

model_ndvi_acled <- lm(
  ACLED ~ ndvi_mean + election_yr + ndvi_mean:election_yr,
  data = annual_ndvi
)

model_ndvi_ucdp <- lm(
  UCDP ~ ndvi_mean + election_yr + ndvi_mean:election_yr,
  data = annual_ndvi
)

print_model(model_ndvi_acled, "ACLED ~ NDVI × election year")
print_model(model_ndvi_ucdp,  "UCDP ~ NDVI × election year")


# ============================================================================
# PART 2: PROTECTED AREA DESIGNATION REGRESSIONS
# ============================================================================

message("\n========== PART 2: Protected-area designation regressions ==========")

# Filter to designations with meaningful sample size (n >= 3 PAs)
pa_for_model <- conflicts_by_pa %>%
  filter(!is.na(conflicts_per_100_sqkm), area_sqkm > 0) %>%
  group_by(DESIG) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  mutate(DESIG = factor(DESIG))

# Set Community Conservancy as reference level, matching the thesis
if ("Community Conservancy" %in% levels(pa_for_model$DESIG)) {
  pa_for_model$DESIG <- relevel(pa_for_model$DESIG, ref = "Community Conservancy")
}

message("  Designations included: ", nlevels(pa_for_model$DESIG))
message("  Protected areas in model: ", nrow(pa_for_model))

# Total conflicts by designation
model_desig_total <- lm(total_conflicts ~ DESIG, data = pa_for_model)
print_model(model_desig_total, "Total conflicts ~ designation")

# Conflicts per 100 sq km by designation (density, area-normalised)
model_desig_density <- lm(conflicts_per_100_sqkm ~ DESIG, data = pa_for_model)
print_model(model_desig_density, "Conflicts per 100 sq km ~ designation")


# ============================================================================
# PART 3: FARMER-HERDER SHARE OF CONFLICTS OVER TIME
# ============================================================================

message("\n========== PART 3: Farmer-herder share over time ==========")

fh_by_year <- farmer_herder_sf %>%
  sf::st_drop_geometry() %>%
  count(year, name = "farmer_herder_events")

acled_by_year <- acled_sf %>%
  sf::st_drop_geometry() %>%
  count(year, name = "all_acled_events")

fh_share <- acled_by_year %>%
  left_join(fh_by_year, by = "year") %>%
  mutate(
    farmer_herder_events = replace_na(farmer_herder_events, 0),
    share_pct            = round(farmer_herder_events / all_acled_events * 100, 1)
  )

message("Farmer-herder share of ACLED events, by year:")
print(fh_share)

# Correlation between farmer-herder events and annual rainfall
fh_rainfall <- fh_share %>%
  left_join(select(annual, year, precip_value), by = "year") %>%
  filter(!is.na(precip_value), !is.na(farmer_herder_events))

model_fh_rainfall <- lm(precip_value ~ farmer_herder_events, data = fh_rainfall)
print_model(model_fh_rainfall, "Precipitation value ~ farmer-herder events")


# ============================================================================
# SAVE MODELS AND SUMMARY TABLES
# ============================================================================

message("\n========== Saving model outputs ==========")

# Save all models as one list for later use
all_models <- list(
  precip_acled    = model_precip_acled,
  precip_ucdp     = model_precip_ucdp,
  index_acled     = model_index_acled,
  index_ucdp      = model_index_ucdp,
  ndvi_acled      = model_ndvi_acled,
  ndvi_ucdp       = model_ndvi_ucdp,
  desig_total     = model_desig_total,
  desig_density   = model_desig_density,
  fh_rainfall     = model_fh_rainfall
)

saveRDS(all_models, here("data", "processed", "regression_models.rds"))

# Tidy coefficient tables for easy reference
model_coefs_df <- map_dfr(all_models, tidy, conf.int = TRUE, .id = "model")
write_csv(model_coefs_df, here("data", "processed", "regression_coefficients.csv"))

# Farmer-herder share time series
write_csv(fh_share, here("data", "processed", "farmer_herder_share_by_year.csv"))

message("Done. Regression outputs saved:")
message("  - regression_models.rds")
message("  - regression_coefficients.csv")
message("  - farmer_herder_share_by_year.csv")