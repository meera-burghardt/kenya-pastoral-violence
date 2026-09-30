# ============================================================================
# run_all.R
#
# Master reproducibility script. Runs the full analysis pipeline in order,
# from raw data to figures. Total runtime: approximately 2-3 minutes.
#
# Usage (from the project root):
#     source("run_all.R")
#
# Requires:
#   - R 4.2+ and the spatial system libraries (GDAL, GEOS, PROJ)
#   - Restore package versions first with: renv::restore()
#   - Two datasets not committed to the repo:
#       * data/raw/acled_kenya_1997_2024.csv (register at acleddata.com)
#       * data/raw/nrt_events_2022.csv (private data; not redistributable)
# ============================================================================

# Track total runtime
start_time <- Sys.time()

cat("\n============================================================\n")
cat("  kenya-pastoral-violence — running full analysis pipeline\n")
cat("============================================================\n\n")


# ---- Check required raw data files exist ------------------------------------

required_files <- c(
  "data/raw/acled_kenya_1997_2024.csv",
  "data/raw/ucdp_ged_2024.csv",
  "data/raw/nrt_events_2022.csv",
  "data/raw/ndvi_kenya_adm2.csv",
  "data/raw/worldbank_precipitation_kenya.csv",
  "data/raw/wdpa_kenya_jan2024"
)

missing <- required_files[!file.exists(required_files)]

if (length(missing) > 0) {
  cat("ERROR: Missing required input files:\n")
  cat(paste0("  - ", missing, collapse = "\n"), "\n\n")
  cat("See README.md 'Data sources' section for download instructions.\n")
  stop("Cannot proceed without required data files.")
}

cat("All required data files found.\n\n")


# ---- Run pipeline scripts in order ------------------------------------------

scripts <- c(
  "R/01_load_spatial_data.R",
  "R/02_spatial_joins.R",
  "R/03_regression_analysis.R",
  "R/04_visualisations.R"
)

for (script in scripts) {
  cat(paste0("\n>>> Running ", script, "\n"))
  cat(paste0(strrep("-", 60), "\n"))
  source(script, echo = FALSE)
  cat(paste0("\n>>> Finished ", script, "\n"))
}


# ---- Report completion -------------------------------------------------------

end_time <- Sys.time()
elapsed  <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 1)

cat("\n============================================================\n")
cat("  Pipeline complete\n")
cat("============================================================\n\n")
cat("  Total runtime: ", elapsed, "seconds\n")
cat("  Outputs saved to: data/processed/ and figures/\n\n")
cat("  Next: open figures/ to view the plots, or\n")
cat("        source individual scripts in R/ to explore intermediate outputs.\n\n")