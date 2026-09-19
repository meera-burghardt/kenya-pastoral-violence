# ============================================================================
# 04_visualisations.R
#
# Produces the key figures for the README and repo. Every plot is saved to
# figures/ as PNG at consistent dimensions and DPI for use in Markdown docs.
#
# Depends on: R/01_load_spatial_data.R, R/02_spatial_joins.R,
#             R/03_regression_analysis.R
# ============================================================================

# ---- Setup ------------------------------------------------------------------

library(sf)
library(tidyverse)
library(here)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)

# Ensure figures directory exists
dir.create(here("figures"), showWarnings = FALSE, recursive = TRUE)

# Consistent visual style for all figures
theme_thesis <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title       = element_text(face = "bold", size = 14, margin = margin(b = 4)),
      plot.subtitle    = element_text(colour = "grey40", size = 11, margin = margin(b = 12)),
      plot.caption     = element_text(colour = "grey50", size = 9, hjust = 0, margin = margin(t = 8)),
      plot.margin      = margin(t = 15, r = 25, b = 10, l = 15),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92"),
      axis.title       = element_text(colour = "grey30", size = 11),
      legend.title     = element_text(colour = "grey30", size = 10),
      strip.text       = element_text(face = "bold")
    )
}

# Muted, colour-blind-friendly palette
COL_PRIMARY   <- "#2E5A88"   # steel blue
COL_SECONDARY <- "#C05746"   # muted red
COL_ACCENT    <- "#7A9B76"   # sage green
COL_GREY      <- "#7A7A7A"


# ---- Load data --------------------------------------------------------------

message("Loading data for visualisations...")

annual          <- read_csv(here("data", "processed", "annual_climate_conflict_index.csv"), show_col_types = FALSE) %>%
  rename(precip_index = `precipitation index`,
         precip_value = `precipitation value`,
         election_yr  = `election year`) %>%
  filter(!is.na(precip_value))

conflicts_by_pa <- readRDS(here("data", "processed", "conflicts_by_pa.rds"))
farmer_herder_sf <- readRDS(here("data", "processed", "farmer_herder_sf.rds"))
wdpa            <- readRDS(here("data", "processed", "wdpa_sf.rds"))
fh_share        <- read_csv(here("data", "processed", "farmer_herder_share_by_year.csv"), show_col_types = FALSE)

# Recompute NDVI yearly averages (needed for figure 2)
ndvi <- read_csv(here("data", "raw", "ndvi_kenya_adm2.csv"), show_col_types = FALSE)
ndvi_date_col  <- grep("date", names(ndvi), value = TRUE, ignore.case = TRUE)[1]
ndvi_value_col <- grep("^vim$|^ndvi$|value", names(ndvi), value = TRUE, ignore.case = TRUE)[1]

ndvi_yearly <- ndvi %>%
  mutate(
    date_parsed = suppressWarnings(lubridate::ymd(.data[[ndvi_date_col]])),
    year        = lubridate::year(date_parsed),
    ndvi_val    = suppressWarnings(as.numeric(.data[[ndvi_value_col]]))
  ) %>%
  filter(!is.na(year), !is.na(ndvi_val)) %>%
  group_by(year) %>%
  summarise(ndvi_mean = mean(ndvi_val, na.rm = TRUE), .groups = "drop")

annual_ndvi <- annual %>%
  left_join(ndvi_yearly, by = "year") %>%
  filter(!is.na(ndvi_mean))


# ============================================================================
# FIGURE 1: Farmer-herder share of ACLED events over time
# ============================================================================

message("Building Figure 1: farmer-herder share over time...")

fig1 <- fh_share %>%
  ggplot(aes(x = year, y = share_pct)) +
  geom_col(fill = COL_PRIMARY, width = 0.7) +
  geom_text(aes(label = paste0(share_pct, "%")),
            vjust = -0.5, size = 3, colour = COL_GREY) +
  scale_x_continuous(breaks = seq(1997, 2024, by = 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title    = "Farmer-herder conflicts as a share of all ACLED events in Kenya",
    subtitle = "Pastoral conflicts identified via ACLED actor and notes fields",
    x        = NULL,
    y        = "Share of ACLED events (%)",
    caption  = "Source: ACLED (1997–2024). Author's analysis."
  ) +
  theme_thesis()

ggsave(here("figures", "01_farmer_herder_share.png"),
       plot = fig1, width = 10, height = 5.5, dpi = 200, bg = "white")


# ============================================================================
# FIGURE 2: NDVI × election year interaction (the strongest finding)
# ============================================================================

message("Building Figure 2: NDVI × conflict, split by election year...")

fig2_data <- annual_ndvi %>%
  mutate(election_label = if_else(election_yr == 1, "Election year", "Non-election year"))

fig2 <- fig2_data %>%
  ggplot(aes(x = ndvi_mean, y = ACLED, colour = election_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  geom_point(size = 3.5, alpha = 0.9) +
  scale_colour_manual(values = c("Election year" = COL_SECONDARY,
                                 "Non-election year" = COL_PRIMARY),
                      name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.08))) +
  labs(
    title    = "The rainfall–conflict relationship reverses in election years",
    subtitle = "Election years show a negative slope (red); non-election years a modest positive one (blue)",
    x        = "Mean annual NDVI (higher = greener vegetation)",
    y        = "ACLED conflict events",
    caption  = "Regression: ACLED ~ NDVI × election year. Adjusted R² = 0.41, interaction p = 0.016. N = 21."
  ) +
  theme_thesis() +
  theme(legend.position = "top")

ggsave(here("figures", "02_ndvi_election_interaction.png"),
       plot = fig2, width = 9, height = 6, dpi = 200, bg = "white")


# ============================================================================
# FIGURE 3: Rainfall vs farmer-herder conflicts (scatter)
# ============================================================================

message("Building Figure 3: rainfall vs farmer-herder conflicts (scatter)...")

fig3_data <- fh_share %>%
  left_join(select(annual, year, precip_value), by = "year") %>%
  filter(!is.na(precip_value), !is.na(farmer_herder_events))

# Label only the visually interesting outlier years to avoid clutter
fig3_labels <- fig3_data %>%
  filter(farmer_herder_events > 15 | year %in% c(2000, 2005, 2019))

fig3 <- fig3_data %>%
  ggplot(aes(x = precip_value, y = farmer_herder_events)) +
  geom_smooth(method = "lm", colour = COL_SECONDARY,
              fill = COL_SECONDARY, alpha = 0.15, linewidth = 1) +
  geom_point(size = 3.5, colour = COL_PRIMARY, alpha = 0.85) +
  geom_text_repel(data = fig3_labels, aes(label = year),
                  size = 3.2, colour = COL_GREY,
                  min.segment.length = 0, segment.colour = "grey70",
                  box.padding = 0.7, point.padding = 0.3, force = 3,
                  seed = 42, max.overlaps = Inf) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(xlim = c(400, 1300), clip = "off") +
  labs(
    title    = "Wetter years bring more farmer-herder conflict, not less",
    subtitle = "Each point is one year (1997–2022); a positive slope contradicts the standard drought-conflict story",
    x        = "Annual precipitation (mm)",
    y        = "Farmer-herder conflict events",
    caption  = "Regression: precipitation ~ farmer-herder events, p = 0.024, R² = 0.19. Sources: ACLED, World Bank."
  ) +
  theme_thesis()

ggsave(here("figures", "03_rainfall_pastoral_conflict.png"),
       plot = fig3, width = 9, height = 6, dpi = 200, bg = "white")


# ============================================================================
# FIGURE 4: Mean conflict count by protected area designation
# ============================================================================

message("Building Figure 4: conflicts by protected area designation...")

fig4_data <- conflicts_by_pa %>%
  filter(!is.na(total_conflicts), area_sqkm > 0) %>%
  group_by(DESIG) %>%
  summarise(
    n_pas       = n(),
    mean_events = mean(total_conflicts, na.rm = TRUE),
    total       = sum(total_conflicts, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  filter(n_pas >= 3) %>%
  arrange(mean_events)

fig4 <- fig4_data %>%
  mutate(DESIG_wrapped = str_wrap(DESIG, width = 25),
         DESIG_wrapped = factor(DESIG_wrapped, levels = DESIG_wrapped)) %>%
  ggplot(aes(x = mean_events, y = DESIG_wrapped)) +
  geom_col(fill = COL_PRIMARY, width = 0.7) +
  geom_text(aes(label = paste0(round(mean_events, 1), " (n=", n_pas, ")")),
            hjust = -0.1, size = 3.2, colour = COL_GREY) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Community Nature Reserves see far more conflict events than other designations",
    subtitle = "Mean total conflict events per protected area, by designation type (n ≥ 3)",
    x        = "Mean conflict events per protected area",
    y        = NULL,
    caption  = "Sources: ACLED (1997–2024), UCDP GED (1989–2023), NRT (2018–Sept 2022 partial snapshot). Spatial joins to WDPA. Author's analysis."
    ) +
  theme_thesis()

ggsave(here("figures", "04_conflicts_by_designation.png"),
       plot = fig4, width = 10, height = 6, dpi = 200, bg = "white")


# ============================================================================
# FIGURE 5: Top 15 protected areas by total conflict count
# ============================================================================

message("Building Figure 5: top 15 protected areas by conflicts...")

fig5_data <- conflicts_by_pa %>%
  filter(!is.na(total_conflicts), total_conflicts > 0) %>%
  arrange(desc(total_conflicts)) %>%
  head(15) %>%
  mutate(
    NAME  = str_trunc(NAME, 40),
    NAME  = factor(NAME, levels = rev(NAME))
  )

fig5 <- fig5_data %>%
  ggplot(aes(x = total_conflicts, y = NAME, fill = DESIG)) +
  geom_col(width = 0.75) +
  scale_fill_brewer(palette = "Set2", name = "Designation") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "The 15 protected areas with the most recorded conflict events",
    subtitle = "Community conservancies in northern Kenya dominate, alongside major forest reserves",
    x        = "Total conflict events (ACLED + UCDP + NRT)",
    y        = NULL,
    caption  = "Note: NRT event totals reflect a Sept 2022 partial snapshot; see repo README for details."
  ) +
  theme_thesis() +
  theme(legend.position = "right")

ggsave(here("figures", "05_top_protected_areas.png"),
       plot = fig5, width = 10, height = 7, dpi = 200, bg = "white")


# ============================================================================
# FIGURE 6: Kenya map — farmer-herder conflicts and protected areas
# ============================================================================

message("Building Figure 6: map of pastoral conflicts and protected areas...")

# Kenya national boundary from Natural Earth
kenya_boundary <- rnaturalearth::ne_countries(
  country = "Kenya",
  scale   = "medium",
  returnclass = "sf"
) %>%
  st_transform(4326)

# Major cities in Kenya for geographic context
# Major cities in Kenya, hardcoded for reliability
# Coordinates from Wikipedia / GeoNames
kenya_cities <- tibble::tibble(
  name = c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret",
           "Lodwar", "Marsabit", "Garissa", "Meru"),
  lon  = c(36.8219, 39.6682, 34.7617, 36.0800, 35.2698,
           35.5966, 37.9908, 39.6461, 37.6559),
  lat  = c(-1.2921, -4.0435, -0.0917, -0.3031,  0.5143,
           3.1191,  2.3284, -0.4536,  0.0500)
) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Ensure conflict + PA layers are in WGS84 for plotting
wdpa_wgs          <- st_transform(wdpa, 4326)
farmer_herder_wgs <- st_transform(farmer_herder_sf, 4326)

fig6 <- ggplot() +
  # Kenya outline as base
  geom_sf(data = kenya_boundary, fill = "#FAF7F2", colour = "grey30", linewidth = 0.4) +
  # Protected areas in muted green
  geom_sf(data = wdpa_wgs, fill = COL_ACCENT, colour = NA, alpha = 0.55) +
  # Pastoral conflict points
  geom_sf(data = farmer_herder_wgs, colour = COL_SECONDARY,
          size = 1.1, alpha = 0.6) +
  # City markers
  # City markers — bigger, darker for visibility over green polygons
  geom_sf(data = kenya_cities, shape = 21, size = 3,
          fill = "grey10", colour = "white", stroke = 0.9) +
  # City labels with white background box for readability
  ggrepel::geom_label_repel(
    data = kenya_cities,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.5, colour = "grey10", fontface = "bold",
    fill = alpha("white", 0.88), label.size = 0,
    label.padding = unit(0.15, "lines"),
    min.segment.length = 0, segment.colour = "grey40",
    segment.size = 0.4, box.padding = 0.6, seed = 42
  ) +
  coord_sf(xlim = c(33.9, 41.9), ylim = c(-4.7, 5.5), expand = FALSE) +
  labs(
    title    = "Farmer-herder conflicts cluster in northern Kenya's protected areas",
    subtitle = paste0("Green polygons: WDPA protected areas (n = ",
                      nrow(wdpa_wgs), "). Red points: pastoral conflict events (n = ",
                      nrow(farmer_herder_wgs), ")."),
    x        = NULL,
    y        = NULL,
    caption  = "Sources: ACLED, WDPA (Jan 2024), Natural Earth (boundary and cities). Projection: WGS84."
  ) +
  theme_thesis() +
  theme(
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
    panel.background = element_rect(fill = "grey98", colour = NA),
    axis.text        = element_text(colour = "grey60", size = 9)
  )

ggsave(here("figures", "06_pastoral_conflicts_map.png"),
       plot = fig6, width = 9, height = 9, dpi = 200, bg = "white")


# ============================================================================
# DONE
# ============================================================================

message("\nDone. Figures saved to figures/:")
message("  01_farmer_herder_share.png")
message("  02_ndvi_election_interaction.png")
message("  03_rainfall_pastoral_conflict.png")
message("  04_conflicts_by_designation.png")
message("  05_top_protected_areas.png")
message("  06_pastoral_conflicts_map.png")