# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Continuity of COVID-19 hospitalisation forecasting, 2021-2026 ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Evidence for "produced nearly uninterrupted since 2021". Every repository that has
# ever carried a COVID-19 hospital-admissions forecast is included; there are exactly
# two (verified against all 23 european-modelling-hubs repos -- the rest hold tooling,
# websites or scenario projections, not time-stamped forecasts):
#   covid19-forecast-hub-europe_archive  European COVID-19 Forecast Hub, 2021-07 -> 2024-10
#   RespiCast-Covid19                    2024-10 -> present
#
# Two panels, because the claim is true at one level and not the other:
#   (A) were forecasts SUBMITTED that week?   -> 255 of 257 weeks (99.2%)
#   (B) was an ENSEMBLE published that week?  -> 215 of 257 weeks (84.3%)
# Reporting only (A) would imply the published product was equally continuous.
#
# Run:  Rscript code/05_figures/fig_covid_continuity.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

ERA_COL <- c("European COVID-19 Forecast Hub" = "#8aa6bf",   # muted, the predecessor
             "RespiCast-Covid19"              = "#0072B2")   # Okabe-Ito blue, as elsewhere
GAP  <- "#D55E00"                                            # Okabe-Ito vermillion
INK  <- "#2b2b28"; MUTED <- "#6f6e69"; RULE <- "#d8d7d1"

theme_sci <- function(base = 10.5) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.25, colour = RULE),
          axis.title         = element_text(colour = MUTED, size = base - 0.5),
          axis.text          = element_text(colour = MUTED),
          plot.title         = element_text(face = "plain", colour = INK, size = base + 0.5),
          plot.tag           = element_text(face = "bold", colour = INK, size = base + 1),
          plot.tag.position  = c(0, 1),
          plot.margin        = margin(4, 10, 4, 6))
}

# ---- |-weekly record, both repositories, on one ISO-week grid ----
s <- read_csv(file.path(params$output_dir, "hub_submissions.csv"), show_col_types = FALSE) %>%
  filter(indicator == "COVID-19 hospitalisations") %>%
  mutate(origin_date = as.Date(origin_date),
         week = lubridate::floor_date(origin_date, "week", week_start = 1))

wk <- s %>% group_by(week) %>%
  summarise(models = n_distinct(model[role == "model"]),
            ens    = any(role == "ensemble"),
            era    = if (any(era == "current")) "RespiCast-Covid19" else "European COVID-19 Forecast Hub",
            .groups = "drop")

XLIM <- c(min(wk$week), max(wk$week))
full <- tibble(week = seq(XLIM[1], XLIM[2], by = 7)) %>%      # every Monday, so gaps are explicit
  left_join(wk, by = "week") %>%
  mutate(era = factor(era, levels = names(ERA_COL)),
         ens_lab = ifelse(is.na(models), "No forecast at all",
                          ifelse(ens, "Ensemble published", "Forecasts but no ensemble")))

gaps <- filter(full, is.na(models))

x_scale <- scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = XLIM,
                        expand = expansion(mult = c(0.015, 0.015)))
year_starts <- seq(as.Date(paste0(lubridate::year(XLIM[1]) + 1, "-01-01")), XLIM[2], by = "1 year")
yearline <- geom_vline(xintercept = year_starts, linewidth = 0.35, colour = "#c9c8c2")

# ---- |-(A) forecasts submitted each week ----
pA <- ggplot(full, aes(week, models, fill = era)) +
  yearline +
  geom_col(width = 7, na.rm = TRUE) +
  geom_point(data = gaps, aes(x = week, y = 0.4), inherit.aes = FALSE,
             colour = GAP, size = 1.5, shape = 17) +
  scale_fill_manual(values = ERA_COL, drop = FALSE) +
  x_scale +
  scale_y_continuous(breaks = seq(0, 12, 4), limits = c(0, 12.5), expand = c(0, 0)) +
  labs(tag = "A", title = "Forecasts submitted per weekly round",
       subtitle = "255 of 257 weeks covered over 58 months; the two weeks with no forecast are marked \u25b2",
       x = NULL, y = "Models") +
  theme_sci() +
  theme(axis.text.x = element_blank(),
        plot.subtitle = element_text(colour = MUTED, size = 8.8),
        legend.position = "top", legend.title = element_blank(),
        legend.key.size = unit(9, "pt"), legend.margin = margin(0, 0, 2, 0),
        legend.text = element_text(size = 8.5, colour = INK))

# ---- |-(B) was an ensemble actually published? ----
STRIP <- c("Ensemble published"        = "#0072B2",
           "Forecasts but no ensemble" = "#cdd7e0",
           "No forecast at all"        = GAP)
pB <- ggplot(full, aes(week, y = 1, fill = ens_lab)) +
  geom_tile(width = 7, height = 1) +
  scale_fill_manual(values = STRIP, name = NULL) +
  x_scale +
  labs(tag = "B", title = "Was an ensemble published that week?", x = NULL, y = NULL) +
  theme_sci() +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        legend.position = "bottom", legend.key.size = unit(9, "pt"),
        legend.text = element_text(size = 8.5, colour = INK), legend.margin = margin(0, 0, 0, 0))

fig <- pA / pB + patchwork::plot_layout(heights = c(3.2, 1))
ggsave(file.path(params$figure_dir, "covid_hosp_continuity.png"), fig,
       width = 8.6, height = 4.4, dpi = 200, bg = "white")
cat("figure -> output/figures/covid_hosp_continuity.png\n")
