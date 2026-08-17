# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Supporting figure: how many models, and how many countries, feed RespiCast? ##
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Two stacked time series on an IDENTICAL x axis, so a reader can read straight down
# from a week's model count to the country coverage the ensemble achieved that week:
#   (A) contributing models per weekly round
#   (B) countries for which an ensemble was actually published that week
#
# Colours are the Okabe-Ito colourblind-safe set. The project's earlier violet/blue
# pair for ARI/COVID sat too close in hue to separate reliably in a line chart, so
# ARI moves to bluish-green; the hues are now ~120 deg apart and stay distinct under
# deuteranopia and protanopia.
#
# RespiCast here = every hub EXCEPT the predecessor EU COVID-19 Forecast Hub. The
# 2023/24 flu and ARI hubs are included because they already published under the
# respicast-hubEnsemble name; the EU COVID-19 hub is a separate, earlier programme.
#
# Run:  Rscript code/05_figures/fig_respicast_participation.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

IND <- c("ILI incidence", "ARI incidence", "COVID-19 hospitalisations")
COL <- c("ILI incidence"             = "#D55E00",   # Okabe-Ito vermillion
         "ARI incidence"             = "#009E73",   # Okabe-Ito bluish green
         "COVID-19 hospitalisations" = "#0072B2")   # Okabe-Ito blue
INK <- "#2b2b28"; MUTED <- "#6f6e69"; RULE <- "#d8d7d1"; BAND <- "#f2f1ec"

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

# ---- |-data: RespiCast era only ----
w <- read_csv(file.path(params$output_dir, "hub_coverage_weekly.csv"), show_col_types = FALSE) %>%
  filter(!grepl("covid_archive", hub)) %>%
  mutate(week = as.Date(week), indicator = factor(indicator, levels = IND))

XLIM <- c(min(w$week), max(w$week))

# winter bands, clamped to the plotted range
yrs   <- sort(unique(lubridate::year(w$week)))
bands <- tibble(xmin = as.Date(paste0(yrs, "-10-01")),
                xmax = as.Date(paste0(yrs + 1, "-03-31"))) %>%
  filter(xmin <= XLIM[2], xmax >= XLIM[1]) %>%
  mutate(xmin = pmax(xmin, XLIM[1]), xmax = pmin(xmax, XLIM[2]))

# complete weekly grid -> NA on weeks a hub did not run, so lines BREAK over the
# summer-2024 gap between hub generations instead of interpolating across it.
grid <- expand_grid(indicator = factor(IND, levels = IND),
                    week = seq(XLIM[1], XLIM[2], by = 7)) %>%
  left_join(select(w, indicator, week, n_models, has_ensemble, ensemble_locations),
            by = c("indicator", "week")) %>%
  # countries only count for weeks an ensemble was actually published
  mutate(ens_countries = ifelse(!is.na(has_ensemble) & has_ensemble, ensemble_locations, NA_real_))

# one x scale object, reused verbatim by both panels so they align exactly
x_scale <- scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
                        limits = XLIM, expand = expansion(mult = c(0.02, 0.02)))
winter <- geom_rect(data = bands, inherit.aes = FALSE,
                    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                    fill = BAND, alpha = 0.9)

# ---- |-(A) contributing models per week ----
pA <- ggplot(grid, aes(week, n_models, colour = indicator)) +
  winter +
  geom_hline(yintercept = 5, linetype = "22", linewidth = 0.4, colour = MUTED) +
  annotate("text", x = as.Date("2024-06-25"), y = 6.0, label = "5 models", hjust = 0,
           size = 2.8, colour = MUTED) +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  scale_colour_manual(values = COL, drop = FALSE) +
  x_scale +
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 21), expand = c(0, 0)) +
  labs(tag = "A", title = "Contributing models per weekly round", x = NULL, y = "models") +
  theme_sci() +
  theme(axis.text.x = element_blank())     # x labels live on panel B only

# ---- |-(B) countries covered by the published ensemble ----
pB <- ggplot(grid, aes(week, ens_countries, colour = indicator)) +
  winter +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  scale_colour_manual(values = COL, drop = FALSE) +
  x_scale +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 33), expand = c(0, 0)) +
  labs(tag = "B", title = "Countries covered by the published ensemble", x = NULL, y = "countries") +
  theme_sci()

fig <- pA / pB +
  patchwork::plot_layout(heights = c(1, 1), guides = "collect") &
  theme(legend.position = "top", legend.title = element_blank(),
        legend.key.width = unit(14, "pt"), legend.margin = margin(0, 0, 2, 0),
        legend.text = element_text(colour = INK, size = 9))

ggsave(file.path(params$figure_dir, "respicast_participation.png"), fig,
       width = 8.6, height = 5.6, dpi = 200, bg = "white")
cat("figure -> output/figures/respicast_participation.png\n")
