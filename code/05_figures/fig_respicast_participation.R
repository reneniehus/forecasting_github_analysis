# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Supporting figure: how many models, and how many EU/EEA countries? ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Two stacked time series on an IDENTICAL x axis, so a reader can read straight down
# from a week's model count to the country coverage the ensemble achieved that week:
#   (A) contributing models per weekly round
#   (B) EU/EEA countries covered by the published ensemble
#
# Everything is restricted to EU/EEA locations (see extract_respicast_eu.R). That is
# what exposes the 2026 outage: through July 2026 the syndromic ensemble was still
# produced, but only for Switzerland, England and Northern Ireland -- non-EU countries
# fed by WHO FluID rather than ERVISS -- so member states saw nothing for seven rounds.
#
# Two kinds of blank are drawn differently, which is why the extract carries hub_ran:
#   hub running, no EU/EEA coverage  -> a genuine ZERO (the 2026 transition)
#   hub not running at all           -> a BREAK in the line (summer 2024, between
#                                       the 2023/24 hubs and the current repos)
#
# Colours are the Okabe-Ito colourblind-safe set.
# Run (after code/03_hubs/extract_respicast_eu.R):
#   Rscript code/05_figures/fig_respicast_participation.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

IND <- c("ILI incidence", "ARI incidence", "COVID-19 hospitalisations")
COL <- c("ILI incidence"             = "#D55E00",   # Okabe-Ito vermillion
         "ARI incidence"             = "#009E73",   # Okabe-Ito bluish green
         "COVID-19 hospitalisations" = "#0072B2")   # Okabe-Ito blue
INK <- "#2b2b28"; MUTED <- "#6f6e69"; RULE <- "#d8d7d1"; BAND <- "#f2f1ec"
# annotation colour for the 2026 gap: a muted purple, deliberately NOT one of the
# three indicator hues (vermillion / green / blue), so the band, the asterisk and the
# footnote read as annotation rather than a fourth data series
GAP <- "#7A5195"

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
          plot.margin        = margin(4, 12, 4, 6))
}

w <- read_csv(file.path(params$output_dir, "respicast_eu_weekly.csv"), show_col_types = FALSE) %>%
  mutate(week = as.Date(week), indicator = factor(indicator, levels = IND))

# End on the last round every hub has completed. The current week's ensemble runs
# Wednesday 23:40 UTC, so including it would draw a false collapse to zero.
LAST  <- w %>% group_by(indicator) %>% summarise(m = max(week[eu_countries > 0]), .groups = "drop")
XLIM  <- c(min(w$week), min(LAST$m))
GAP_X <- c(as.Date("2026-06-22"), as.Date("2026-08-03"))   # 7 rounds, EU/EEA coverage = 0

bands <- tibble(xmin = as.Date(paste0(sort(unique(lubridate::year(w$week))), "-10-01")),
                xmax = as.Date(paste0(sort(unique(lubridate::year(w$week))) + 1, "-03-31"))) %>%
  filter(xmin <= XLIM[2], xmax >= XLIM[1]) %>%
  mutate(xmin = pmax(xmin, XLIM[1]), xmax = pmin(xmax, XLIM[2]))

grid <- expand_grid(indicator = factor(IND, levels = IND),
                    week = seq(XLIM[1], XLIM[2], by = 7)) %>%
  left_join(select(w, indicator, week, n_models, eu_countries, hub_ran), by = c("indicator", "week")) %>%
  mutate(hub_ran      = ifelse(is.na(hub_ran), FALSE, hub_ran),
         n_models     = ifelse(hub_ran, n_models,     NA_integer_),   # NA -> line breaks
         eu_countries = ifelse(hub_ran, eu_countries, NA_integer_))

LAUNCH   <- XLIM[1]                       # first round of the earliest RespiCast hub
launchln <- geom_vline(xintercept = LAUNCH, linetype = "dotted",
                       linewidth = 0.5, colour = MUTED)

x_scale  <- scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = XLIM,
                         expand = expansion(mult = c(0.02, 0.02)))
winter   <- geom_rect(data = bands, inherit.aes = FALSE,
                      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = BAND, alpha = 0.9)
yearline <- geom_vline(xintercept = seq(as.Date(paste0(lubridate::year(XLIM[1]) + 1, "-01-01")),
                                        XLIM[2], by = "1 year"),
                       linewidth = 0.35, colour = "#a9a8a2")
# the 2026 transition gap, drawn above the winter band so it stays visible inside it
gapband  <- annotate("rect", xmin = GAP_X[1], xmax = GAP_X[2] + 6,
                     ymin = -Inf, ymax = Inf, fill = GAP, alpha = 0.16)
gapstar  <- function(y) annotate("text", x = mean(GAP_X), y = y, label = "*",
                                 size = 5, colour = GAP, fontface = "bold")

# ---- |-(A) contributing models per week ----
pA <- ggplot(grid, aes(week, n_models, colour = indicator)) +
  winter + yearline + gapband + gapstar(19.6) + launchln +
  annotate("text", x = LAUNCH + 22, y = 10.5, label = "RespiCast launch",
           angle = 90, hjust = 0, vjust = 0.5, size = 2.9, colour = MUTED) +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  scale_colour_manual(values = COL, drop = FALSE) +
  x_scale +
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 21), expand = c(0, 0)) +
  labs(tag = "A", title = "Contributing models per weekly round", x = NULL, y = "Models") +
  theme_sci() + theme(axis.text.x = element_blank())

# ---- |-(B) EU/EEA countries covered by the ensemble ----
pB <- ggplot(grid, aes(week, eu_countries, colour = indicator)) +
  winter + yearline + gapband + gapstar(28) + launchln +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  scale_colour_manual(values = COL, drop = FALSE) +
  x_scale +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 31), expand = c(0, 0)) +
  labs(tag = "B", title = "EU/EEA countries covered by the published ensemble",
       x = NULL, y = "Countries") +
  theme_sci()

fig <- pA / pB +
  patchwork::plot_layout(heights = c(1, 1), guides = "collect") +
  patchwork::plot_annotation(
    caption = "* 24 Jun - 5 Aug 2026: seven weeks without ensembles for EU/EEA countries, following ECDC's transition of\n   surveillance reporting from TESSy to EpiPulse Cases.",
    theme = theme(plot.caption = element_text(hjust = 0, colour = GAP, size = 8.4, lineheight = 1.1),
                  plot.caption.position = "plot")) &
  theme(legend.position = "top", legend.title = element_blank(),
        legend.key.width = unit(14, "pt"), legend.margin = margin(0, 0, 2, 0),
        legend.text = element_text(colour = INK, size = 9))

ggsave(file.path(params$figure_dir, "respicast_participation.png"), fig,
       width = 8.6, height = 5.9, dpi = 200, bg = "white")
cat("figure -> output/figures/respicast_participation.png\n")
