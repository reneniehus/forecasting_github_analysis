# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### ILI: reported surveillance against the ensemble's Modelled Smooth Point ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Three example countries, each on its own y scale because national ILI consultation
# rates are not comparable in level (Greece reports two orders of magnitude above
# Belgium; the definitions and denominators differ).
#
# Per panel:
#   black   the five most recent reported ERVISS weeks, dots joined by lines
#   turquoise  the MSP slope across the last two weeks -- MSP(W36) -> MSP(W37)
#   a rule at today, which sits two weeks past the newest reported week
#
# The gap between the black line's end and the rule is the reporting lag, and it is
# the reason the MSP exists: the turquoise segment is the ensemble's smooth read of
# weeks that surveillance either has only just reported or has not yet settled.
#
# Run (after code/03_hubs/compute_msp.R):  Rscript code/05_figures/fig_msp_ili.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

# Swap these to re-point the figure. Chosen to show three different agreements between
# what was reported and what the ensemble makes of it: Denmark rising and the MSP
# agreeing, Belgium rising through a noisy series, Greece falling while the MSP turns up.
COUNTRIES <- c(DK = "Denmark", BE = "Belgium", GR = "Greece")
N_OBS     <- 5                                   # surveillance points to show
TODAY     <- as.Date("2026-09-21")

TURQ  <- "#00A0A8"                               # the MSP
INK   <- "#1f1f1c"; MUTED <- "#6f6e69"; RULE <- "#dcdbd5"; LATE <- "#f4f3ee"

truth <- read_csv(Sys.getenv("ILI_TRUTH",
                    "/workspace/emh-syndromic/target-data/ERVISS/latest-ILI_incidence.csv"),
                  show_col_types = FALSE) %>%
  filter(location %in% names(COUNTRIES), !is.na(value)) %>%
  mutate(week_end = as.Date(truth_date))

msp <- read_csv(file.path(params$output_dir, "msp_weekly.csv"), show_col_types = FALSE) %>%
  filter(indicator == "ILI incidence", location %in% names(COUNTRIES)) %>%
  mutate(week_end = as.Date(week_end))

# The two weeks the MSP slope spans: the newest reported week and the one before it.
LAST_OBS <- max(truth$week_end)
SLOPE_WK <- c(LAST_OBS - 7, LAST_OBS)
OBS_WKS  <- seq(LAST_OBS - 7 * (N_OBS - 1), LAST_OBS, by = 7)

obs   <- filter(truth, week_end %in% OBS_WKS)
slope <- filter(msp,   week_end %in% SLOPE_WK)

stopifnot(nrow(obs) == length(COUNTRIES) * N_OBS, nrow(slope) == length(COUNTRIES) * 2)

wk_lab <- function(d) sprintf("W%02d", lubridate::isoweek(d))
# run the axis one week past today so W39, the week we are in, carries a tick
XLIM   <- c(min(OBS_WKS) - 3, TODAY + 9)
BREAKS <- seq(min(OBS_WKS), TODAY + 6, by = 7)

panel <- function(code) {
  o <- filter(obs, location == code)
  s <- filter(slope, location == code) %>% arrange(week_end)
  hi <- max(c(o$value, s$msp)); lo <- min(c(o$value, s$msp))
  pad <- 0.18 * (hi - lo + 1e-9)

  ggplot() +
    # everything after the newest reported week is not yet observed
    annotate("rect", xmin = LAST_OBS + 3.5, xmax = XLIM[2], ymin = -Inf, ymax = Inf,
             fill = LATE, colour = NA) +
    geom_vline(xintercept = TODAY, linetype = "dashed", linewidth = 0.45, colour = MUTED) +
    annotate("text", x = TODAY - 1.6, y = hi + pad * 0.95, label = "today (W39)",
             angle = 90, hjust = 1, vjust = 0.5, size = 2.8, colour = MUTED) +
    geom_line(data = o, aes(week_end, value), colour = INK, linewidth = 0.5) +
    geom_point(data = o, aes(week_end, value), colour = INK, size = 1.9) +
    geom_line(data = s, aes(week_end, msp), colour = TURQ, linewidth = 1) +
    geom_point(data = s, aes(week_end, msp), colour = TURQ, size = 2.3) +
    scale_x_date(breaks = BREAKS, labels = wk_lab, limits = XLIM, expand = c(0, 0)) +
    scale_y_continuous(limits = c(max(0, lo - pad), hi + pad),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = COUNTRIES[[code]], x = NULL,
         y = if (code == names(COUNTRIES)[1]) "ILI consultations per 100 000" else NULL) +
    theme_minimal(base_size = 10.5) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.25, colour = RULE),
          axis.text          = element_text(colour = MUTED, size = 8.6),
          axis.title.y       = element_text(colour = MUTED, size = 8.6),
          plot.title         = element_text(colour = INK, size = 10.5, face = "plain"),
          plot.margin        = margin(4, 8, 2, 4))
}

panels <- wrap_plots(map(names(COUNTRIES), panel), nrow = 1)

# A drawn key rather than a ggplot legend: the two series come from different layers
# with no shared aesthetic, and a hand-built key can show the MSP as the slope it is.
key <- ggplot() +
  annotate("segment", x = 0.02, xend = 0.075, y = 0.5, yend = 0.5, colour = INK, linewidth = 0.5) +
  annotate("point", x = c(0.02, 0.0475, 0.075), y = 0.5, colour = INK, size = 1.9) +
  annotate("text", x = 0.088, y = 0.5, hjust = 0, size = 3.1, colour = INK,
           label = "Reported ILI (ERVISS surveillance)") +
  annotate("segment", x = 0.52, xend = 0.575, y = 0.42, yend = 0.58, colour = TURQ, linewidth = 1) +
  annotate("point", x = c(0.52, 0.575), y = c(0.42, 0.58), colour = TURQ, size = 2.3) +
  annotate("text", x = 0.588, y = 0.5, hjust = 0, size = 3.1, colour = INK,
           label = "Modelled Smooth Point (RespiCast ensemble)") +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) +
  theme_void()

fig <- key / panels +
  plot_layout(heights = c(0.1, 1)) +
  plot_annotation(
    title    = "ILI incidence: reported surveillance and the ensemble's modelled smooth point",
    subtitle = paste0("Weeks ", wk_lab(min(OBS_WKS)), "-", wk_lab(LAST_OBS),
                      " of 2026. Newest reported week is ", wk_lab(LAST_OBS),
                      " (ending ", format(LAST_OBS, "%d %b"),
                      "); the shaded strip is the reporting lag, two weeks wide today."),
    caption  = paste0(
      "MSP = the ensemble's 1- and 2-week-ahead medians extrapolated back one week on a log scale (MSP = f1^2 / f2), ",
      "giving its smooth read of a\nweek without leaning on that week's own provisional count. ",
      "The ", wk_lab(LAST_OBS), " MSP has no round anchored on it yet, so it is the 1-week-ahead median of the ",
      format(max(msp$origin_date), "%d %b"), " round.\n",
      "y axes are free: national ILI consultation rates differ in definition and denominator and are not comparable in level. ",
      "Source: RespiCast + ERVISS, retrieved ", format(TODAY, "%d %b %Y"), "."),
    theme = theme(
      plot.title    = element_text(colour = INK, size = 12.5, face = "plain"),
      plot.subtitle = element_text(colour = MUTED, size = 9, margin = margin(b = 4)),
      plot.caption  = element_text(colour = MUTED, size = 7.4, hjust = 0, lineheight = 1.25),
      plot.caption.position = "plot", plot.title.position = "plot"))

ggsave(file.path(params$figure_dir, "msp_ili_examples.png"), fig,
       width = 9.6, height = 4.2, dpi = 500, bg = "white")
cat("figure -> output/figures/msp_ili_examples.png\n")

step("What the figure shows")
bind_rows(obs %>% transmute(location, week = wk_lab(week_end), series = "reported", value),
          slope %>% transmute(location, week = wk_lab(week_end), series = "MSP", value = msp)) %>%
  pivot_wider(names_from = week, values_from = value) %>%
  arrange(location, series) %>% as.data.frame() %>% print(row.names = FALSE)
