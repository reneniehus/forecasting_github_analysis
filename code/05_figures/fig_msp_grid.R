# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### EU/EEA grid: reported surveillance vs the ensemble's MSP, with year-ago slopes ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Small multiples, one panel per EU/EEA country, built to be re-run unchanged every
# week: the reporting vintage, the weeks shown and the comparison weeks are all derived
# from the data, never hard-coded.
#
# Per panel:
#   black      the last N_OBS reported weeks, dots joined by lines
#   turquoise  the MSP slope across the final two of those weeks -- this week's read
#   grey       the SAME two calendar weeks one and two years earlier, lines only,
#              drawn at the current x positions so the slopes overlay
#
# "Exactly one year ago" is 364 days (52 whole weeks), not a calendar year: it keeps the
# Sunday week-ending alignment and lands on the same ISO week number. Verified for the
# current vintage -- 2026-W36/W37 maps to 2025-W36/W37 and 2024-W36/W37.
#
# Panels are ordered by the modelled weekly change, steepest rise first, so the grid
# reads as a ranking; the strip carries that change as a percentage.
#
# Levels are NOT comparable between countries -- national ILI consultation rates differ
# in case definition and denominator -- hence free y scales and no shared axis.
#
# y axes are log10, which is the scale the MSP is defined on: a log-linear extrapolation
# plots as a straight line, and equal gradients mean equal weekly growth rates whatever
# the level. On a linear axis a grey slope sitting at twice the current level looks twice
# as steep for the same growth, which is exactly the comparison this figure is for.
# The cost is that zeros cannot be drawn, so a country reporting only zeros is dropped.
#
# Re-run weekly:      Rscript code/05_figures/fig_msp_grid.R
# Other indicators:   INDICATOR="ARI incidence" Rscript code/05_figures/fig_msp_grid.R
#                     INDICATOR="COVID-19 hospitalisations" Rscript code/05_figures/fig_msp_grid.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

INDICATOR <- Sys.getenv("INDICATOR", "ILI incidence")
N_OBS     <- 4L                      # reported weeks to draw
LAG_YEARS <- c(1L, 2L)               # historical slopes to overlay, in whole years
MIN_OBS   <- 2L                      # a country needs at least this many NON-ZERO reported points

# Countries withheld, with the reason. LU's ERVISS series alternates between 0 and 2200
# within a single month, which is a reporting artefact rather than epidemiology; plotting
# it would put a meaningless panel in a grid meant to be read at a glance.
EXCLUDE <- c(LU = "ERVISS series alternates 0 / 2200 -- reporting artefact")

TRUTH <- c("ILI incidence"             = "/workspace/emh-syndromic/target-data/ERVISS/latest-ILI_incidence.csv",
           "ARI incidence"             = "/workspace/emh-syndromic/target-data/ERVISS/latest-ARI_incidence.csv",
           "COVID-19 hospitalisations" = "/workspace/emh-covid/target-data/latest-hospital_admissions.csv")
UNITS <- c("ILI incidence"             = "ILI consultations per 100 000",
           "ARI incidence"             = "ARI consultations per 100 000",
           "COVID-19 hospitalisations" = "Weekly hospital admissions")

NAME <- c(AT="Austria", BE="Belgium", BG="Bulgaria", CY="Cyprus", CZ="Czechia", DE="Germany",
          DK="Denmark", EE="Estonia", ES="Spain", FI="Finland", FR="France", GR="Greece",
          HR="Croatia", HU="Hungary", IE="Ireland", IS="Iceland", IT="Italy", LI="Liechtenstein",
          LT="Lithuania", LU="Luxembourg", LV="Latvia", MT="Malta", NL="Netherlands", NO="Norway",
          PL="Poland", PT="Portugal", RO="Romania", SE="Sweden", SI="Slovenia", SK="Slovakia")

INK  <- "#1f1f1c"; MUTED <- "#6f6e69"; RULE <- "#e3e2dc"
SER  <- c("Reported (ERVISS)"   = INK,
          "Modelled, this week" = "#00A0A8",      # turquoise: the current MSP slope
          "Modelled, 1 year ago"  = "#9a9a94",    # two greys, the older one lighter
          "Modelled, 2 years ago" = "#c8c7c1")

# ---- |-1. data, with the vintage read off the files ----
step(sprintf("Building the %s grid", INDICATOR))

truth <- read_csv(TRUTH[[INDICATOR]], show_col_types = FALSE) %>%
  filter(!is.na(value)) %>%
  mutate(week_end = as.Date(truth_date))

msp <- read_csv(file.path(params$output_dir, "msp_weekly.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR, eu_eea) %>%
  mutate(week_end = as.Date(week_end))

LAST_OBS <- max(truth$week_end)                       # newest reported week, from the data
OBS_WKS  <- seq(LAST_OBS - 7 * (N_OBS - 1), LAST_OBS, by = 7)
SLOPE_WK <- c(LAST_OBS - 7, LAST_OBS)                 # the two weeks the MSP slope spans
say(sprintf("newest reported week %s (%s); showing %s -> %s",
            format(LAST_OBS), sprintf("W%02d", lubridate::isoweek(LAST_OBS)),
            format(min(OBS_WKS)), format(LAST_OBS)))

# ---- |-2. who qualifies ----
obs_all <- truth %>% filter(week_end %in% OBS_WKS, location %in% unique(msp$location), value > 0)
now     <- msp %>% filter(week_end %in% SLOPE_WK, msp > 0)

eligible <- full_join(
    count(obs_all, location, name = "n_obs"),
    now %>% count(location, name = "n_msp"), by = "location") %>%
  mutate(across(c(n_obs, n_msp), ~ replace_na(.x, 0L)),
         excluded = location %in% names(EXCLUDE),
         keep     = n_msp == 2 & n_obs >= MIN_OBS & !excluded)

say(sprintf("%d EU/EEA countries kept; dropped %d (no positive MSP pair or < %d positive reported points), %d withheld",
            sum(eligible$keep), sum(!eligible$keep & !eligible$excluded), MIN_OBS, sum(eligible$excluded)))
KEEP <- eligible$location[eligible$keep]

# ---- |-3. the historical slopes, shifted onto the current x positions ----
# 364 days = 52 whole weeks, so the Sunday alignment and the ISO week number both hold.
lagged <- map_dfr(LAG_YEARS, function(k) {
  msp %>%
    filter(week_end %in% (SLOPE_WK - 364L * k), location %in% KEEP, msp > 0) %>%
    transmute(location, week_end = week_end + 364L * k, value = msp,
              series = sprintf("Modelled, %d year%s ago", k, ifelse(k == 1, "", "s")))
})

for (k in LAG_YEARS) {
  lab <- sprintf("Modelled, %d year%s ago", k, ifelse(k == 1, "", "s"))
  n   <- n_distinct(lagged$location[lagged$series == lab])
  say(sprintf("%-22s %s -> %d countries", lab,
              paste(format(SLOPE_WK - 364L * k), collapse = " & "), n))
}

plot_df <- bind_rows(
  obs_all %>% filter(location %in% KEEP) %>% transmute(location, week_end, value, series = "Reported (ERVISS)"),
  now     %>% filter(location %in% KEEP) %>% transmute(location, week_end, value = msp, series = "Modelled, this week"),
  lagged) %>%
  mutate(series = factor(series, levels = names(SER)))

# ---- |-4. order the panels by the modelled weekly change ----
growth <- now %>% filter(location %in% KEEP) %>%
  select(location, week_end, msp) %>%
  pivot_wider(names_from = week_end, values_from = msp) %>%
  rename(prev = 2, last = 3) %>%
  mutate(chg = last / prev - 1) %>%
  arrange(desc(chg)) %>%
  mutate(strip = sprintf("%s   %+.0f%%", NAME[location], 100 * chg))

plot_df <- plot_df %>%
  left_join(select(growth, location, strip), by = "location") %>%
  mutate(strip = factor(strip, levels = growth$strip))

# ---- |-5. draw ----
drawn <- levels(droplevels(plot_df$series))
# 4 columns: with the usual 12-16 reporting countries this fills every row, so no
# column is left without an x axis under it
NCOL  <- 4L

p <- ggplot(plot_df, aes(week_end, value, colour = series)) +
  # historical first, current on top, reported points last so nothing hides them
  geom_line(data = ~ filter(.x, grepl("ago", series)), linewidth = 0.6, na.rm = TRUE) +
  geom_line(data = ~ filter(.x, series == "Modelled, this week"), linewidth = 0.95, na.rm = TRUE) +
  geom_line(data = ~ filter(.x, series == "Reported (ERVISS)"), linewidth = 0.45, na.rm = TRUE) +
  geom_point(data = ~ filter(.x, series == "Reported (ERVISS)"), size = 1.25, na.rm = TRUE) +
  facet_wrap(~ strip, ncol = NCOL, scales = "free_y") +
  scale_colour_manual(values = SER, breaks = drawn, name = NULL, drop = TRUE) +
  scale_x_date(breaks = OBS_WKS, labels = ~ sprintf("W%02d", lubridate::isoweek(.x)),
               expand = expansion(mult = 0.06)) +
  scale_y_log10(labels = label_number(big.mark = " ", drop0trailing = TRUE, accuracy = 0.01),
                breaks = scales::breaks_extended(3),
                expand = expansion(mult = c(0.10, 0.14))) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1.1))) +
  labs(x = NULL, y = paste0(UNITS[[INDICATOR]], "  (log scale)")) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25, colour = RULE),
        panel.spacing      = unit(9, "pt"),
        strip.text         = element_text(colour = INK, size = 8.8, hjust = 0,
                                          margin = margin(b = 2.5)),
        axis.text.x        = element_text(colour = MUTED, size = 7.2),
        axis.text.y        = element_text(colour = MUTED, size = 7.2),
        axis.title.y       = element_text(colour = MUTED, size = 8.5),
        legend.position    = "top",
        legend.text        = element_text(colour = INK, size = 8.8),
        legend.key.width   = unit(20, "pt"),
        legend.margin      = margin(0, 0, 2, 0))

# The absent comparison year is stated, not silently omitted: the syndromic hubs did not
# run in autumn 2024, so there is no MSP to overlay for that vintage.
gap_note <- setdiff(sprintf("Modelled, %d year%s ago", LAG_YEARS, ifelse(LAG_YEARS == 1, "", "s")), drawn)
gap_txt  <- if (length(gap_note))
  paste0("\n", paste(gap_note, collapse = " and "),
         " is not drawn: no ensemble ran in those weeks, so no MSP exists for them.") else ""

fig <- p + plot_annotation(
  title    = sprintf("%s across EU/EEA: what was reported, and what the ensemble makes of it", INDICATOR),
  subtitle = sprintf("Weeks W%02d-W%02d of %d, %d countries, ordered by the modelled weekly change. Newest reported week is W%02d (ending %s); today is W%02d.",
                     lubridate::isoweek(min(OBS_WKS)), lubridate::isoweek(LAST_OBS),
                     lubridate::isoyear(LAST_OBS), length(KEEP),
                     lubridate::isoweek(LAST_OBS), format(LAST_OBS, "%d %b"),
                     lubridate::isoweek(Sys.Date())),
  caption  = paste0(
    "MSP (Modelled Smooth Point) = the ensemble's 1- and 2-week-ahead medians extrapolated back one week on a log scale (f1^2 / f2).",
    "\nGrey slopes are the same two calendar weeks 52 and 104 weeks earlier, at their own levels, drawn at the current x positions.",
    gap_txt,
    "\ny axes are log10 and free. Equal gradients mean equal weekly growth whatever the level, but levels are NOT comparable between",
    "\ncountries: national case definitions and denominators differ. Zeros cannot be shown on a log axis.",
    if (length(EXCLUDE)) paste0("\nWithheld: ", paste(sprintf("%s -- %s", NAME[names(EXCLUDE)], EXCLUDE), collapse = "; "), ".") else "",
    "\nSource: RespiCast + ERVISS, retrieved ", format(Sys.Date(), "%d %b %Y"), "."),
  theme = theme(plot.title    = element_text(colour = INK, size = 13, face = "plain"),
                plot.subtitle = element_text(colour = MUTED, size = 9, margin = margin(b = 6)),
                plot.caption  = element_text(colour = MUTED, size = 7.2, hjust = 0, lineheight = 1.3),
                plot.caption.position = "plot", plot.title.position = "plot"))

NROW <- ceiling(length(KEEP) / NCOL)
ggsave(file.path(params$figure_dir, sprintf("msp_grid_%s.png", gsub("[^a-z0-9]+", "_", tolower(INDICATOR)))),
       fig, width = 11, height = 2.0 + 1.55 * NROW, dpi = 500, bg = "white", limitsize = FALSE)
cat(sprintf("figure -> output/figures/msp_grid_%s.png\n", gsub("[^a-z0-9]+", "_", tolower(INDICATOR))))

step("Countries dropped")
eligible %>% filter(!keep) %>%
  transmute(location, n_obs, n_msp,
            reason = case_when(excluded  ~ unname(EXCLUDE[location]),
                               n_msp < 2 ~ "no positive current MSP pair",
                               TRUE      ~ sprintf("only %d positive reported point(s)", n_obs))) %>%
  arrange(location) %>% as.data.frame() %>% print(row.names = FALSE)

step("Modelled weekly change, steepest first")
growth %>% transmute(location, prev = round(prev, 2), last = round(last, 2),
                     change = sprintf("%+.1f%%", 100 * chg)) %>%
  as.data.frame() %>% print(row.names = FALSE)
