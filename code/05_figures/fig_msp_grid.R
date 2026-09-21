# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### EU/EEA rows: surveillance context beside the modelled slope ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# One ROW per country, stacked in order of the modelled weekly change, steepest rise
# at the top. Two columns, answering two different questions about the same country:
#
#   column 1  REPORTED ONLY, no modelling. The last N_SURV reported weeks in black
#             (dots and lines), with the same calendar weeks 52 and 104 weeks earlier
#             in grey (lines only). Context: is this year's level and shape unusual?
#   column 2  THE SLOPE, zoomed to the final two reported weeks. This week's MSP in
#             turquoise, the same two weeks a year and two years earlier in grey,
#             all lines only. Comparison: is the modelled turn steeper than usual?
#
# The two columns share a y scale within a row -- facet_grid frees y by row and x by
# column -- so the eye can carry a level straight across. Their x scales differ: column
# two is a zoom, and a given gradient there is drawn steeper than the same gradient in
# column one. Compare slopes WITHIN a panel, never across the two columns.
#
# "Exactly one year ago" is 364 days (52 whole weeks), not a calendar year: it keeps the
# Sunday week-ending alignment and lands on the same ISO week number.
#
# Everything is derived from the data -- reporting vintage, the weeks shown, the
# comparison weeks -- so the same command redraws the current picture in any week.
#
# Levels are NOT comparable between countries: national case definitions and
# denominators differ. Hence free y scales and no shared axis.
#
# y axes are log10, the scale the MSP is defined on: a log-linear extrapolation plots as
# the straight line it is, and equal gradients mean equal weekly growth whatever the
# level. On a linear axis a grey line sitting at twice the current level reads as twice
# as steep for the same growth, which would defeat the comparison. The cost is that
# zeros cannot be drawn, so an all-zero series drops out.
#
# Re-run weekly:      Rscript code/05_figures/fig_msp_grid.R
# Other indicators:   INDICATOR="ARI incidence" Rscript code/05_figures/fig_msp_grid.R
#                     INDICATOR="COVID-19 hospitalisations" Rscript code/05_figures/fig_msp_grid.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

INDICATOR <- Sys.getenv("INDICATOR", "ILI incidence")
N_SURV    <- 5L                      # reported weeks in column 1
N_SLOPE   <- 2L                      # weeks in the column 2 zoom
LAG_YEARS <- c(1L, 2L)               # historical overlays, in whole years
MIN_OBS   <- 2L                      # minimum NON-ZERO reported points to keep a country

# Countries withheld, with the reason. LU's ERVISS series alternates between 0 and 2200
# within a single month, which is a reporting artefact rather than epidemiology; plotting
# it would put a meaningless row in a figure meant to be read at a glance.
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

INK <- "#1f1f1c"; MUTED <- "#6f6e69"; RULE <- "#e3e2dc"
LAG_LAB <- sprintf("%d year%s ago", LAG_YEARS, ifelse(LAG_YEARS == 1, "", "s"))
# One grey per lag, older = lighter. The SAME grey means the same lag in both columns;
# the column header says whether it is a reported or a modelled quantity.
LAG_COL <- setNames(c("#8f8f89", "#c4c3bd", "#dcdbd5")[seq_along(LAG_YEARS)], LAG_LAB)
SER <- c("This year, reported"        = INK,
         "This year, modelled (MSP)"  = "#00A0A8",
         LAG_COL)

COL1 <- sprintf("Reported surveillance   last %d weeks", N_SURV)
COL2 <- sprintf("Modelled slope   last %d weeks", N_SLOPE)

# ---- |-1. data, with the vintage read off the files ----
step(sprintf("Building the %s figure", INDICATOR))

truth <- read_csv(TRUTH[[INDICATOR]], show_col_types = FALSE) %>%
  filter(!is.na(value), value > 0) %>%                 # zeros cannot go on a log axis
  mutate(week_end = as.Date(truth_date))

msp <- read_csv(file.path(params$output_dir, "msp_weekly.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR, eu_eea, msp > 0) %>%
  mutate(week_end = as.Date(week_end))

LAST_OBS <- max(truth$week_end)                        # newest reported week, from the data
SURV_WKS <- seq(LAST_OBS - 7 * (N_SURV  - 1), LAST_OBS, by = 7)
SLOPE_WK <- seq(LAST_OBS - 7 * (N_SLOPE - 1), LAST_OBS, by = 7)
say(sprintf("newest reported week %s (W%02d); surveillance %s -> %s, slope %s -> %s",
            format(LAST_OBS), lubridate::isoweek(LAST_OBS),
            format(min(SURV_WKS)), format(LAST_OBS), format(min(SLOPE_WK)), format(LAST_OBS)))

# ---- |-2. who qualifies ----
obs_now <- truth %>% filter(week_end %in% SURV_WKS, location %in% unique(msp$location))
msp_now <- msp   %>% filter(week_end %in% SLOPE_WK)

eligible <- full_join(count(obs_now, location, name = "n_obs"),
                      count(msp_now, location, name = "n_msp"), by = "location") %>%
  mutate(across(c(n_obs, n_msp), ~ replace_na(.x, 0L)),
         excluded = location %in% names(EXCLUDE),
         keep     = n_msp == N_SLOPE & n_obs >= MIN_OBS & !excluded)

KEEP <- eligible$location[eligible$keep]
say(sprintf("%d EU/EEA countries kept; %d dropped (no MSP pair or < %d positive reported points), %d withheld",
            length(KEEP), sum(!eligible$keep & !eligible$excluded), MIN_OBS, sum(eligible$excluded)))

# ---- |-3. the historical overlays, shifted onto the current x positions ----
# 364 days = 52 whole weeks, so Sunday alignment and the ISO week number both hold.
lag_of <- function(tbl, val, weeks, k)
  tbl %>% filter(week_end %in% (weeks - 364L * k), location %in% KEEP) %>%
    transmute(location, week_end = week_end + 364L * k, value = {{ val }},
              series = sprintf("%d year%s ago", k, ifelse(k == 1, "", "s")))

surv_lag <- map_dfr(LAG_YEARS, ~ lag_of(truth, value, SURV_WKS,  .x) %>% mutate(panel = COL1))
msp_lag  <- map_dfr(LAG_YEARS, ~ lag_of(msp,   msp,   SLOPE_WK,  .x) %>% mutate(panel = COL2))

for (i in seq_along(LAG_YEARS)) {
  k <- LAG_YEARS[i]
  say(sprintf("%-12s reported %s -> %2d countries | modelled %s -> %2d countries", LAG_LAB[i],
              format(min(SURV_WKS) - 364L * k), n_distinct(surv_lag$location[surv_lag$series == LAG_LAB[i]]),
              format(min(SLOPE_WK) - 364L * k), n_distinct(msp_lag$location[msp_lag$series == LAG_LAB[i]])))
}

plot_df <- bind_rows(
  obs_now %>% filter(location %in% KEEP) %>%
    transmute(location, week_end, value, series = "This year, reported",       panel = COL1),
  msp_now %>% filter(location %in% KEEP) %>%
    transmute(location, week_end, value = msp, series = "This year, modelled (MSP)", panel = COL2),
  surv_lag, msp_lag) %>%
  mutate(series = factor(series, levels = names(SER)),
         panel  = factor(panel,  levels = c(COL1, COL2)))

# ---- |-4. row order: the modelled weekly change, steepest rise first ----
growth <- msp_now %>% filter(location %in% KEEP) %>%
  arrange(location, week_end) %>%
  group_by(location) %>%
  summarise(prev = first(msp), last = last(msp), chg = last / prev - 1, .groups = "drop") %>%
  arrange(desc(chg)) %>%
  mutate(strip = sprintf("%s  %+.0f%%", NAME[location], 100 * chg))

plot_df <- plot_df %>%
  left_join(select(growth, location, strip), by = "location") %>%
  mutate(strip = factor(strip, levels = growth$strip))

# ---- |-5. draw ----
drawn <- levels(droplevels(plot_df$series))
is_pt  <- function(d) filter(d, series == "This year, reported")
is_ln  <- function(d) filter(d, series != "This year, reported")

p <- ggplot(plot_df, aes(week_end, value, colour = series)) +
  # historical greys underneath, this year's series on top
  geom_line(data = ~ filter(.x, grepl("ago", series)), linewidth = 0.55, na.rm = TRUE) +
  geom_line(data = ~ filter(.x, series == "This year, modelled (MSP)"), linewidth = 1, na.rm = TRUE) +
  geom_line(data = is_pt, linewidth = 0.5, na.rm = TRUE) +
  geom_point(data = is_pt, size = 1.35, na.rm = TRUE) +
  facet_grid(strip ~ panel, scales = "free", switch = "y") +
  scale_colour_manual(values = SER, breaks = drawn, name = NULL, drop = TRUE) +
  scale_x_date(breaks = SURV_WKS, labels = ~ sprintf("W%02d", lubridate::isoweek(.x)),
               expand = expansion(mult = 0.07)) +
  scale_y_log10(labels = label_number(big.mark = " ", drop0trailing = TRUE, accuracy = 0.01),
                breaks = scales::breaks_extended(4),
                expand = expansion(mult = c(0.14, 0.18))) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1.1, size = 1.6), nrow = 1)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 9.5) +
  theme(panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_blank(),
        panel.grid.major.y  = element_line(linewidth = 0.22, colour = RULE),
        panel.spacing.x     = unit(16, "pt"),
        panel.spacing.y     = unit(5,  "pt"),
        strip.placement     = "outside",
        strip.text.y.left   = element_text(colour = INK, size = 9, angle = 0, hjust = 0,
                                           margin = margin(r = 6)),
        strip.text.x        = element_text(colour = MUTED, size = 9, margin = margin(b = 5)),
        axis.text.x         = element_text(colour = MUTED, size = 7.4),
        axis.text.y         = element_text(colour = MUTED, size = 7),
        legend.position     = "top",
        legend.text         = element_text(colour = INK, size = 8.8),
        legend.key.width    = unit(22, "pt"),
        legend.margin       = margin(0, 0, 4, 0),
        plot.margin         = margin(4, 10, 4, 4))

# An absent comparison year is stated, not silently omitted.
gap  <- setdiff(LAG_LAB, unique(msp_lag$series))
gap_txt <- if (length(gap))
  paste0("\nNo modelled slope exists for ", paste(gap, collapse = " or "),
         ": the hubs did not run in those weeks. The reported column still shows them.") else ""

fig <- p + plot_annotation(
  title    = sprintf("%s across EU/EEA: what was reported, and the slope the ensemble reads into it", INDICATOR),
  subtitle = sprintf("%d countries, ordered by the modelled weekly change. Newest reported week W%02d of %d (ending %s); today is W%02d.\n%s.",
                     length(KEEP), lubridate::isoweek(LAST_OBS), lubridate::isoyear(LAST_OBS),
                     format(LAST_OBS, "%d %b"), lubridate::isoweek(Sys.Date()),
                     UNITS[[INDICATOR]]),
  caption  = paste0(
    "MSP (Modelled Smooth Point) = the ensemble's 1- and 2-week-ahead medians extrapolated back one week on a log scale (f1^2 / f2). ",
    "Grey lines are the same\ncalendar weeks 52 and 104 weeks earlier, at their own levels, drawn at the current x positions.",
    gap_txt,
    "\nThe two columns share a y scale within a row but NOT an x scale: the right column is a two-week zoom, so a given gradient is drawn",
    "\nsteeper there. Compare slopes within a panel, not across columns.",
    "\ny axes are log10 and free. Equal gradients mean equal weekly growth whatever the level, but levels are NOT comparable between",
    "\ncountries: national case definitions and denominators differ. Zeros cannot be shown on a log axis.",
    if (length(EXCLUDE)) paste0("\nWithheld: ", paste(sprintf("%s -- %s", NAME[names(EXCLUDE)], EXCLUDE), collapse = "; "), ".") else "",
    "\nSource: RespiCast + ERVISS, retrieved ", format(Sys.Date(), "%d %b %Y"), "."),
  theme = theme(plot.title    = element_text(colour = INK, size = 12.5, face = "plain"),
                plot.subtitle = element_text(colour = MUTED, size = 8.8, margin = margin(b = 6)),
                plot.caption  = element_text(colour = MUTED, size = 7.1, hjust = 0, lineheight = 1.3),
                plot.caption.position = "plot", plot.title.position = "plot"))

SLUG <- gsub("[^a-z0-9]+", "_", tolower(INDICATOR))
ggsave(file.path(params$figure_dir, sprintf("msp_grid_%s.png", SLUG)), fig,
       width = 9.4, height = 3.3 + 0.80 * length(KEEP), dpi = 500, bg = "white", limitsize = FALSE)
cat(sprintf("figure -> output/figures/msp_grid_%s.png\n", SLUG))

step("Countries dropped")
eligible %>% filter(!keep) %>%
  transmute(location, n_obs, n_msp,
            reason = case_when(excluded        ~ unname(EXCLUDE[location]),
                               n_msp < N_SLOPE ~ "no positive MSP pair",
                               TRUE            ~ sprintf("only %d positive reported point(s)", n_obs))) %>%
  arrange(location) %>% as.data.frame() %>% print(row.names = FALSE)

step("Modelled weekly change, steepest first")
growth %>% transmute(location, prev = round(prev, 2), last = round(last, 2),
                     change = sprintf("%+.1f%%", 100 * chg)) %>%
  as.data.frame() %>% print(row.names = FALSE)
