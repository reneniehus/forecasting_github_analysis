# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### SLOPE MONITOR -- output/figures/slope_monitor_<indicator>.{pdf,png} ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The figure titled "Slope monitor" is built here. Its caption carries this path, so a
# printed copy always says where it came from.
#
# One ROW per country, ordered by the modelled weekly change, steepest rise at the top.
#
#   LEFT   REAL LEVELS, log10, own scale per country.
#          black      the last N_SURV reported weeks
#          turquoise  the ensemble's forecast from the latest FINAL round: median (dashed)
#                     and 50% interval (band), 1 and 2 weeks ahead. Because of the
#                     reporting lag the 1-week-ahead lands ON the newest reported week, so
#                     the forecast overlaps the black line by a week and extends it by one.
#          grey       the same ISO weeks in every earlier season available, one plain line
#                     per season, all one grey -- back to 2015 via RespiCompass, see
#                     code/03_hubs/extract_respicompass_history.R
#
#   RIGHT  THE MSP SLOPE ACROSS THE LAST TWO REPORTED WEEKS, nothing else. Each slope is
#          re-centred on its midpoint, so the level is gone, the gradient is kept, and every
#          slope crosses the dotted zero line mid-way. ONE scale for every country, so
#          steepness compares across rows. Turquoise this season, grey earlier seasons --
#          only seasons the hubs actually covered, which started in late 2023.
#
# "The ensemble median" is the ensemble's 0.5 QUANTILE throughout (see compute_msp.R): the
# hub's separate `median` row is built from only the members that publish one.
#
# Only FINAL rounds are used (see compute_msp.R): the hubs publish provisional ensembles
# under an open round's date while submissions are still coming in.
#
# Everything is derived from the data -- vintage, weeks shown, comparison weeks -- so the
# same command redraws the current picture in any week. AS_OF=YYYY-MM-DD redraws a past one.
#
# Page: A4 portrait by default, as a vector PDF that prints at 100%, with a PNG beside it.
# PAGE=wide gives the broad screen layout.
#
# Run (after compute_msp.R):  Rscript code/05_figures/fig_slope_monitor.R
# Other indicators:           INDICATOR="ARI incidence" Rscript code/05_figures/fig_slope_monitor.R
#                             INDICATOR="COVID-19 hospitalisations" Rscript code/05_figures/fig_slope_monitor.R
# Screen layout:              PAGE=wide Rscript code/05_figures/fig_slope_monitor.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

# ---- |-0. configuration ----
INDICATOR <- Sys.getenv("INDICATOR", "ILI incidence")
PAGE      <- tolower(Sys.getenv("PAGE", "a4"))
A4        <- PAGE != "wide"
MARGIN_MM <- 12
PG_W      <- if (A4) 8.27 else 9.6
TYPE      <- if (A4) 0.76 else 1        # one factor scales every type size together
DPI       <- as.numeric(Sys.getenv("DPI", if (A4) "300" else "500"))
TODAY     <- as.Date(Sys.getenv("AS_OF", as.character(Sys.Date())))

N_SURV    <- 5L       # reported weeks in the left column
N_SLOPE   <- 2L       # weeks the MSP slope spans
HIST_MAX  <- 12L      # earlier seasons reach back at most this many years
MIN_OBS   <- 2L       # a country needs this many non-zero reported points
SCALE_Q   <- 0.90     # the slope scale is a quantile of the drawn slopes, not the maximum,
MIN_HALF  <- 0.025    # floored so a flat week is not magnified into noise

# LU's ERVISS ILI alternates between 0 and 2200 within a single month: a reporting
# artefact, not epidemiology.
EXCLUDE <- c(LU = "ERVISS series alternates 0 / 2200 -- reporting artefact")

TRUTH <- c("ILI incidence"             = "/workspace/emh-syndromic/target-data/ERVISS/latest-ILI_incidence.csv",
           "ARI incidence"             = "/workspace/emh-syndromic/target-data/ERVISS/latest-ARI_incidence.csv",
           "COVID-19 hospitalisations" = "/workspace/emh-covid/target-data/latest-hospital_admissions.csv")
UNITS <- c("ILI incidence"             = "ILI consultations per 100 000",
           "ARI incidence"             = "ARI consultations per 100 000",
           "COVID-19 hospitalisations" = "Weekly hospital admissions")
NAME  <- c(AT="Austria", BE="Belgium", BG="Bulgaria", CY="Cyprus", CZ="Czechia", DE="Germany",
           DK="Denmark", EE="Estonia", ES="Spain", FI="Finland", FR="France", GR="Greece",
           HR="Croatia", HU="Hungary", IE="Ireland", IS="Iceland", IT="Italy", LI="Liechtenstein",
           LT="Lithuania", LU="Luxembourg", LV="Latvia", MT="Malta", NL="Netherlands", NO="Norway",
           PL="Poland", PT="Portugal", RO="Romania", SE="Sweden", SI="Slovenia", SK="Slovakia")

INK       <- "#1f1f1c"; MUTED <- "#6f6e69"; RULE <- "#e6e5df"
TURQ      <- "#00A0A8"
TURQ_FILL <- "#b9e3e5"   # the forecast's 50% interval
PAST      <- "#aaa9a3"   # every earlier season, lines and arrows alike
STRIPE    <- "#f6f5f0"   # alternating row tint: barely there up close, a stripe from afar

isoy <- lubridate::isoyear; isow <- lubridate::isoweek
wlab <- function(d) sprintf("W%02d", isow(d))

# ---- |-1. data, with the vintage read off the files ----
step(sprintf("Building the %s slope monitor", INDICATOR))

live <- read_csv(TRUTH[[INDICATOR]], show_col_types = FALSE) %>%
  transmute(location, week_end = as.Date(truth_date), value) %>%
  filter(!is.na(value), value > 0)                         # zeros cannot go on a log axis
hist <- read_csv(here::here("data", "respicompass_history.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR, value > 0) %>% select(location, week_end, value)
truth <- bind_rows(live, hist) %>% mutate(iy = isoy(week_end), iw = isow(week_end))

msp <- read_csv(file.path(params$output_dir, "msp_weekly.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR, eu_eea, msp > 0) %>%
  mutate(week_end = as.Date(week_end), iy = isoy(week_end), iw = isow(week_end))
fc_all <- read_csv(file.path(params$output_dir, "ensemble_forecasts.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR, eu_eea)
not_spliced <- read_csv(here::here("data", "respicompass_not_spliced.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR)
AGREE <- read_csv(here::here("data", "respicompass_agreement.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR) %>% pull(identical_share)

LAST_OBS  <- max(live$week_end)                         # newest reported week (live data only)
wk_floor  <- function(d) lubridate::floor_date(d, "week", week_start = 1)
WK_BEHIND <- as.integer(as.numeric(wk_floor(TODAY) - wk_floor(LAST_OBS)) / 7)
SURV_WKS  <- seq(LAST_OBS - 7 * (N_SURV  - 1), LAST_OBS, by = 7)
SLOPE_WK  <- seq(LAST_OBS - 7 * (N_SLOPE - 1), LAST_OBS, by = 7)

# the forecast comes from the latest FINAL round; compute_msp.R has already dropped open ones
FC_ROUND <- max(fc_all$origin_date)
fc       <- filter(fc_all, origin_date == FC_ROUND, horizon %in% 1:2)
FC_WKS   <- sort(unique(fc$target_end_date))
X1_WKS   <- sort(unique(c(SURV_WKS, FC_WKS)))

# The two columns must describe the same round: the MSP's newest point is that round's
# 1-week-ahead median. If they ever diverge, say so loudly rather than plot a mismatch.
edge_round <- max(msp$origin_date[msp$source == "h1_nowcast"])
if (!identical(edge_round, FC_ROUND))
  warning(sprintf("MSP leading edge is from %s but the forecast round is %s", edge_round, FC_ROUND))

say(sprintf("newest reported %s (%s); forecast round %s -> %s; today %s (%s), %d week(s) behind",
            LAST_OBS, wlab(LAST_OBS), FC_ROUND, paste(wlab(FC_WKS), collapse = "+"),
            TODAY, wlab(TODAY), WK_BEHIND))

# ---- |-2. who qualifies ----
obs_now <- live %>% filter(week_end %in% SURV_WKS, location %in% unique(msp$location))
msp_now <- msp  %>% filter(week_end %in% SLOPE_WK)
eligible <- full_join(count(obs_now, location, name = "n_obs"),
                      count(msp_now, location, name = "n_msp"), by = "location") %>%
  mutate(across(c(n_obs, n_msp), ~ replace_na(.x, 0L)),
         excluded = location %in% names(EXCLUDE),
         keep     = n_msp == N_SLOPE & n_obs >= MIN_OBS & !excluded)
KEEP <- eligible$location[eligible$keep]
say(sprintf("%d EU/EEA countries kept; %d dropped, %d withheld",
            length(KEEP), sum(!eligible$keep & !eligible$excluded), sum(eligible$excluded)))

# ---- |-3. earlier seasons, matched on ISO week ----
# Same ISO week number, k years earlier: the epidemiological convention, and unlike a fixed
# 364-day shift it does not drift a week each time a 53-week year (2015, 2020) intervenes.
at_lag <- function(tbl, weeks, k)
  tbl %>% filter(location %in% KEEP) %>% select(location, iy, iw, value) %>%
    inner_join(tibble(week_end = weeks, iy = isoy(weeks) - k, iw = isow(weeks)), by = c("iy", "iw")) %>%
    mutate(k = k)

# a season with a single week inside the window has no shape to compare, and ggplot would
# silently drop it as a one-point line: drop it here, deliberately
past_lines <- map_dfr(seq_len(HIST_MAX), ~ at_lag(truth, X1_WKS, .x)) %>%
  group_by(location, k) %>% filter(n() >= 2) %>% ungroup()
past_msp   <- map_dfr(seq_len(HIST_MAX), ~ at_lag(rename(msp, value = msp), SLOPE_WK, .x))

PAST_YRS <- isoy(LAST_OBS) - range(past_lines$k)
PAST_LAB <- sprintf("Earlier seasons, same weeks (%d-%d)", PAST_YRS[2], PAST_YRS[1])
say(sprintf("earlier seasons: %d lines over %d countries, %d-%d",
            n_distinct(paste(past_lines$location, past_lines$k)), n_distinct(past_lines$location),
            PAST_YRS[2], PAST_YRS[1]))

# ---- |-4. row order: the modelled weekly change, steepest rise first ----
growth <- msp_now %>% filter(location %in% KEEP) %>% arrange(location, week_end) %>%
  group_by(location) %>%
  summarise(prev = first(msp), last = last(msp), chg = last / prev - 1, .groups = "drop") %>%
  arrange(desc(chg))
ORDER  <- setNames(NAME[growth$location], growth$location)
as_row <- function(d) mutate(d, row = factor(NAME[location], levels = unname(ORDER)))

# ---- |-5. the normalised slopes ----
# Subtracting the mean of the two logs removes the level and leaves the gradient, so every
# slope crosses zero half-way between its weeks.
slopes <- bind_rows(msp_now %>% filter(location %in% KEEP) %>%
                      transmute(location, week_end, value = msp, now = TRUE, k = 0L),
                    past_msp %>% transmute(location, week_end, value, now = FALSE, k)) %>%
  group_by(location, k) %>% filter(n() == N_SLOPE) %>%
  arrange(week_end, .by_group = TRUE) %>%
  summarise(now = first(now), x = first(week_end), xend = last(week_end),
            d = log10(last(value)) - log10(first(value)), .groups = "drop") %>%
  as_row()

# Years of the grey arrows ACTUALLY drawn. An earlier season can hold an MSP for only one
# of the two weeks (COVID 2023 has W39 but not W38, the archive entering scope on 1 Oct),
# and must not then be named in the caption.
MSP_YRS <- sort(unique(isoy(LAST_OBS) - slopes$k[!slopes$now]))
say(sprintf("earlier-season arrows drawn for: %s", if (length(MSP_YRS)) paste(MSP_YRS, collapse = ", ") else "none"))

HALF <- max(unname(quantile(abs(slopes$d) / 2, SCALE_Q)) * 1.15, MIN_HALF)

# Truncate rather than squash: cut a too-steep segment where it meets +-HALF, gradient
# intact, so it reads as running off the frame rather than being quietly rescaled.
slopes <- slopes %>%
  mutate(t_lo = (-HALF + d / 2) / d, t_hi = (HALF + d / 2) / d,
         t_lo = ifelse(is.finite(t_lo), pmin(pmax(t_lo, 0), 1), 0),
         t_hi = ifelse(is.finite(t_hi), pmin(pmax(t_hi, 0), 1), 1),
         span = as.numeric(xend - x),
         x2 = x + span * pmin(t_lo, t_hi), xend2 = x + span * pmax(t_lo, t_hi),
         y2 = -d / 2 + d * pmin(t_lo, t_hi), yend2 = -d / 2 + d * pmax(t_lo, t_hi),
         clipped = abs(d) / 2 > HALF)
say(sprintf("slope scale +-%.3f log10 (full height %+.0f%%/wk); %d of %d slopes truncated",
            HALF, 100 * (10 ^ (2 * HALF) - 1), sum(slopes$clipped), nrow(slopes)))

# The label sits at the tip of THIS season's arrow (joined on location AND season, never
# matched on a pasted name -- that once resolved to a grey arrow), held inside the panel.
X2  <- c(min(SLOPE_WK) - 1.5, TODAY + 2)
tip <- slopes %>% filter(now) %>% select(location, y_tip = yend2)
stopifnot(nrow(tip) == length(KEEP), !anyNA(tip$y_tip))
lab_now <- growth %>% transmute(location, chg) %>%
  left_join(tip, by = "location") %>% as_row() %>%
  mutate(x = max(SLOPE_WK) + 1.6, y = pmin(pmax(y_tip, -HALF * 0.70), HALF * 0.70),
         label = sprintf("%+.0f%%", 100 * chg))

# ---- |-6. shared furniture ----
stripe_rows <- tibble(row = factor(unname(ORDER), levels = unname(ORDER))) %>%
  filter(seq_len(n()) %% 2 == 1) %>%
  mutate(xmin = as.Date(-Inf), xmax = as.Date(Inf))
stripe <- function(floor = -Inf) geom_rect(data = mutate(stripe_rows, ymin = floor, ymax = Inf),
                                           inherit.aes = FALSE,
                                           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                           fill = STRIPE)

base_theme <- theme_minimal(base_size = 10 * TYPE) +
  theme(panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_blank(),
        panel.spacing.y     = unit(3, "pt"),            # just enough that y labels do not collide
        axis.text.x         = element_text(colour = MUTED, size = 7.8 * TYPE),
        axis.ticks          = element_blank(),
        legend.position     = "none",
        plot.title          = element_text(colour = MUTED, size = 9.2 * TYPE, hjust = 0,
                                           margin = margin(b = 7)),
        plot.title.position = "plot")

# ---- |-7. LEFT: real levels, earlier seasons, and the ensemble's forecast ----
p1 <- ggplot() +
  stripe(0) +                       # 0 -> -Inf once log-transformed; -Inf itself would be NaN
  geom_line(data = as_row(past_lines), aes(week_end, value, group = k),
            colour = PAST, linewidth = 0.42 * TYPE) +
  geom_ribbon(data = fc %>% filter(location %in% KEEP, q25 > 0) %>% as_row(),
              aes(target_end_date, ymin = q25, ymax = q75), fill = TURQ_FILL, alpha = 0.85) +
  geom_line(data = fc %>% filter(location %in% KEEP) %>% as_row(),
            aes(target_end_date, median), colour = TURQ, linewidth = 0.75 * TYPE, linetype = "22") +
  geom_line(data = as_row(obs_now %>% filter(location %in% KEEP)), aes(week_end, value),
            colour = INK, linewidth = 0.55 * TYPE) +
  geom_point(data = as_row(obs_now %>% filter(location %in% KEEP)), aes(week_end, value),
             colour = INK, size = 1.35 * TYPE) +
  facet_wrap(~ row, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_x_date(breaks = X1_WKS, labels = wlab, expand = expansion(mult = 0.05)) +
  scale_y_log10(labels = label_number(big.mark = " ", drop0trailing = TRUE, accuracy = 0.01),
                # 1-2-3-5 steps sit evenly on a log axis; linear "nice" steps bunch at the top
                breaks = scales::breaks_log(4), expand = expansion(mult = 0.12)) +
  labs(x = NULL, y = NULL, title = "REPORTED, WITH THE ENSEMBLE FORECAST   own scale per country") +
  base_theme +
  theme(panel.grid.major.y = element_line(linewidth = 0.2, colour = RULE),
        strip.placement    = "outside",
        strip.text.y.left  = element_text(colour = INK, size = 9.6 * TYPE, angle = 0, hjust = 1,
                                          margin = margin(r = 9)),
        axis.text.y        = element_text(colour = MUTED, size = 6.9 * TYPE),
        plot.margin        = margin(2, 4, 2, 2))

# ---- |-8. RIGHT: the two-week slope, one scale for everyone ----
p2 <- ggplot() +
  stripe() +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.4, colour = "#b8b7b1") +
  geom_vline(xintercept = TODAY, linewidth = 0.4, colour = MUTED) +
  # earlier seasons underneath, this season on top
  geom_segment(data = filter(slopes, !now), aes(x = x2, xend = xend2, y = y2, yend = yend2),
               colour = PAST, linewidth = 0.7 * TYPE, lineend = "butt",
               arrow = arrow(length = unit(3.4 * TYPE, "pt"), type = "closed", angle = 22)) +
  geom_segment(data = filter(slopes, now), aes(x = x2, xend = xend2, y = y2, yend = yend2),
               colour = TURQ, linewidth = 0.8 * TYPE, lineend = "butt",
               arrow = arrow(length = unit(3.6 * TYPE, "pt"), type = "closed", angle = 22)) +
  geom_text(data = lab_now, aes(x = x, y = y, label = label),
            hjust = 0, vjust = 0.5, size = 2.9 * TYPE, colour = TURQ) +
  geom_text(data = tibble(row = factor(unname(ORDER)[1], levels = unname(ORDER))),
            aes(x = TODAY - 1.1, y = 0), label = sprintf("today, %s", wlab(TODAY)),
            angle = 90, hjust = 0.5, vjust = 0, size = 2.85 * TYPE, colour = MUTED) +
  facet_wrap(~ row, ncol = 1) +
  scale_x_date(breaks = SLOPE_WK, labels = wlab) +
  coord_cartesian(xlim = X2, ylim = c(-HALF, HALF), expand = FALSE) +
  labs(x = NULL, y = NULL, title = "MODELLED SLOPE, LAST TWO WEEKS   level removed") +
  base_theme +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y        = element_blank(),
        strip.text         = element_blank(),
        plot.margin        = margin(2, 2, 2, 8))

# ---- |-9. the key ----
glyph <- function(type, x, y, w = 0.034) {
  switch(type,
    obs  = list(annotate("segment", x = x, xend = x + w, y = y, yend = y, colour = INK, linewidth = 0.55),
                annotate("point", x = x + c(0, w / 2, w), y = y, colour = INK, size = 1.3 * TYPE)),
    msp  = list(annotate("segment", x = x, xend = x + w, y = y - 0.1, yend = y + 0.1, colour = TURQ,
                         linewidth = 0.8, arrow = arrow(length = unit(3 * TYPE, "pt"), type = "closed", angle = 22))),
    fc   = list(annotate("rect", xmin = x, xmax = x + w, ymin = y - 0.13, ymax = y + 0.13, fill = TURQ_FILL),
                annotate("segment", x = x, xend = x + w, y = y, yend = y, colour = TURQ, linewidth = 0.7, linetype = "22")),
    past = list(annotate("segment", x = x, xend = x + w, y = y, yend = y, colour = PAST, linewidth = 0.6)))
}
key_items <- tribble(
  ~type,  ~x,    ~y,   ~label,
  "obs",  0.005, 0.72, "This year, reported",
  "msp",  0.005, 0.28, "This year, modelled slope (MSP)",
  "fc",   0.36,  0.72, "Ensemble forecast: median, 50% interval",
  "past", 0.36,  0.28, PAST_LAB)
key <- ggplot() +
  pmap(key_items, function(type, x, y, label) c(glyph(type, x, y),
       list(annotate("text", x = x + 0.044, y = y, hjust = 0, size = 2.95 * TYPE, colour = INK, label = label)))) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + theme_void()

# ---- |-10. assemble ----
spliced_note <- if (nrow(not_spliced)) sprintf(
  " %s: history not spliced, its overlap with ERVISS differs by %s.",
  paste(NAME[not_spliced$location], collapse = " and "),
  paste(sprintf("%.0f%%", 100 * not_spliced$median_rel_diff), collapse = " / ")) else ""
dk_note <- if (INDICATOR == "ILI incidence" && "DK" %in% KEEP) " Denmark's earlier extract differs by ~10%, a later ERVISS revision." else ""
msp_yrs_note <- if (length(MSP_YRS))
  sprintf(" Grey arrows: %s, the only earlier season%s with an ensemble in these weeks (the hubs began in late 2023).",
          paste(MSP_YRS, collapse = " and "), ifelse(length(MSP_YRS) == 1, "", "s")) else
  " No earlier season has an ensemble in these weeks."

fig <- key / (p1 | p2) +
  plot_layout(heights = c(0.055, 1), widths = c(1.12, 1)) +
  plot_annotation(
    title    = sprintf("Slope monitor: %s across EU/EEA", INDICATOR),
    subtitle = sprintf(paste0("%d countries, steepest modelled rise first. Newest reported week %s of %d (ending %s). ",
                              "Forecast from the ensemble round of %s, the latest final one.\n",
                              "Today is %s, so the newest reported week is %d week%s behind. %s."),
                       length(KEEP), wlab(LAST_OBS), isoy(LAST_OBS), format(LAST_OBS, "%d %b"),
                       format(FC_ROUND, "%d %b"), wlab(TODAY), WK_BEHIND, ifelse(WK_BEHIND == 1, "", "s"),
                       UNITS[[INDICATOR]]),
    caption  = paste0(
      "MSP (Modelled Smooth Point) = the ensemble's 1- and 2-week-ahead medians extrapolated back one week on a log scale (f1^2 / f2).",
      "\nEnsemble median = the ensemble's 0.5 quantile; the hub's separate `median` row is built from only the members that publish one, and is not used.",
      sprintf("\nLeft: real levels, log10. The forecast's 1-week-ahead lands on %s, the newest reported week, so it overlaps the black line; its 2-week-ahead is %s.",
              wlab(FC_WKS[1]), wlab(FC_WKS[2])),
      sprintf("\nGrey: the same ISO weeks in each earlier season, %d-%d. Seasons before mid-2022 come from RespiCompass's earlier ERVISS extract,",
              PAST_YRS[2], PAST_YRS[1]),
      sprintf("\nidentical to ERVISS for %.0f%% of country-weeks where both exist.%s%s", 100 * AGREE, dk_note, spliced_note),
      "\nRight: each slope re-centred on its midpoint, so the level is gone and only the gradient remains; one scale for every country. Arrowheads mark",
      sprintf("\nthe week the slope lands on; the rule marks today.%s", msp_yrs_note),
      sprintf("\nThe right panel spans %+.0f%% per week top to bottom, set from the 90th percentile of the drawn slopes; a steeper one is cut at the frame.",
              100 * (10 ^ (2 * HALF) - 1)),
      "\nLevels are NOT comparable between countries: national case definitions and denominators differ. Zeros cannot be shown on a log axis.",
      if (length(EXCLUDE) && any(names(EXCLUDE) %in% eligible$location))
        paste0("\nWithheld: ", paste(sprintf("%s (%s)", NAME[names(EXCLUDE)], EXCLUDE), collapse = "; "), ".") else "",
      "\nSource: RespiCast, ERVISS, RespiCompass; retrieved ", format(TODAY, "%d %b %Y"),
      ". Built by code/05_figures/fig_slope_monitor.R."),
    theme = theme(plot.title    = element_text(colour = INK, size = 13 * TYPE, face = "plain"),
                  plot.subtitle = element_text(colour = MUTED, size = 8.6 * TYPE, margin = margin(b = 6), lineheight = 1.25),
                  plot.caption  = element_text(colour = MUTED, size = 6.9 * TYPE, hjust = 0, lineheight = 1.3),
                  plot.caption.position = "plot", plot.title.position = "plot",
                  plot.margin = if (A4) margin(MARGIN_MM, MARGIN_MM, MARGIN_MM, MARGIN_MM, unit = "mm")
                                else margin(5, 5, 5, 5)))

# The left column's stripe floors at 0 so that log10 takes it to -Inf (the panel bottom);
# ggplot reports that as a warning every time. It is intended, so it alone is muffled.
expected <- "log-10 transformation introduced infinite values"
save_quietly <- function(...) withCallingHandlers(ggsave(...), warning = function(w)
  if (grepl(expected, conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning"))

SLUG <- gsub("[^a-z0-9]+", "_", tolower(INDICATOR))
PG_H <- if (A4) 11.69 else 4 + 1.12 * length(KEEP)
STEM <- sprintf("slope_monitor_%s%s", SLUG, if (A4) "" else "_wide")
if (A4) {
  save_quietly(file.path(params$figure_dir, paste0(STEM, ".pdf")), fig,
               width = PG_W, height = PG_H, device = cairo_pdf, bg = "white")
  cat(sprintf("figure -> output/figures/%s.pdf  (A4 portrait, prints at 100%%)\n", STEM))
}
save_quietly(file.path(params$figure_dir, paste0(STEM, ".png")), fig,
             width = PG_W, height = PG_H, dpi = DPI, bg = "white", limitsize = FALSE)
cat(sprintf("figure -> output/figures/%s.png  (%g ppi)\n", STEM, DPI))

# ---- |-11. the numbers behind it ----
step("Countries dropped")
eligible %>% filter(!keep) %>%
  transmute(location, n_obs, n_msp,
            reason = case_when(excluded        ~ unname(EXCLUDE[location]),
                               n_msp < N_SLOPE ~ "no positive MSP pair",
                               TRUE            ~ sprintf("only %d positive reported point(s)", n_obs))) %>%
  arrange(location) %>% as.data.frame() %>% print(row.names = FALSE)

step("Modelled weekly change, steepest first")
growth %>% transmute(country = NAME[location], prev = round(prev, 2), last = round(last, 2),
                     change = sprintf("%+.1f%%", 100 * chg)) %>%
  as.data.frame() %>% print(row.names = FALSE)
