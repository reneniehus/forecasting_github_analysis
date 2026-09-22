# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### SLOPE MONITOR -- output/figures/slope_monitor_<indicator>.{pdf,png} ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The figure titled "Slope monitor" is built here. Its caption carries this path, so a
# printed copy always says where it came from.
# One ROW per country, stacked in order of the modelled weekly change, steepest rise at
# the top. Two columns answering two different questions about the same country:
#
#   LEFT   REPORTED ONLY, no modelling. The last N_SURV reported weeks in black (dots and
#          lines), the same calendar weeks 52 and 104 weeks earlier in grey (lines only).
#          Real levels, free y per country. Context: is this year unusual?
#
#   RIGHT  THE SLOPE ALONE. Each MSP slope is re-centred so its midpoint sits at zero --
#          the level is removed, the gradient is kept, and every slope crosses the dotted
#          zero line mid-way. Arrowheads point at the week the slope lands on; the rule
#          further right marks today, so the gap between them IS the reporting lag.
#
# Because the right column is normalised it carries ONE fixed y scale for every country:
# a steeper arrow means faster weekly growth, and that now compares across countries as
# well as within a row. The scale is log10, which is what the MSP is defined on, so a
# given gradient means the same growth ratio wherever it appears. It has no level, which
# is why it has no y axis -- the magnitude is printed beside each arrow instead.
#
# "Exactly one year ago" is 364 days (52 whole weeks), not a calendar year: it keeps the
# Sunday week-ending alignment and lands on the same ISO week number.
#
# Everything is derived from the data -- reporting vintage, weeks shown, comparison weeks
# -- so the same command redraws the current picture in any week.
#
# Page: A4 portrait by default, written as a vector PDF (the right thing to print) with a
# PNG beside it. PAGE=wide gives the original broad screen layout instead.
#
# Re-run weekly:      Rscript code/05_figures/fig_slope_monitor.R
# Other indicators:   INDICATOR="ARI incidence" Rscript code/05_figures/fig_slope_monitor.R
#                     INDICATOR="COVID-19 hospitalisations" Rscript code/05_figures/fig_slope_monitor.R
# Screen layout:      PAGE=wide Rscript code/05_figures/fig_slope_monitor.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

INDICATOR <- Sys.getenv("INDICATOR", "ILI incidence")
# A4 portrait: the PAGE is a true 210 x 297 mm and the 12 mm margin is drawn inside it,
# so the PDF prints at 100% with no dialog rescaling. Type is scaled by TYPE so the whole
# design shrinks together rather than the panels alone being squeezed under fixed fonts.
PAGE      <- tolower(Sys.getenv("PAGE", "a4"))
A4        <- PAGE != "wide"
MARGIN_MM <- 12
PG_W      <- if (A4) 8.27 else 9.6
TYPE      <- if (A4) 0.76 else 1
DPI       <- as.numeric(Sys.getenv("DPI", if (A4) "300" else "500"))
N_SURV    <- 5L                      # reported weeks in the left column
N_SLOPE   <- 2L                      # weeks the slope spans
LAG_YEARS <- c(1L, 2L)               # historical overlays, in whole years
MIN_OBS   <- 2L                      # minimum NON-ZERO reported points to keep a country
TODAY     <- Sys.Date()
# The normalised scale is set from a QUANTILE of the drawn slopes, not their maximum: one
# freak historical slope (a country coming off a near-zero summer) would otherwise flatten
# every other arrow to a few percent of the panel. Anything steeper than the scale is
# truncated at the panel edge with its true gradient intact, so it reads as running off
# the top rather than being quietly rescaled. MIN_HALF floors the scale so a week in which
# every slope is flat is not magnified into a field of dramatic-looking noise.
SCALE_Q   <- 0.90
MIN_HALF  <- 0.025

# Countries withheld, with the reason. LU's ERVISS series alternates between 0 and 2200
# within a single month, which is a reporting artefact rather than epidemiology.
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

INK  <- "#1f1f1c"; MUTED <- "#6f6e69"; RULE <- "#e6e5df"
TURQ <- "#00A0A8"
BAND <- "#f6f5f0"                    # the alternating row tint: barely there up close,
                                     # a clear stripe from across the room
LAG_LAB <- sprintf("%d year%s ago", LAG_YEARS, ifelse(LAG_YEARS == 1, "", "s"))
LAG_COL <- setNames(c("#8f8f89", "#c4c3bd", "#dcdbd5")[seq_along(LAG_YEARS)], LAG_LAB)
SER <- c("This year, reported" = INK, "This year, modelled (MSP)" = TURQ, LAG_COL)

# ---- |-1. data, with the vintage read off the files ----
step(sprintf("Building the %s figure", INDICATOR))

truth <- read_csv(TRUTH[[INDICATOR]], show_col_types = FALSE) %>%
  filter(!is.na(value), value > 0) %>%               # zeros cannot go on a log axis
  mutate(week_end = as.Date(truth_date))

msp <- read_csv(file.path(params$output_dir, "msp_weekly.csv"), show_col_types = FALSE) %>%
  filter(indicator == INDICATOR, eu_eea, msp > 0) %>%
  mutate(week_end = as.Date(week_end))

LAST_OBS <- max(truth$week_end)
wk_floor <- function(d) lubridate::floor_date(d, "week", week_start = 1)
WK_BEHIND <- as.integer(as.numeric(wk_floor(TODAY) - wk_floor(LAST_OBS)) / 7)
SURV_WKS <- seq(LAST_OBS - 7 * (N_SURV  - 1), LAST_OBS, by = 7)
SLOPE_WK <- seq(LAST_OBS - 7 * (N_SLOPE - 1), LAST_OBS, by = 7)
say(sprintf("newest reported week %s (W%02d); surveillance %s -> %s, slope %s -> %s, today %s (W%02d)",
            format(LAST_OBS), lubridate::isoweek(LAST_OBS), format(min(SURV_WKS)), format(LAST_OBS),
            format(min(SLOPE_WK)), format(LAST_OBS), format(TODAY), lubridate::isoweek(TODAY)))

# ---- |-2. who qualifies ----
obs_now <- truth %>% filter(week_end %in% SURV_WKS, location %in% unique(msp$location))
msp_now <- msp   %>% filter(week_end %in% SLOPE_WK)

eligible <- full_join(count(obs_now, location, name = "n_obs"),
                      count(msp_now, location, name = "n_msp"), by = "location") %>%
  mutate(across(c(n_obs, n_msp), ~ replace_na(.x, 0L)),
         excluded = location %in% names(EXCLUDE),
         keep     = n_msp == N_SLOPE & n_obs >= MIN_OBS & !excluded)
KEEP <- eligible$location[eligible$keep]
say(sprintf("%d EU/EEA countries kept; %d dropped, %d withheld",
            length(KEEP), sum(!eligible$keep & !eligible$excluded), sum(eligible$excluded)))

# ---- |-3. historical overlays, shifted onto the current x positions ----
lag_of <- function(tbl, val, weeks, k)
  tbl %>% filter(week_end %in% (weeks - 364L * k), location %in% KEEP) %>%
    transmute(location, week_end = week_end + 364L * k, value = {{ val }},
              series = sprintf("%d year%s ago", k, ifelse(k == 1, "", "s")))

surv_lag <- map_dfr(LAG_YEARS, ~ lag_of(truth, value, SURV_WKS, .x))
msp_lag  <- map_dfr(LAG_YEARS, ~ lag_of(msp,   msp,   SLOPE_WK, .x))
for (i in seq_along(LAG_YEARS))
  say(sprintf("%-12s reported %2d countries | modelled %2d countries", LAG_LAB[i],
              n_distinct(surv_lag$location[surv_lag$series == LAG_LAB[i]]),
              n_distinct(msp_lag$location[msp_lag$series  == LAG_LAB[i]])))

# ---- |-4. row order: the modelled weekly change, steepest rise first ----
growth <- msp_now %>% filter(location %in% KEEP) %>% arrange(location, week_end) %>%
  group_by(location) %>%
  summarise(prev = first(msp), last = last(msp), chg = last / prev - 1, .groups = "drop") %>%
  arrange(desc(chg))
ORDER <- setNames(NAME[growth$location], growth$location)
as_row <- function(d) mutate(d, row = factor(NAME[location], levels = unname(ORDER)))

# ---- |-5. the normalised slopes ----
# Re-centre each slope on the geometric mean of its two points: subtracting the mean of
# the logs removes the level and leaves the gradient untouched, so every slope crosses
# zero half-way between the two weeks.
slopes <- bind_rows(msp_now %>% filter(location %in% KEEP) %>%
                      transmute(location, week_end, value = msp, series = "This year, modelled (MSP)"),
                    msp_lag) %>%
  group_by(location, series) %>%
  filter(n() == N_SLOPE) %>%
  arrange(week_end, .by_group = TRUE) %>%
  summarise(x = first(week_end), xend = last(week_end),
            d = log10(last(value)) - log10(first(value)), .groups = "drop") %>%
  mutate(y = -d / 2, yend = d / 2) %>%                       # midpoint pinned to zero
  as_row()

HALF <- max(unname(quantile(abs(slopes$d) / 2, SCALE_Q)) * 1.15, MIN_HALF)

# Truncate rather than squash: solve for where the segment crosses +-HALF and cut it
# there. The gradient is untouched, so a clipped arrow is still read correctly -- it
# simply stops at the frame.
slopes <- slopes %>%
  mutate(t_lo = pmin(pmax((-HALF + d / 2) / d, 0), 1),
         t_hi = pmin(pmax(( HALF + d / 2) / d, 0), 1),
         t_lo = ifelse(is.finite(t_lo), t_lo, 0), t_hi = ifelse(is.finite(t_hi), t_hi, 1),
         span = as.numeric(xend - x),
         x2    = x + span * pmin(t_lo, t_hi), xend2 = x + span * pmax(t_lo, t_hi),
         y2    = -d / 2 + d * pmin(t_lo, t_hi), yend2 = -d / 2 + d * pmax(t_lo, t_hi),
         clipped = abs(d) / 2 > HALF)
say(sprintf("normalised scale: +-%.3f log10 (full height %+.0f%%/wk); steepest drawn %+.0f%%; %d of %d slopes truncated",
            HALF, 100 * (10 ^ (2 * HALF) - 1),
            100 * (10 ^ slopes$d[which.max(abs(slopes$d))] - 1), sum(slopes$clipped), nrow(slopes)))

# the right column runs from the slope out to today, so the gap reads as the reporting lag
X2 <- c(min(SLOPE_WK) - 1.5, TODAY + 2)
# The label must sit at the tip of THIS YEAR'S arrow. Join on location AND series: an
# earlier version pasted the series name onto every row of `slopes` and matched on that,
# which silently resolved to whichever series sorted first -- the grey "1 year ago" tip.
tip <- slopes %>% filter(series == "This year, modelled (MSP)") %>%
  select(location, y_tip = yend2)
stopifnot(nrow(tip) == length(KEEP), !anyNA(tip$y_tip))

lab_now <- growth %>% transmute(location, chg) %>%
  left_join(tip, by = "location") %>% as_row() %>%
  mutate(x = max(SLOPE_WK) + 1.6,
         y = pmin(pmax(y_tip, -HALF * 0.70), HALF * 0.70),
         label = sprintf("%+.0f%%", 100 * chg))

# ---- |-6. shared furniture ----
rows  <- tibble(row = factor(unname(ORDER), levels = unname(ORDER))) %>%
  mutate(shade = seq_len(n()) %% 2 == 1)
band  <- filter(rows, shade)
stripe <- function(floor = -Inf) geom_rect(data = band, inherit.aes = FALSE,
                                           aes(xmin = -Inf, xmax = Inf, ymin = floor, ymax = Inf),
                                           fill = BAND)

base_theme <- theme_minimal(base_size = 10 * TYPE) +
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing.y   = unit(3, "pt"),          # just enough that adjacent y labels do not collide
        axis.text.x       = element_text(colour = MUTED, size = 7.8 * TYPE),
        axis.ticks        = element_blank(),
        legend.position   = "none",
        plot.title         = element_text(colour = MUTED, size = 9.2 * TYPE, hjust = 0,
                                          margin = margin(b = 7)),
        plot.title.position = "plot")

# ---- |-7. LEFT: reported surveillance, real levels ----
surv <- bind_rows(obs_now %>% filter(location %in% KEEP) %>%
                    transmute(location, week_end, value, series = "This year, reported"),
                  surv_lag) %>%
  mutate(series = factor(series, levels = names(SER))) %>% as_row()

p1 <- ggplot(surv, aes(week_end, value, colour = series)) +
  stripe(0) +                       # 0 -> -Inf once log-transformed; -Inf would be NaN
  geom_line(data = ~ filter(.x, series != "This year, reported"), linewidth = 0.55, na.rm = TRUE) +
  geom_line(data  = ~ filter(.x, series == "This year, reported"), linewidth = 0.55, na.rm = TRUE) +
  geom_point(data = ~ filter(.x, series == "This year, reported"), size = 1.4 * TYPE, na.rm = TRUE) +
  facet_wrap(~ row, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_colour_manual(values = SER, drop = FALSE) +
  scale_x_date(breaks = SURV_WKS, labels = ~ sprintf("W%02d", lubridate::isoweek(.x)),
               expand = expansion(mult = 0.05)) +
  scale_y_log10(labels = label_number(big.mark = " ", drop0trailing = TRUE, accuracy = 0.01),
                breaks = scales::breaks_extended(3), expand = expansion(mult = 0.22)) +
  labs(x = NULL, y = NULL, title = sprintf("REPORTED   last %d weeks, own scale", N_SURV)) +
  base_theme +
  theme(panel.grid.major.y = element_line(linewidth = 0.2, colour = RULE),
        strip.placement    = "outside",
        strip.text.y.left  = element_text(colour = INK, size = 9.6 * TYPE, angle = 0, hjust = 1,
                                          margin = margin(r = 9)),
        axis.text.y        = element_text(colour = MUTED, size = 6.9 * TYPE),
        plot.margin        = margin(2, 4, 2, 2))

# ---- |-8. RIGHT: the slope alone, normalised, one scale for everyone ----
p2 <- ggplot(slopes) +
  stripe() +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.4, colour = "#b8b7b1") +
  geom_vline(xintercept = as.numeric(TODAY), linewidth = 0.4, colour = MUTED) +
  geom_segment(aes(x = x2, xend = xend2, y = y2, yend = yend2, colour = series),
               linewidth = 0.75 * TYPE, lineend = "butt",
               arrow = arrow(length = unit(3.6 * TYPE, "pt"), type = "closed", angle = 22)) +
  geom_text(data = lab_now, aes(x = x, y = y, label = label),
            hjust = 0, vjust = 0.5, size = 2.9 * TYPE, colour = TURQ) +
  facet_wrap(~ row, ncol = 1) +
  scale_colour_manual(values = SER, drop = FALSE) +
  scale_x_date(breaks = SLOPE_WK, labels = ~ sprintf("W%02d", lubridate::isoweek(.x)),
               limits = X2, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-HALF, HALF), expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       title = "MODELLED SLOPE   level removed, shared scale") +
  base_theme +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y        = element_blank(),
        strip.text         = element_blank(),
        plot.margin        = margin(2, 2, 2, 8))

# the "today" rule is labelled once, above the top row, not twelve times
p2 <- p2 +
  geom_text(data = tibble(row = factor(unname(ORDER)[1], levels = unname(ORDER))),
            aes(x = TODAY - 1.2, y = 0), inherit.aes = FALSE,
            label = sprintf("today, W%02d", lubridate::isoweek(TODAY)),
            angle = 90, hjust = 0.5, vjust = 0, size = 2.85 * TYPE, colour = MUTED)

# ---- |-9. one drawn key, so the two columns share a legend without ggplot's ----
key_items <- c("This year, reported", "This year, modelled (MSP)",
               intersect(LAG_LAB, unique(c(surv_lag$series, msp_lag$series))))
kx <- head(cumsum(c(0.015, 0.085 + 0.0072 * nchar(key_items) + 0.028)), length(key_items))
key <- ggplot() +
  map2(kx, key_items, ~ annotate("segment", x = .x, xend = .x + 0.038, y = 0.5, yend = 0.5,
                                 colour = SER[[.y]], linewidth = if (grepl("modelled", .y)) 0.9 else 0.6)) +
  map2(kx[1], key_items[1], ~ annotate("point", x = c(.x, .x + 0.019, .x + 0.038), y = 0.5,
                                       colour = SER[[.y]], size = 1.4 * TYPE)) +
  map2(kx, key_items, ~ annotate("text", x = .x + 0.047, y = 0.5, hjust = 0, size = 3 * TYPE,
                                 colour = INK, label = .y)) +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) + theme_void()

gap <- setdiff(LAG_LAB, unique(msp_lag$series))
fig <- key / (p1 | p2) +
  plot_layout(heights = c(0.055, 1), widths = c(1, 1)) +
  plot_annotation(
    title    = sprintf("Slope monitor: %s across EU/EEA", INDICATOR),
    subtitle = sprintf("%d countries, steepest modelled rise first. Newest reported week W%02d of %d (ending %s); today is W%02d, so the model is reading %s behind.\n%s.",
                       length(KEEP), lubridate::isoweek(LAST_OBS), lubridate::isoyear(LAST_OBS),
                       format(LAST_OBS, "%d %b"), lubridate::isoweek(TODAY),
                       sprintf("%d week%s", WK_BEHIND, ifelse(WK_BEHIND == 1, "", "s")), UNITS[[INDICATOR]]),
    caption  = paste0(
      "MSP (Modelled Smooth Point) = the ensemble's 1- and 2-week-ahead medians extrapolated back one week on a log scale (f1^2 / f2).",
      "\nLeft: real levels, own y scale per country, log10. Grey lines are the same calendar weeks 52 and 104 weeks earlier.",
      "\nRight: each slope re-centred on the mean of its two logs, so the level is gone and only the gradient remains; every slope crosses the dotted",
      "\nzero line mid-way. One shared scale, so steepness compares across countries. Arrowheads mark the week the slope lands on; the rule marks today.",
      sprintf("\nThe right panel spans %+.0f%% per week top to bottom. Its scale is set from the 90th percentile of the drawn slopes; a steeper one is cut at",
              100 * (10 ^ (2 * HALF) - 1)),
      "\nthe frame with its gradient intact, not rescaled.",
      if (length(gap)) paste0("\nNo modelled slope exists for ", paste(gap, collapse = " or "),
                              ": the hubs did not run in those weeks. The reported column still shows them.") else "",
      "\nLevels are NOT comparable between countries: national case definitions and denominators differ. Zeros cannot be shown on a log axis.",
      if (length(EXCLUDE)) paste0("\nWithheld: ", paste(sprintf("%s -- %s", NAME[names(EXCLUDE)], EXCLUDE), collapse = "; "), ".") else "",
      "\nSource: RespiCast + ERVISS, retrieved ", format(TODAY, "%d %b %Y"), ".",
      "   Built by code/05_figures/fig_slope_monitor.R from output/msp_weekly.csv."),
    theme = theme(plot.title    = element_text(colour = INK, size = 13 * TYPE, face = "plain"),
                  plot.subtitle = element_text(colour = MUTED, size = 8.8 * TYPE, margin = margin(b = 8), lineheight = 1.25),
                  plot.caption  = element_text(colour = MUTED, size = 7.1 * TYPE, hjust = 0, lineheight = 1.35),
                  plot.caption.position = "plot", plot.title.position = "plot",
                  plot.margin = if (A4) margin(MARGIN_MM, MARGIN_MM, MARGIN_MM, MARGIN_MM, unit = "mm")
                                else margin(5, 5, 5, 5)))

SLUG <- gsub("[^a-z0-9]+", "_", tolower(INDICATOR))
PG_H <- if (A4) 11.69 else 3.6 + 1.12 * length(KEEP)
STEM <- sprintf("slope_monitor_%s%s", SLUG, if (A4) "" else "_wide")

# A4 goes out as vector PDF first -- that is what actually prints well -- with a raster
# copy beside it for screens and for pasting into slides.
if (A4) {
  ggsave(file.path(params$figure_dir, paste0(STEM, ".pdf")), fig,
         width = PG_W, height = PG_H, device = cairo_pdf, bg = "white")
  cat(sprintf("figure -> output/figures/%s.pdf  (true A4 portrait, 210 x 297 mm, %g mm margin, prints at 100%%)\n",
              STEM, MARGIN_MM))
}
ggsave(file.path(params$figure_dir, paste0(STEM, ".png")), fig,
       width = PG_W, height = PG_H, dpi = DPI, bg = "white", limitsize = FALSE)
cat(sprintf("figure -> output/figures/%s.png  (%g ppi)\n", STEM, DPI))

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
