# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Modelled Smooth Point (MSP) from the RespiCast ensemble ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# An MSP is the ensemble's own smooth read of a week, obtained by extrapolating its
# short-horizon trend BACK onto that week rather than by taking the nowcast directly:
#
#   f1, f2  = ensemble median 1 and 2 weeks ahead, from one round
#   log-linear through (1, log f1) and (2, log f2), evaluated at horizon 0:
#       log MSP = 2*log(f1) - log(f2)      ->      MSP = f1^2 / f2
#
# Geometric, not arithmetic: incidence grows multiplicatively, so a straight line in
# log space is the natural trend and the MSP is f1 * (f1/f2), i.e. f1 carried one more
# week back along the ensemble's own weekly growth ratio.
#
# ---- which week does an MSP belong to? ----
# Horizon 0 of the round. RespiCast anchors horizons to DATA AVAILABILITY, not to the
# calendar: in the round of 2026-09-16, horizon -1 is the last reported week (W35),
# horizon 0 the first unreported one (W36), horizon 1 is W37. So the MSP from that
# round is the W36 point. The leading week (W37 here) has no round of its own yet and
# is handled by the fallback below.
#
# ---- anchoring is derived, never trusted ----
# The 2023/24 archives contain rounds where one horizon carries two different
# target_end_dates (177 of 5,545 round x indicator x location groups) -- a stale ladder
# left beside the real one. So the anchor is RECOVERED: A = target_end_date - 7*horizon
# for every row, take the modal A, and keep only the rows consistent with it. This also
# makes the legacy "N wk ahead inc hosp" hub fall out of the same code path, since its
# horizon convention differs but its target_end_dates do not lie.
#
# ---- the leading edge ----
# The most recent surveillance week has no round anchored on it yet, so no f1/f2 pair
# exists to extrapolate from. There the one-week-ahead forecast of the latest round is
# used as the MSP directly (source = "h1_nowcast"). Applied ONLY at the live leading
# edge -- a series that stopped being forecast in 2024 gets no synthetic tail point.
#
# Sources: the four RespiCast-era hubs (hubverse layout) plus the European COVID-19
# Forecast Hub archive (legacy layout), which carries COVID-19 hospitalisations back to
# end-2023.
#
# ---- the ensemble median is its 0.5 QUANTILE, not its `median` row ----
# The hubverse `median` output type is optional, and several members (e.g. ISI-LightGBM,
# DHauser-FluChronos) publish quantiles only. The hub builds its ensemble `median` row
# from just the members that supplied one, but its quantiles from all of them -- so the
# two disagree. Across every RespiCast-era ensemble prediction carrying both, they differ
# by >1% in 59.5% of cases and by >10% in 24%, and in 13% the `median` row lies outside
# the ensemble's own 25-75% range (e.g. Belgium ARI, round 2026-09-30: `median` row 620,
# 0.5 quantile 739 at h1 -- the `median` row there is simply IceLab-EDM's value, the
# middle of the three members that published one). The 0.5 quantile combines every member
# and is coherent with the 50% interval, so it is the ensemble median used throughout;
# the `median` row is a fallback only where a round has no 0.5 quantile.
#
# ---- only FINAL rounds ----
# The hubs rebuild a provisional ensemble several times while a round is open (Sun-Wed,
# 10:00 and 18:00 UTC), under the round's own origin date, from whatever has been
# submitted so far. On 7 Oct 2026 the open round's ensemble held 10 of the 13 models that
# made the previous round. A round is final once its Wednesday has passed, so only rounds
# with origin_date < AS_OF are used. AS_OF defaults to today; set it to reproduce a past
# vintage exactly.
#
# The ensemble's interquartile range (quantiles 0.25 / 0.75) is carried alongside the
# median and written per round and horizon to output/ensemble_forecasts.csv, which is
# where the figures take their 50% intervals from.
#
# Run:  Rscript code/03_hubs/compute_msp.R   ->  output/msp_weekly.csv
#                                                output/ensemble_forecasts.csv

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()

MODERN <- tribble(
  ~dir,                                                                     ~hub,
  Sys.getenv("FLU_DIR",       "/workspace/emh-flu-forecast-hub_archive"),   "flu_archive",
  Sys.getenv("ARI_DIR",       "/workspace/emh-ari-forecast-hub_archive"),   "ari_archive",
  Sys.getenv("SYNDROMIC_DIR", "/workspace/emh-syndromic"),                  "syndromic",
  Sys.getenv("COVID_DIR",     "/workspace/emh-covid"),                      "covid"
)
LEGACY  <- Sys.getenv("COVID_ARCHIVE_DIR", "/workspace/emh-covid-archive")
ENS_MOD <- "respicast-hubEnsemble"
ENS_LEG <- "EuroCOVIDhub-ensemble"
START   <- as.Date("2023-10-01")          # RespiCast era; nothing earlier is in scope
AS_OF   <- as.Date(Sys.getenv("AS_OF", as.character(Sys.Date())))   # rounds before this are final
QTL     <- c("q25" = 0.25, "median" = 0.5, "q75" = 0.75)

LABEL <- c("hospital admissions" = "COVID-19 hospitalisations",
           "ILI incidence" = "ILI incidence", "ARI incidence" = "ARI incidence")
EU_EEA <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IT",
            "LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","IS","LI","NO")

missing <- c(MODERN$dir[!dir.exists(MODERN$dir)], LEGACY[!dir.exists(LEGACY)])
if (length(missing)) stop("Clone(s) not found: ", paste(missing, collapse = ", "),
                          "\nThe committed output/msp_weekly.csv holds the extracted result.")

# ---- |-1. read both hub layouts into one shape ----
# indicator | location | origin_date | horizon | target_end_date | qtl | value
step("Reading ensemble medians and interquartile ranges")

read_modern <- function(dir, hub) {
  fs <- list.files(file.path(dir, "model-output", ENS_MOD), pattern = "\\.csv$", full.names = TRUE)
  say(sprintf("%-12s %4d rounds", hub, length(fs)))
  map_dfr(fs, function(f) {
    x <- tryCatch(data.table::fread(f, showProgress = FALSE), error = function(e) NULL)
    if (is.null(x) || !nrow(x)) return(NULL)
    q <- suppressWarnings(as.numeric(x$output_type_id))
    as_tibble(x) %>%
      mutate(qtl = case_when(output_type == "quantile" & abs(q - 0.50) < 1e-9    ~ "median",
                             output_type == "median"                             ~ "median_row",
                             output_type == "quantile" & abs(q - 0.25) < 1e-9    ~ "q25",
                             output_type == "quantile" & abs(q - 0.75) < 1e-9    ~ "q75")) %>%
      filter(!is.na(qtl)) %>%
      transmute(hub = hub,
                indicator       = recode(as.character(target), !!!LABEL, .default = as.character(target)),
                location        = as.character(location),
                origin_date     = as.Date(origin_date),
                horizon         = as.integer(horizon),
                target_end_date = as.Date(target_end_date),
                qtl,
                value           = as.numeric(value))
  })
}

# Legacy: horizon lives inside the target string, the median is quantile 0.5, and only
# the incident-hospitalisation target is in scope (the hub also carried cases/deaths).
read_legacy <- function(dir) {
  fs <- list.files(file.path(dir, "data-processed", ENS_LEG), pattern = "\\.csv$", full.names = TRUE)
  fs <- fs[as.Date(substr(basename(fs), 1, 10)) >= START]
  say(sprintf("%-12s %4d rounds (in scope)", "covid_archive", length(fs)))
  map_dfr(fs, function(f) {
    x <- tryCatch(data.table::fread(f, showProgress = FALSE), error = function(e) NULL)
    if (is.null(x) || !nrow(x)) return(NULL)
    q <- suppressWarnings(as.numeric(x$quantile))
    as_tibble(x) %>%
      mutate(qtl = names(QTL)[match(round(q, 3), QTL)]) %>%
      filter(str_detect(target, "wk ahead inc hosp"), type == "quantile", !is.na(qtl)) %>%
      transmute(hub = "covid_archive",
                indicator       = "COVID-19 hospitalisations",
                location        = as.character(location),
                origin_date     = as.Date(forecast_date),
                horizon         = as.integer(str_extract(target, "^\\d+")),
                target_end_date = as.Date(target_end_date),
                qtl,
                value           = as.numeric(value))
  })
}

every <- bind_rows(pmap_dfr(list(MODERN$dir, MODERN$hub), read_modern), read_legacy(LEGACY)) %>%
  filter(origin_date >= START, is.finite(value), !is.na(target_end_date), horizon >= 1)

open_rounds <- every %>% filter(origin_date >= AS_OF) %>% distinct(hub, origin_date)
if (nrow(open_rounds))
  say(sprintf("excluded %d provisional ensemble(s) of a round still open on %s: %s",
              nrow(open_rounds), AS_OF,
              paste(sprintf("%s %s", open_rounds$hub, open_rounds$origin_date), collapse = ", ")))
every <- filter(every, origin_date < AS_OF)

# the ensemble median: the 0.5 quantile, with the `median` row only as a fallback
KEYS   <- c("hub", "indicator", "location", "origin_date", "horizon", "target_end_date")
med_q  <- filter(every, qtl == "median")
med_rw <- filter(every, qtl == "median_row")
cmp <- inner_join(med_q, med_rw, by = KEYS, suffix = c("_q", "_row")) %>%
  mutate(rel = abs(value_row - value_q) / pmax(abs(value_q), 1e-9))
say(sprintf("ensemble `median` row vs 0.5 quantile: differ by >1%% in %.1f%%, by >10%% in %.1f%% of %d predictions -- 0.5 quantile used",
            100 * mean(cmp$rel > 0.01), 100 * mean(cmp$rel > 0.10), nrow(cmp)))
every <- bind_rows(filter(every, qtl != "median_row"),
                   anti_join(med_rw, med_q, by = KEYS) %>% mutate(qtl = "median"))

raw <- filter(every, qtl == "median")
say(sprintf("%d ensemble median rows, %s -> %s (final rounds only)",
            nrow(raw), min(raw$origin_date), max(raw$origin_date)))

# ---- |-2. recover each round's anchor and drop rows inconsistent with it ----
step("Recovering round anchors")
anchored <- raw %>%
  mutate(anchor = target_end_date - 7L * horizon) %>%
  group_by(hub, indicator, location, origin_date) %>%
  # modal anchor; ties broken towards the LATER one, which is the live ladder in every
  # observed case (the stale duplicate always sits two weeks behind)
  mutate(anchor_star = { tb <- table(anchor)
                         as.Date(max(names(tb)[tb == max(tb)])) }) %>%
  ungroup()

dropped <- sum(anchored$anchor != anchored$anchor_star)
say(sprintf("%d of %d rows dropped as inconsistent with their round's anchor (%.2f%%)",
            dropped, nrow(anchored), 100 * dropped / nrow(anchored)))
anchored <- filter(anchored, anchor == anchor_star)
anchors  <- distinct(anchored, hub, indicator, location, origin_date, anchor_star)

# ---- |-3. the MSP itself ----
step("Computing MSPs")
pairs <- anchored %>%
  filter(horizon %in% 1:2) %>%
  select(hub, indicator, location, origin_date, week = anchor_star, horizon, value) %>%
  pivot_wider(names_from = horizon, values_from = value, names_prefix = "f") %>%
  filter(!is.na(f1), !is.na(f2))

msp_extrap <- pairs %>%
  # log-linear back-extrapolation; undefined if either point sits at or below zero
  mutate(msp    = ifelse(f1 > 0 & f2 > 0, exp(2 * log(f1) - log(f2)), NA_real_),
         ratio  = ifelse(f2 > 0, f1 / f2, NA_real_),
         source = "extrapolated") %>%
  filter(!is.na(msp))
say(sprintf("%d extrapolated MSPs (%d pairs dropped: a zero at f1 or f2)",
            nrow(msp_extrap), nrow(pairs) - nrow(msp_extrap)))

# Leading edge: the newest surveillance week has no round anchored on it yet. Use that
# round's one-week-ahead forecast as its MSP -- but only for series still being
# forecast, i.e. rounds that ARE the latest round for their indicator.
latest_round <- pairs %>% group_by(indicator) %>% summarise(latest = max(origin_date), .groups = "drop")
msp_edge <- pairs %>%
  inner_join(latest_round, by = "indicator") %>%
  filter(origin_date == latest) %>%
  transmute(hub, indicator, location, origin_date, week = week + 7L,
            f1, f2, msp = f1, ratio = ifelse(f2 > 0, f1 / f2, NA_real_), source = "h1_nowcast") %>%
  anti_join(msp_extrap, by = c("indicator", "location", "week"))   # never overwrite a real MSP
say(sprintf("%d leading-edge MSPs from the latest round of each indicator", nrow(msp_edge)))

msp <- bind_rows(msp_extrap, msp_edge) %>%
  # one row per series-week: prefer the extrapolated value, then the most recent round
  arrange(indicator, location, week, source == "h1_nowcast", desc(origin_date)) %>%
  distinct(indicator, location, week, .keep_all = TRUE) %>%
  transmute(indicator, location,
            eu_eea    = location %in% EU_EEA,
            week_end  = week,                                   # Sunday ending the MSP week
            iso_week  = sprintf("%d-W%02d", lubridate::isoyear(week), lubridate::isoweek(week)),
            msp, f1, f2, ratio, source, hub, origin_date) %>%
  arrange(indicator, location, week_end)

# ---- |-3b. the ensemble's own forecasts, median with the 50% interval ----
# Same anchor filter as the MSP, so the quantile rows of a stale duplicate ladder are
# dropped exactly as its medians were.
fc <- every %>%
  inner_join(anchors, by = c("hub", "indicator", "location", "origin_date")) %>%
  filter(target_end_date - 7L * horizon == anchor_star, horizon %in% 1:4) %>%
  select(hub, indicator, location, origin_date, horizon, target_end_date, qtl, value) %>%
  distinct(hub, indicator, location, origin_date, horizon, qtl, .keep_all = TRUE) %>%
  pivot_wider(names_from = qtl, values_from = value) %>%
  mutate(eu_eea = location %in% EU_EEA) %>%
  select(indicator, location, eu_eea, origin_date, horizon, target_end_date, q25, median, q75, hub) %>%
  arrange(indicator, location, origin_date, horizon)
write_csv(fc, file.path(params$output_dir, "ensemble_forecasts.csv"))
say(sprintf("wrote output/ensemble_forecasts.csv | %d rows | %d with a full 50%% interval",
            nrow(fc), sum(!is.na(fc$q25) & !is.na(fc$q75))))

OUT <- file.path(params$output_dir, "msp_weekly.csv")
write_csv(msp, OUT)
say(sprintf("wrote %s | %d rows | %s -> %s", OUT, nrow(msp), min(msp$week_end), max(msp$week_end)))

# ---- |-4. what came out ----
step("MSP coverage by indicator")
msp %>%
  group_by(indicator) %>%
  summarise(weeks = n_distinct(week_end), countries = n_distinct(location),
            eu_countries = n_distinct(location[eu_eea]), rows = n(),
            first = min(week_end), last = max(week_end), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

step("Leading edge (latest MSP week per indicator)")
msp %>% group_by(indicator) %>% filter(week_end == max(week_end)) %>%
  summarise(week = first(iso_week), week_end = first(week_end), countries = n(),
            source = paste(unique(source), collapse = "/"), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

# A very large f1/f2 means the ensemble expects a steep fall, which the back-extrapolation
# turns into a steep rise. Report the tail so it is never silently trusted.
step("Extrapolation stability")
say(sprintf("f1/f2 ratio: median %.2f, 1st-99th pct %.2f-%.2f, %d rows beyond 2x either way",
            median(msp$ratio, na.rm = TRUE),
            quantile(msp$ratio, 0.01, na.rm = TRUE), quantile(msp$ratio, 0.99, na.rm = TRUE),
            sum(msp$ratio > 2 | msp$ratio < 0.5, na.rm = TRUE)))
