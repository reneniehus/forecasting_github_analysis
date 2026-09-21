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
# Sources: the four RespiCast-era hubs (hubverse layout, output_type == "median") plus
# the European COVID-19 Forecast Hub archive (legacy layout, quantile 0.5), which is
# what carries COVID-19 hospitalisations back to end-2023.
#
# Run:  Rscript code/03_hubs/compute_msp.R   ->  output/msp_weekly.csv

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

LABEL <- c("hospital admissions" = "COVID-19 hospitalisations",
           "ILI incidence" = "ILI incidence", "ARI incidence" = "ARI incidence")
EU_EEA <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IT",
            "LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","IS","LI","NO")

missing <- c(MODERN$dir[!dir.exists(MODERN$dir)], LEGACY[!dir.exists(LEGACY)])
if (length(missing)) stop("Clone(s) not found: ", paste(missing, collapse = ", "),
                          "\nThe committed output/msp_weekly.csv holds the extracted result.")

# ---- |-1. read both hub layouts into one shape ----
# indicator | location | origin_date | horizon | target_end_date | value
step("Reading ensemble medians")

read_modern <- function(dir, hub) {
  fs <- list.files(file.path(dir, "model-output", ENS_MOD), pattern = "\\.csv$", full.names = TRUE)
  say(sprintf("%-12s %4d rounds", hub, length(fs)))
  map_dfr(fs, function(f) {
    x <- tryCatch(data.table::fread(f, showProgress = FALSE), error = function(e) NULL)
    if (is.null(x) || !nrow(x)) return(NULL)
    as_tibble(x) %>%
      filter(output_type == "median") %>%
      transmute(hub = hub,
                indicator       = recode(as.character(target), !!!LABEL, .default = as.character(target)),
                location        = as.character(location),
                origin_date     = as.Date(origin_date),
                horizon         = as.integer(horizon),
                target_end_date = as.Date(target_end_date),
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
    as_tibble(x) %>%
      filter(str_detect(target, "wk ahead inc hosp"),
             type == "quantile", abs(as.numeric(quantile) - 0.5) < 1e-9) %>%
      transmute(hub = "covid_archive",
                indicator       = "COVID-19 hospitalisations",
                location        = as.character(location),
                origin_date     = as.Date(forecast_date),
                horizon         = as.integer(str_extract(target, "^\\d+")),
                target_end_date = as.Date(target_end_date),
                value           = as.numeric(value))
  })
}

raw <- bind_rows(pmap_dfr(list(MODERN$dir, MODERN$hub), read_modern), read_legacy(LEGACY)) %>%
  filter(origin_date >= START, is.finite(value), !is.na(target_end_date), horizon >= 1)
say(sprintf("%d ensemble median rows, %s -> %s", nrow(raw), min(raw$origin_date), max(raw$origin_date)))

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
