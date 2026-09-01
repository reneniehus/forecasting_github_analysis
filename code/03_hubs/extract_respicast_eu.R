# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Fresh RespiCast participation, restricted to EU/EEA countries ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Reads the four RespiCast-era hubs straight from live clones, so the snapshot is
# current, and counts EVERYTHING on EU/EEA locations only:
#   n_models      models that submitted at least one EU/EEA location that round
#   eu_countries  EU/EEA locations in the published ensemble that round
#
# COVID-19 hospitalisations is then back-filled before October 2024 from the European
# COVID-19 Forecast Hub archive (see the splice block below), so the indicator's record
# is continuous from 2021-07-26 rather than beginning when RespiCast-Covid19 opened.
#
# The EU/EEA restriction is what makes the 2026 break visible. Through July 2026 the
# syndromic ensemble was still produced, but only for Switzerland, England and
# Northern Ireland -- non-EU countries whose data reaches the hub via WHO FluID
# rather than ERVISS. Counting all locations hides the outage; counting EU/EEA
# locations shows it as the seven-round hole it was for member states.
#
# Clones (override with env vars):
#   FLU_DIR /workspace/emh-flu-forecast-hub_archive   ARI_DIR /workspace/emh-ari-forecast-hub_archive
#   SYNDROMIC_DIR /workspace/emh-syndromic            COVID_DIR /workspace/emh-covid
#
# Run:  Rscript code/03_hubs/extract_respicast_eu.R   ->  output/respicast_eu_weekly.csv

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()

HUBS <- tribble(
  ~dir,                                                                        ~hub,
  Sys.getenv("FLU_DIR",       "/workspace/emh-flu-forecast-hub_archive"),      "flu_archive",
  Sys.getenv("ARI_DIR",       "/workspace/emh-ari-forecast-hub_archive"),      "ari_archive",
  Sys.getenv("SYNDROMIC_DIR", "/workspace/emh-syndromic"),                     "syndromic",
  Sys.getenv("COVID_DIR",     "/workspace/emh-covid"),                         "covid"
)
missing <- HUBS$dir[!dir.exists(HUBS$dir)]
if (length(missing)) stop("Clone(s) not found: ", paste(missing, collapse = ", "),
                          "\nThe committed output/respicast_eu_weekly.csv holds the extracted result.")

# EU/EEA = 27 member states + Iceland, Liechtenstein, Norway. Deliberately excludes
# CH and every GB-* code, which is precisely what the 2026 break turns on.
EU_EEA <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IT",
            "LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","IS","LI","NO")
ENS <- "respicast-hubEnsemble"; BASE <- "respicast-quantileBaseline"
LABEL <- c("hospital admissions" = "COVID-19 hospitalisations",
           "ILI incidence" = "ILI incidence", "ARI incidence" = "ARI incidence")

# Also record, per hub-round, whether the hub RAN AT ALL (any submission, any
# location). That separates a true zero -- hub operating but covering no EU/EEA
# country, as in the 2026 transition -- from the hub simply not running, as in the
# summer-2024 gap between hub generations. The figure draws the first as 0 and the
# second as a break.
read_one <- function(f, hub, model) {
  x <- tryCatch(data.table::fread(f, select = c("origin_date", "target", "location"),
                                  showProgress = FALSE, colClasses = list(character = "origin_date")),
                error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(NULL)
  as_tibble(x) %>%
    filter(location %in% EU_EEA) %>%                    # EU/EEA only, everywhere
    mutate(indicator = recode(as.character(target), !!!LABEL, .default = as.character(target))) %>%
    distinct(origin_date, indicator, location) %>%
    mutate(hub = hub, model = model)
}

step("Scanning the four RespiCast-era hubs (EU/EEA locations only)")
all <- pmap_dfr(list(HUBS$dir, HUBS$hub), function(dir, hub) {
  fs <- list.files(file.path(dir, "model-output"), pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  say(sprintf("%-12s %4d files", hub, length(fs)))
  map_dfr(fs, ~ read_one(.x, hub, basename(dirname(.x))))
})

# every round present in each hub, before the EU/EEA filter
ran <- pmap_dfr(list(HUBS$dir, HUBS$hub), function(dir, hub) {
  fs <- list.files(file.path(dir, "model-output"), pattern = "\\.csv$", recursive = TRUE)
  tibble(hub = hub, origin_date = regmatches(basename(fs), regexpr("^\\d{4}-\\d{2}-\\d{2}", basename(fs))))
}) %>% filter(!is.na(origin_date)) %>%
  mutate(week = lubridate::floor_date(as.Date(origin_date), "week", week_start = 1)) %>%
  distinct(hub, week) %>%
  # which indicators each hub carries, so "ran" maps onto indicators
  mutate(inds = case_when(hub == "covid" ~ "COVID-19 hospitalisations",
                          hub == "flu_archive" ~ "ILI incidence",
                          hub == "ari_archive" ~ "ARI incidence",
                          TRUE ~ "ILI incidence|ARI incidence")) %>%
  separate_rows(inds, sep = "\\|") %>% rename(indicator = inds) %>%
  distinct(indicator, week) %>% mutate(hub_ran = TRUE)

weekly_live <- all %>%
  mutate(week = lubridate::floor_date(as.Date(origin_date), "week", week_start = 1)) %>%
  group_by(indicator, week) %>%
  summarise(n_models     = n_distinct(model[!model %in% c(ENS, BASE)]),
            has_ensemble = any(model == ENS),
            eu_countries = n_distinct(location[model == ENS]),
            .groups = "drop")

# ---- |-COVID-19 hospitalisations before October 2024 ----
# RespiCast-Covid19 only opens on 2024-10-21. Before that the same indicator was
# carried by the European COVID-19 Forecast Hub (covid19-forecast-hub-europe_archive),
# whose record runs 2021-07-26 -> 2024-10-14 with no overlap. Splicing it in makes the
# COVID line continuous across the plotted window instead of starting mid-2024.
#
# Source is the committed output/hub_submissions.csv rather than a fresh clone: the
# archive is frozen (last commit 14 Apr 2025), so re-scanning it cannot change the
# answer, and that file already carries the per-file `locations` list the EU/EEA filter
# needs. Counting rules match the live hubs above -- models exclude the ensemble and the
# baseline; eu_countries counts EU/EEA locations in the published ensemble.
step("Splicing in the European COVID-19 Forecast Hub archive (pre-Oct 2024)")
arch <- read_csv(file.path(params$output_dir, "hub_submissions.csv"), show_col_types = FALSE) %>%
  filter(hub == "covid_archive", indicator == "COVID-19 hospitalisations") %>%
  mutate(week = lubridate::floor_date(as.Date(origin_date), "week", week_start = 1)) %>%
  separate_rows(locations, sep = ",") %>%
  rename(location = locations) %>%
  filter(location %in% EU_EEA)

FIRST_LIVE <- min(weekly_live$week[weekly_live$indicator == "COVID-19 hospitalisations"])
weekly_arch <- arch %>%
  filter(week < FIRST_LIVE) %>%                        # no double-count at the handover
  group_by(indicator, week) %>%
  summarise(n_models     = n_distinct(model[role == "model"]),
            has_ensemble = any(role == "ensemble"),
            eu_countries = n_distinct(location[role == "ensemble"]),
            .groups = "drop")
say(sprintf("archive: %d weeks, %s -> %s (RespiCast-Covid19 opens %s)",
            nrow(weekly_arch), min(weekly_arch$week), max(weekly_arch$week), FIRST_LIVE))

# the archive ran every week it has a file, so those weeks are hub_ran = TRUE as well
ran <- bind_rows(ran, transmute(weekly_arch, indicator, week, hub_ran = TRUE)) %>%
  distinct(indicator, week, .keep_all = TRUE)

weekly <- bind_rows(weekly_live, weekly_arch) %>%
  arrange(indicator, week) %>%
  full_join(ran, by = c("indicator", "week")) %>%
  mutate(across(c(n_models, eu_countries), ~ ifelse(is.na(.x), 0L, .x)),
         has_ensemble = ifelse(is.na(has_ensemble), FALSE, has_ensemble),
         hub_ran      = ifelse(is.na(hub_ran), FALSE, hub_ran)) %>%
  arrange(indicator, week)

OUT <- file.path(params$output_dir, "respicast_eu_weekly.csv")
write_csv(weekly, OUT)
say(sprintf("wrote %s | %s -> %s | %d rows", OUT, min(weekly$week), max(weekly$week), nrow(weekly)))

# ---- |-the EU/EEA gap, per indicator ----
step("EU/EEA ensemble gap")
for (ind in unique(weekly$indicator)) {
  d <- weekly %>% filter(indicator == ind) %>% arrange(week)
  g <- tibble(week = seq(min(d$week), max(d$week), by = 7)) %>%
    left_join(d, by = "week") %>%
    mutate(eu_countries = ifelse(is.na(eu_countries), 0L, eu_countries))
  r <- rle(g$eu_countries == 0)
  i <- which(r$values & r$lengths == max(r$lengths[r$values]))[1]
  if (!length(i) || is.na(i)) next
  end <- cumsum(r$lengths)[i]; start <- end - r$lengths[i] + 1
  say(sprintf("%-27s longest run with 0 EU/EEA countries: %d rounds (%s -> %s)",
              ind, r$lengths[i], g$week[start], g$week[end]))
}
