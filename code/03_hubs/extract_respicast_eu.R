# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Fresh RespiCast participation, restricted to EU/EEA countries ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Reads the four RespiCast-era hubs straight from live clones, so the snapshot is
# current, and counts EVERYTHING on EU/EEA locations only:
#   n_models      models that submitted at least one EU/EEA location that round
#   eu_countries  EU/EEA locations in the published ensemble that round
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

weekly <- all %>%
  mutate(week = lubridate::floor_date(as.Date(origin_date), "week", week_start = 1)) %>%
  group_by(indicator, week) %>%
  summarise(n_models     = n_distinct(model[!model %in% c(ENS, BASE)]),
            has_ensemble = any(model == ENS),
            eu_countries = n_distinct(location[model == ENS]),
            .groups = "drop") %>%
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
