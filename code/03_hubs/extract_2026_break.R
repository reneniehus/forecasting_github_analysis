# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### The 2026 TESSy -> EpiPulse break: extract the evidence ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Pairs two independent records week by week:
#   the ERVISS truth-data feed  (EU-ECDC/Respiratory_viruses_weekly_data, whose weekly
#                                "commit_as_of_<date>" commits ARE the publication events)
#   the RespiCast rounds        (the two current hubs' model-output folders)
# so the data outage and the forecast outage can be lined up on one axis.
#
# Needs live clones, because the committed hub_submissions.csv predates the break:
#   ERVISS_DIR     default /workspace/ervissdata
#   SYNDROMIC_DIR  default /workspace/emh-syndromic
#   COVID_DIR      default /workspace/emh-covid
# The result is written to output/break_2026.csv so the figure rebuilds without them.
#
# Run:  Rscript code/03_hubs/extract_2026_break.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()

ERVISS    <- Sys.getenv("ERVISS_DIR",    "/workspace/ervissdata")
SYNDROMIC <- Sys.getenv("SYNDROMIC_DIR", "/workspace/emh-syndromic")
COVID     <- Sys.getenv("COVID_DIR",     "/workspace/emh-covid")
OUT       <- file.path(params$output_dir, "break_2026.csv")

if (!all(dir.exists(c(ERVISS, SYNDROMIC, COVID)))) {
  stop("Live clones not found. Clone the ERVISS data repo and the two RespiCast hubs, ",
       "or set ERVISS_DIR / SYNDROMIC_DIR / COVID_DIR. The committed output/break_2026.csv ",
       "already holds the extracted result.")
}

# ---- |-1. ERVISS: each weekly data publication, from the commit log ----
log <- system(sprintf("git -C %s log --format='%%s'", ERVISS), intern = TRUE)
erviss <- tibble(subject = log) %>%
  filter(str_detect(subject, "^commit_as_of_")) %>%
  mutate(as_of = as.Date(str_extract(subject, "\\d{4}-\\d{2}-\\d{2}"))) %>%
  filter(!is.na(as_of)) %>% distinct(as_of) %>% arrange(as_of)
say(sprintf("ERVISS publications: %d, %s -> %s", nrow(erviss), min(erviss$as_of), max(erviss$as_of)))

# ---- |-2. RespiCast: every round in the two current hubs ----
scan_hub <- function(path, label) {
  dirs <- list.dirs(file.path(path, "model-output"), recursive = FALSE)
  map_dfr(dirs, function(d) {
    f  <- list.files(d, pattern = "\\.csv$")
    dt <- regmatches(f, regexpr("^\\d{4}-\\d{2}-\\d{2}", f))
    if (!length(dt)) return(NULL)
    tibble(hub = label, model = basename(d), origin_date = as.Date(dt))
  })
}
ENS <- "respicast-hubEnsemble"; BASE <- "respicast-quantileBaseline"
rounds <- bind_rows(scan_hub(SYNDROMIC, "RespiCast-SyndromicIndicators"),
                    scan_hub(COVID,     "RespiCast-Covid19")) %>%
  group_by(hub, origin_date) %>%
  summarise(models   = n_distinct(model[!model %in% c(ENS, BASE)]),
            ensemble = any(model == ENS), .groups = "drop")

# ---- |-3. one tidy long table: a row per (source, week) ----
out <- bind_rows(
  erviss %>% transmute(source = "ERVISS data feed", date = as_of, models = NA_integer_, ensemble = NA),
  rounds %>% transmute(source = hub, date = origin_date, models, ensemble)
) %>% arrange(source, date)

dir.create(params$output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(out, OUT)
say(sprintf("wrote %s (%d rows)", OUT, nrow(out)))

# ---- |-4. the numbers behind the statement ----
step("The 2026 break")
gapdays <- function(d) as.integer(diff(d))
e <- erviss$as_of
i <- which.max(gapdays(e))
say(sprintf("ERVISS: last update before pause %s, next %s -> %d days (%.1f weeks) with no data",
            e[i], e[i + 1], as.integer(e[i + 1] - e[i]), as.integer(e[i + 1] - e[i]) / 7))

for (h in unique(rounds$hub)) {
  d <- rounds %>% filter(hub == h, origin_date >= as.Date("2026-04-01")) %>% arrange(origin_date)
  grid <- tibble(origin_date = seq(min(d$origin_date), max(d$origin_date), by = 7)) %>%
    left_join(d, by = "origin_date")
  miss <- grid$origin_date[is.na(grid$models) | grid$models == 0]
  r <- rle(is.na(grid$models) | grid$models == 0)
  say(sprintf("%s: %d round(s) with no forecast since Apr 2026 (%s); longest run %d",
              h, length(miss), paste(format(miss, "%d %b"), collapse = ", "),
              max(c(0, r$lengths[r$values]))))
}
