# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### On which weekday is a forecast round dated? ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Every submission file is named <origin_date>-<team-model>.csv, so the round's
# nominal day-of-week is recoverable for all 10,861 submission cells without
# re-cloning the hubs. This asks: on which weekday were ensembles dated, did that
# change across the 2024 archive -> RespiCast handover, and does it differ by
# indicator or between the ensemble and the models feeding it?
#
# NB this is the round's NOMINAL date (the label the hub gives the week), not the
# moment the file was committed -- that would need the hubs' git history.
#
# Run:  Rscript code/03_hubs/analyse_round_days.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()

# NB namespace it: data.table is loaded after lubridate in setup.R and masks wday()
DAY <- function(d) lubridate::wday(d, label = TRUE, abbr = FALSE, week_start = 1)

s <- read_csv(file.path(params$output_dir, "hub_submissions.csv"), show_col_types = FALSE) %>%
  mutate(origin_date = as.Date(origin_date), weekday = DAY(origin_date))

hdr <- function(x) cat("\n", strrep("-", 78), "\n", x, "\n", strrep("-", 78), "\n", sep = "")

# ---- |-1. the headline: which weekday carries an ensemble round ----
hdr("1. ENSEMBLE rounds by weekday (all hubs, 2021-2026)")
ens <- s %>% filter(role == "ensemble")
ens %>% distinct(hub, indicator, origin_date, weekday) %>%
  count(weekday, name = "rounds") %>% mutate(pct = round(100 * rounds / sum(rounds), 1)) %>%
  arrange(desc(rounds)) %>% as.data.frame() %>% print(row.names = FALSE)

# ---- |-2. by hub / era: does the day move at the handover? ----
hdr("2. ENSEMBLE rounds by hub and era")
ens %>% distinct(hub, era, indicator, origin_date, weekday) %>%
  count(hub, era, weekday, name = "rounds") %>%
  arrange(hub, weekday) %>% as.data.frame() %>% print(row.names = FALSE)

# ---- |-3. by indicator ----
hdr("3. ENSEMBLE rounds by indicator")
ens %>% distinct(indicator, origin_date, weekday) %>%
  count(indicator, weekday, name = "rounds") %>%
  arrange(indicator, desc(rounds)) %>% as.data.frame() %>% print(row.names = FALSE)

# ---- |-4. when did the weekday change? first/last round on each day ----
hdr("4. The span of each weekday convention, per indicator")
ens %>% distinct(indicator, origin_date, weekday) %>%
  group_by(indicator, weekday) %>%
  summarise(rounds = n(), first = min(origin_date), last = max(origin_date), .groups = "drop") %>%
  arrange(indicator, first) %>% as.data.frame() %>% print(row.names = FALSE)

# ---- |-5. do the contributing models use the same day as the ensemble? ----
hdr("5. ALL submissions by role x weekday (is anything off-schedule?)")
s %>% count(role, weekday, name = "submissions") %>%
  pivot_wider(names_from = weekday, values_from = submissions, values_fill = 0) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ---- |-6. the weekday CONVENTION over time (rounds are dated, not scattered) ----
# The legacy hub did not always use Monday: its first rounds are Sunday-dated. So
# rather than call those "off-schedule", trace the convention era by era.
hdr("6. Weekday convention over time - ALL submissions, by quarter")
s %>% distinct(hub, origin_date, weekday) %>%
  mutate(quarter = paste0(year(origin_date), " Q", quarter(origin_date))) %>%
  count(quarter, weekday, name = "rounds") %>%
  pivot_wider(names_from = weekday, values_from = rounds, values_fill = 0) %>%
  arrange(quarter) %>% as.data.frame() %>% print(row.names = FALSE)

hdr("7. Span of each weekday convention (all roles), per hub")
s %>% distinct(hub, origin_date, weekday) %>%
  group_by(hub, weekday) %>%
  summarise(rounds = n(), first = min(origin_date), last = max(origin_date), .groups = "drop") %>%
  arrange(hub, first) %>% as.data.frame() %>% print(row.names = FALSE)

# ---- |-8. the early-2021 Sunday period: was an ensemble ever dated Sunday? ----
hdr("8. The Sunday-dated period (legacy hub) - who submitted then?")
sun <- s %>% filter(weekday == "Sunday")
cat(sprintf("Sunday-dated rounds: %d, %s -> %s\n", n_distinct(sun$origin_date),
            min(sun$origin_date), max(sun$origin_date)))
sun %>% count(role, name = "submissions") %>% as.data.frame() %>% print(row.names = FALSE)
cat("\nSaturday-dated submissions (rare):\n")
s %>% filter(weekday == "Saturday") %>% count(role, model, name = "submissions") %>%
  as.data.frame() %>% print(row.names = FALSE)

cat("\n")
