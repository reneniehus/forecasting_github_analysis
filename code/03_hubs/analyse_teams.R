# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Modelling teams across the last three seasons ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Counts TEAMS, not models. Model identifiers are `<team>-<model>`, so several entries
# in a hub belong to one group: MRC_GIDA alone contributes five models. The question
# "how many teams forecast" is about groups, and answering it with model counts inflates
# participation roughly two-fold.
#
# Seasons run 1 Oct -> 30 Sep, which puts the October-2024 hub reorganisation exactly on
# a season boundary and keeps each respiratory winter inside one season:
#   2023/24  2023-10-01 .. 2024-09-30   (RespiCast launches Nov 2023)
#   2024/25  2024-10-01 .. 2025-09-30
#   2025/26  2025-10-01 .. 2026-09-30   (in progress)
#
# Sources: the four RespiCast-era hubs from live clones (directory + filename is enough
# -- team is the folder, round is the filename prefix), plus the European COVID-19
# Forecast Hub archive via the committed output/hub_submissions.csv, so COVID-19
# hospitalisations is covered for 2023/24 too, when RespiCast-Covid19 did not yet exist.
#
# Not teams, and excluded: respicast-hubEnsemble / respicast-quantileBaseline and
# EuroCOVIDhub-ensemble / EuroCOVIDhub-baseline are hub products, and TestTeam is a
# validation artefact. ECDC is kept but reported separately: it coordinates the hub and
# runs in-house models every week, so counting it among contributors overstates how much
# external modelling capacity the hub attracts.
#
# Run:  Rscript code/03_hubs/analyse_teams.R   ->  output/teams_by_season.csv
#                                                  output/team_seasons.csv

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()

HUBS <- tribble(
  ~dir,                                                                   ~hub,
  Sys.getenv("FLU_DIR",       "/workspace/emh-flu-forecast-hub_archive"), "flu_archive",
  Sys.getenv("ARI_DIR",       "/workspace/emh-ari-forecast-hub_archive"), "ari_archive",
  Sys.getenv("SYNDROMIC_DIR", "/workspace/emh-syndromic"),                "syndromic",
  Sys.getenv("COVID_DIR",     "/workspace/emh-covid"),                    "covid"
)
missing <- HUBS$dir[!dir.exists(HUBS$dir)]
if (length(missing)) stop("Clone(s) not found: ", paste(missing, collapse = ", "),
                          "\nThe committed output/team_seasons.csv holds the extracted result.")

HUB_PRODUCT <- c("respicast-hubEnsemble", "respicast-quantileBaseline",
                 "EuroCOVIDhub-ensemble", "EuroCOVIDhub-baseline")
NOT_A_TEAM  <- c("TestTeam")
COORDINATOR <- "ECDC"

SEASONS <- tribble(
  ~season,   ~start,                 ~end,
  "2023/24", as.Date("2023-10-01"),  as.Date("2024-09-30"),
  "2024/25", as.Date("2024-10-01"),  as.Date("2025-09-30"),
  "2025/26", as.Date("2025-10-01"),  as.Date("2026-09-30")
)

# ---- |-1. every (model, round) in the RespiCast-era hubs ----
step("Scanning the RespiCast-era hubs")
live <- pmap_dfr(list(HUBS$dir, HUBS$hub), function(dir, hub) {
  fs <- list.files(file.path(dir, "model-output"), pattern = "\\.csv$", recursive = TRUE)
  tibble(hub         = hub,
         model       = dirname(fs),
         origin_date = as.Date(regmatches(basename(fs), regexpr("^\\d{4}-\\d{2}-\\d{2}", basename(fs))))) %>%
    filter(!is.na(origin_date))
})
say(sprintf("live hubs: %d submissions, %d model ids", nrow(live), n_distinct(live$model)))

# ---- |-2. the archive, for COVID-19 hospitalisations before Oct 2024 ----
arch <- read_csv(file.path(params$output_dir, "hub_submissions.csv"), show_col_types = FALSE) %>%
  filter(hub == "covid_archive", indicator == "COVID-19 hospitalisations") %>%
  transmute(hub, model, origin_date = as.Date(origin_date))
say(sprintf("archive:   %d submissions, %d model ids, %s -> %s",
            nrow(arch), n_distinct(arch$model), min(arch$origin_date), max(arch$origin_date)))

subs <- bind_rows(live, arch) %>%
  filter(!model %in% HUB_PRODUCT) %>%
  mutate(team = sub("-.*$", "", model)) %>%          # `<team>-<model>`; team is the prefix
  filter(!team %in% NOT_A_TEAM) %>%
  rowwise() %>%
  mutate(season = SEASONS$season[which(origin_date >= SEASONS$start & origin_date <= SEASONS$end)][1]) %>%
  ungroup() %>%
  filter(!is.na(season))

# ---- |-3. team x season presence ----
grid <- subs %>%
  group_by(team, season) %>%
  summarise(models = n_distinct(model), rounds = n_distinct(origin_date), .groups = "drop") %>%
  complete(team, season = SEASONS$season, fill = list(models = 0L, rounds = 0L)) %>%
  mutate(present = rounds > 0)

# ---- |-4. classify each team's trajectory across the three seasons ----
# Season codes alone mislead at the boundaries: the archive closed on 2024-10-14, two
# weeks into 2024/25, so a team that in fact left with it still scores "1" for that
# season. Carry the actual first and last submission dates, the longest gap between
# consecutive rounds (an intra-season pause the season code cannot see), and whether the
# team has submitted since EU/EEA rounds resumed on 12 Aug 2026.
#
# Two gap measures, because a raw one is dominated by an outage that is not the team's:
#   max_gap_wk     longest gap over the team's whole record
#   max_gap_post   longest gap since the Oct-2024 reorganisation, which excludes the
#                  ~25-week summer-2024 hiatus when the hubs themselves were not running
LAST_ROUND <- max(subs$origin_date)
REORG      <- as.Date("2024-10-01")
gapwk <- function(d) { d <- sort(unique(d)); if (length(d) < 2) 0L else as.integer(max(diff(d)) / 7) }
span <- subs %>%
  group_by(team) %>%
  summarise(first_sub = min(origin_date), last_sub = max(origin_date),
            max_gap_wk   = gapwk(origin_date),
            max_gap_post = gapwk(origin_date[origin_date >= REORG]),
            .groups = "drop") %>%
  # The ERVISS outage (24 Jun - 5 Aug 2026) suspended EU/EEA rounds, so absence over it
  # is not evidence a team left. "Returned" means a submission in one of the three rounds
  # since data came back on 12 Aug 2026.
  mutate(currently_active = last_sub >= as.Date("2026-08-12"),
         last_seen_pre_outage = !currently_active & last_sub >= as.Date("2026-04-01"))

pattern <- grid %>%
  arrange(team, season) %>%
  group_by(team) %>%
  summarise(seasons_active = sum(present),
            first_season   = min(season[present]),
            last_season    = max(season[present]),
            code           = paste0(ifelse(present, "1", "0"), collapse = ""),
            rounds         = sum(rounds),
            models         = max(models),
            .groups = "drop") %>%
  left_join(span, by = "team") %>%
  mutate(status = case_when(
    code == "111" ~ "Continuous (all three)",
    code == "011" ~ "Joined in 2024/25, still active",
    code == "001" ~ "Joined in 2025/26",
    code == "110" ~ "Stopped after 2024/25",
    code == "100" ~ "Stopped after 2023/24",
    code == "101" ~ "Paused 2024/25, returned",
    code == "010" ~ "Active in 2024/25 only",
    TRUE          ~ code),
    role = ifelse(team == COORDINATOR, "Hub coordinator (in-house)", "External team")) %>%
  arrange(role, desc(seasons_active), team)

by_season <- grid %>%
  filter(present) %>%
  mutate(role = ifelse(team == COORDINATOR, "coordinator", "external")) %>%
  group_by(season) %>%
  summarise(teams_all = n_distinct(team),
            teams_external = n_distinct(team[role == "external"]),
            models_all = sum(models), .groups = "drop")

dir.create(params$output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(pattern,   file.path(params$output_dir, "team_seasons.csv"))
write_csv(by_season, file.path(params$output_dir, "teams_by_season.csv"))

# ---- |-5. the numbers ----
step("Teams per season")
print(as.data.frame(by_season))

step("Trajectories across the three seasons")
pattern %>% count(role, status, name = "teams") %>% arrange(role, desc(teams)) %>% as.data.frame() %>% print()

step("Per team")
pattern %>%
  transmute(team, role = ifelse(role == "External team", "external", "ECDC"), code,
            first_sub, last_sub, rounds, models, max_gap_wk, max_gap_post,
            currently_active, last_seen_pre_outage) %>%
  as.data.frame() %>% print(row.names = FALSE)

step("Headline")
ext <- filter(pattern, role == "External team")
say(sprintf("%d distinct external teams submitted at least once across the three seasons", nrow(ext)))
say(sprintf("  %d active in all three seasons; %d still submitting as of %s",
            sum(ext$code == "111"), sum(ext$currently_active), LAST_ROUND))
say(sprintf("  %d joined after 2023/24; %d have stopped", sum(ext$code %in% c("011", "001")),
            sum(!ext$currently_active)))
say(sprintf("  %d last seen before the 2026 ERVISS outage - left or not yet returned: %s",
            sum(ext$last_seen_pre_outage), paste(ext$team[ext$last_seen_pre_outage], collapse = ", ")))
say(sprintf("  longest post-reorganisation pause by a team that came back: %d weeks",
            max(ext$max_gap_post[ext$currently_active])))
say(sprintf("median %d rounds per external team (range %d-%d)",
            median(ext$rounds), min(ext$rounds), max(ext$rounds)))
