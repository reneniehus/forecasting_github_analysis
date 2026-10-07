# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Surveillance history before ERVISS's public repo, from RespiCompass ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ERVISS's GitHub repo (EU-ECDC/Respiratory_viruses_weekly_data) starts at 2022-W25 --
# in every file, including its oldest snapshot. The European scenario hub, RespiCompass,
# shipped an earlier and longer ERVISS extract with its 2024/25 influenza round:
#
#   ILI consultation rate      2014-W40 -> 2024-W21   auxiliary-data/influenza/epidemiological/
#   ARI consultation rate      same span, same folder
#   COVID-19 hospital admiss.  2019-W49 -> 2024-W21   target-data/covid-19/
#
# so the left column of the slope monitor can reach back eleven seasons instead of two.
# No back-calculation is involved: RespiCompass also publishes ILI+ (ILI x influenza
# positivity), but it ships ILI itself as auxiliary data, and ILI = ILI+/positivity would
# in any case be ill-conditioned in exactly the inter-season weeks the monitor shows,
# where both numerator and denominator sit near zero.
#
# Splice rule: ERVISS wherever it exists (it is the current vintage, revisions included),
# RespiCompass strictly before ERVISS's first week. On the 2022-W25 -> 2024-W21 overlap
# the two agree exactly for ~84% of country-weeks with a median difference of zero; the
# disagreements are later ERVISS revisions, concentrated in Denmark's ILI (~10%).
#
# RespiCompass is frozen -- its last data week is 2024-W21 -- so this runs once and the
# result is committed to data/respicompass_history.csv. The weekly refresh does not need
# the 2 GB clone.
#
# Run:  Rscript code/03_hubs/extract_respicompass_history.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()

RC  <- Sys.getenv("RESPICOMPASS_DIR", "/workspace/respicompass")
RND <- file.path(RC, "Previous_Rounds", "2024-2025_round_1")
SRC <- c("ILI incidence"             = file.path(RND, "auxiliary-data/influenza/epidemiological/ILIconsultationrate.csv"),
         "ARI incidence"             = file.path(RND, "auxiliary-data/influenza/epidemiological/ARIconsultationrate.csv"),
         "COVID-19 hospitalisations" = file.path(RND, "target-data/covid-19/hospitaladmissions.csv"))
LIVE <- c("ILI incidence"             = "/workspace/emh-syndromic/target-data/ERVISS/latest-ILI_incidence.csv",
          "ARI incidence"             = "/workspace/emh-syndromic/target-data/ERVISS/latest-ARI_incidence.csv",
          "COVID-19 hospitalisations" = "/workspace/emh-covid/target-data/latest-hospital_admissions.csv")
EU_EEA <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IT",
            "LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","IS","LI","NO")

missing <- c(SRC, LIVE)[!file.exists(c(SRC, LIVE))]
if (length(missing)) stop("Not found: ", paste(missing, collapse = ", "),
                          "\nThe committed data/respicompass_history.csv holds the extracted result.")

rc <- imap_dfr(SRC, function(f, ind)
  read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(age == "total", iso2_code %in% EU_EEA) %>%
    transmute(indicator = ind, location = iso2_code,
              week_end = as.Date(week_end_date), value = as.numeric(value)) %>%
    filter(!is.na(value)))

live <- imap_dfr(LIVE, function(f, ind)
  read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(location %in% EU_EEA) %>%
    transmute(indicator = ind, location, week_end = as.Date(truth_date), value = as.numeric(value)) %>%
    filter(!is.na(value)))

# ---- |-agreement on the overlap, per indicator ----
step("RespiCompass vs ERVISS where both report")
both <- inner_join(rc, live, by = c("indicator", "location", "week_end"), suffix = c("_rc", "_ervis"))
both %>%
  group_by(indicator) %>%
  summarise(country_weeks   = n(),
            identical       = sprintf("%.1f%%", 100 * mean(abs(value_rc - value_ervis) <= 0.05)),
            median_rel_diff = median(abs(value_rc - value_ervis)[value_ervis > 0] / value_ervis[value_ervis > 0]),
            .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)

worst <- both %>%
  filter(value_ervis > 0) %>%
  group_by(indicator, location) %>%
  summarise(median_rel_diff = median(abs(value_rc - value_ervis) / value_ervis), n = n(), .groups = "drop") %>%
  filter(median_rel_diff > 0.02) %>% arrange(desc(median_rel_diff))
if (nrow(worst)) { say("country series differing by more than 2% (median), i.e. revised since:"); print(as.data.frame(worst), row.names = FALSE) }

# ---- |-splice only series the overlap shows are on ERVISS's current basis ----
# A revision moves a few weeks by a few percent; a 40% median gap across ~100 weeks (Malta
# and Estonia COVID admissions) means the two files measure different things, and
# splicing them would put a step change at June 2022 that is pure artefact. A series is
# spliced only if it overlaps ERVISS for MIN_OVERLAP weeks AND its median relative
# difference there is within MAX_REL_DIFF. Denmark's ILI (~10%) passes and is flagged in
# the figure caption; anything that fails keeps its ERVISS data and simply loses history.
MAX_REL_DIFF <- 0.15
# A basis difference shows in essentially every overlapping week, so a handful of weeks
# of agreement is enough to rule one out; requiring more only discards good series.
MIN_OVERLAP  <- 5L
validated <- both %>% filter(value_ervis > 0) %>%
  group_by(indicator, location) %>%
  summarise(n = n(), rel = median(abs(value_rc - value_ervis) / value_ervis), .groups = "drop") %>%
  mutate(ok = n >= MIN_OVERLAP & rel <= MAX_REL_DIFF)
rejected <- filter(validated, !ok)
# recorded so the figure caption can name them without re-deriving the rule
write_csv(transmute(rejected, indicator, location, overlap_weeks = n, median_rel_diff = round(rel, 3)),
          here::here("data", "respicompass_not_spliced.csv"))
if (nrow(rejected)) {
  say(sprintf("NOT spliced (overlap < %d weeks or median difference > %.0f%%):", MIN_OVERLAP, 100 * MAX_REL_DIFF))
  print(as.data.frame(mutate(rejected, rel = round(rel, 3))), row.names = FALSE)
}

# Agreement over the series actually spliced, per indicator -- the number a caption may
# quote. Over all series it would include the rejected ones and understate the match.
agreement <- both %>%
  semi_join(filter(validated, ok), by = c("indicator", "location")) %>%
  group_by(indicator) %>%
  summarise(country_weeks = n(), identical_share = mean(abs(value_rc - value_ervis) <= 0.05), .groups = "drop")
write_csv(agreement, here::here("data", "respicompass_agreement.csv"))
say("agreement over the spliced series:")
print(as.data.frame(mutate(agreement, identical_share = sprintf("%.1f%%", 100 * identical_share))), row.names = FALSE)

# ---- |-splice: RespiCompass strictly before ERVISS's first week ----
first_live <- live %>% group_by(indicator) %>% summarise(first_live = min(week_end), .groups = "drop")
hist <- rc %>%
  semi_join(filter(validated, ok), by = c("indicator", "location")) %>%
  inner_join(first_live, by = "indicator") %>%
  filter(week_end < first_live) %>% select(-first_live) %>%
  mutate(source = "RespiCompass 2024/25 (ERVISS extract)") %>%
  arrange(indicator, location, week_end)

OUT <- here::here("data", "respicompass_history.csv")
write_csv(hist, OUT)
step("Written")
hist %>% group_by(indicator) %>%
  summarise(from = min(week_end), to = max(week_end), countries = n_distinct(location), rows = n(), .groups = "drop") %>%
  as.data.frame() %>% print(row.names = FALSE)
say(sprintf("-> %s (%.0f KB)", OUT, file.size(OUT) / 1024))
