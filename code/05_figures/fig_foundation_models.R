# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Foundation-model submitters: when they submitted, and how they rank ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Six models built on pretrained time-series foundation models (five on Chronos-2, one on
# TabPFN-TS -- the latter assumed, its metadata only says "augmented transformer").
#
# (A) Tile plot: one tile per weekly round each model submitted, coloured by the hub.
#     Behind it, faint lines give the SHAPE of EU/EEA ILI, ARI and COVID-19 admissions
#     over the same weeks: each country's series divided by its own maximum in the window,
#     then averaged across EU/EEA countries, then rescaled to 0-1. Shape only, no level.
# (B) Mean rank score per model, from the hubs' own WIS scores (model-evaluation/), with
#     the hub ensemble for reference. Rank score is the hub's: 1 = best of the models
#     scored on that target, 0 = worst.
#
# Run:  Rscript code/05_figures/fig_foundation_models.R
#       (needs clones in /workspace/emh-syndromic and /workspace/emh-covid, or set
#        SYNDROMIC_DIR / COVID_DIR)

suppressMessages({library(dplyr); library(tidyr); library(readr); library(ggplot2); library(patchwork)})

SYN <- Sys.getenv("SYNDROMIC_DIR", "/workspace/emh-syndromic")
COV <- Sys.getenv("COVID_DIR",     "/workspace/emh-covid")
OUT <- "output/figures/foundation_models.png"

MODELS <- tribble(
  ~model,                ~hub,        ~kind,
  "Chronos-Chronos2",    "syndromic", "Chronos-2, zero-shot",
  "fluxengine-chronos2", "syndromic", "Chronos-2, zero-shot (Germany ARI)",
  "pedro-Chronos2",      "syndromic", "Chronos-2, zero-shot",
  "DHauser-FluChronos",  "syndromic", "Chronos-2, fine-tuned",
  "fluxengine-triad",    "syndromic", "Chronos-2 + boosting (Germany ARI)",
  "QMUL-TabPFNTS",       "covid",     "TabPFN-TS (assumed foundation model)")
EU <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IT",
        "LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","IS","LI","NO")
INK <- "#1f1f1c"; MUTED <- "#6f6e69"
HUBCOL <- c(syndromic = "#0f7f86", covid = "#c2552b")
SHAPE  <- c("ILI" = "#d38a3c", "ARI" = "#5b8fc9", "COVID-19 hosp." = "#9b6bb0")

# ---- |-submissions ----
subs <- MODELS %>% rowwise() %>%
  mutate(files = list(list.files(file.path(if (hub == "covid") COV else SYN, "model-output", model),
                                 pattern = "^\\d{4}-\\d{2}-\\d{2}"))) %>%
  unnest(files) %>% ungroup() %>%
  mutate(round = as.Date(substr(files, 1, 10)),
         label = sprintf("%s\n%s", model, kind))
X <- c(min(subs$round) - 14, max(subs$round) + 7)
ylev <- subs %>% distinct(model, label, first = min(round)) %>% group_by(model, label) %>%
  summarise(first = min(first), .groups = "drop") %>% arrange(desc(first)) %>% pull(label)
subs$label <- factor(subs$label, levels = ylev)
NR <- length(ylev)

# ---- |-background shapes ----
rd <- function(f) read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  transmute(location, week = as.Date(truth_date), value = as.numeric(value)) %>%
  filter(location %in% EU, !is.na(value), week >= X[1], week <= X[2])
shape <- bind_rows(
  rd(file.path(SYN, "target-data/ERVISS/latest-ILI_incidence.csv")) %>% mutate(ind = "ILI"),
  rd(file.path(SYN, "target-data/ERVISS/latest-ARI_incidence.csv")) %>% mutate(ind = "ARI"),
  rd(file.path(COV, "target-data/latest-hospital_admissions.csv"))  %>% mutate(ind = "COVID-19 hosp.")) %>%
  group_by(ind, location) %>% filter(max(value) > 0, n() >= 20) %>%
  mutate(v = value / max(value)) %>%
  group_by(ind, week) %>% summarise(v = mean(v), n = n(), .groups = "drop") %>%
  group_by(ind) %>% mutate(v = (v - min(v)) / (max(v) - min(v))) %>% ungroup() %>%
  mutate(y = 0.5 + v * NR)                     # stretch the shape over the tile rows

# ---- |-(A) tiles ----
pA <- ggplot() +
  geom_line(data = shape, aes(week, y, colour = ind), linewidth = 0.9, alpha = 0.35) +
  geom_tile(data = subs, aes(round, as.integer(label), fill = hub),
            width = 5.5, height = 0.62, colour = NA) +
  scale_colour_manual(values = SHAPE, name = "EU/EEA shape (normalised)") +
  scale_fill_manual(values = HUBCOL, name = "Hub",
                    labels = c(syndromic = "RespiCast-SyndromicIndicators", covid = "RespiCast-Covid19")) +
  scale_y_continuous(breaks = seq_len(NR), labels = ylev, expand = expansion(add = 0.3)) +
  scale_x_date(limits = X, date_breaks = "1 month", date_labels = "%b\n%Y", expand = c(0, 0)) +
  labs(x = NULL, y = NULL, tag = "A",
       title = "Weekly rounds submitted by foundation-model forecasters") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = INK, size = 8.2, hjust = 1, lineheight = 0.95),
        axis.text.x = element_text(colour = MUTED), plot.title = element_text(size = 11),
        legend.position = "bottom", legend.box = "vertical", legend.text = element_text(size = 8),
        legend.title = element_text(size = 8.5, colour = MUTED))

# ---- |-(B) mean rank score ----
scores <- bind_rows(read_csv(file.path(SYN, "model-evaluation/latest-forecast_scores.csv"), show_col_types = FALSE),
                    read_csv(file.path(COV, "model-evaluation/latest-forecast_scores.csv"), show_col_types = FALSE)) %>%
  filter(metric == "WIS") %>% mutate(model = paste(team_id, model_id, sep = "-"))
rk <- scores %>% filter(model %in% MODELS$model) %>%
  group_by(model) %>% summarise(rank_score = mean(rank_score), n = n(), rounds = n_distinct(origin_date), .groups = "drop")
# the ensemble on exactly the same targets as each model, so the comparison is like for like
ens <- scores %>% filter(model == "respicast-hubEnsemble") %>%
  select(origin_date, target, location, horizon, ens = rank_score)
rk <- rk %>% left_join(
  scores %>% filter(model %in% MODELS$model) %>%
    inner_join(ens, by = c("origin_date", "target", "location", "horizon")) %>%
    group_by(model) %>% summarise(ens = mean(ens), .groups = "drop"), by = "model") %>%
  left_join(MODELS, by = "model") %>%
  mutate(label = factor(sprintf("%s\n%s", model, kind), levels = ylev))
cat("rank scores:\n"); print(as.data.frame(select(rk, model, rank_score, ens, n, rounds)), row.names = FALSE)

pB <- ggplot(rk, aes(y = label)) +
  geom_segment(aes(x = ens, xend = rank_score, yend = label), colour = "#c9c8c2", linewidth = 0.8) +
  geom_point(aes(x = ens), shape = 21, size = 2.6, fill = "white", colour = MUTED, stroke = 0.8) +
  geom_point(aes(x = rank_score, colour = hub), size = 3) +
  geom_text(aes(x = 1.02, label = sprintf("%.2f  (%d forecasts, %d rounds)", rank_score, n, rounds)),
            hjust = 0, size = 2.7, colour = MUTED) +
  scale_colour_manual(values = HUBCOL, guide = "none") +
  scale_x_continuous(limits = c(0, 1.45), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  labs(x = "Mean rank score (1 = best model on a target, 0 = worst)", y = NULL, tag = "B",
       title = "Mean rank score; open circle = hub ensemble on the same targets") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(colour = INK, size = 8.2, hjust = 1, lineheight = 0.95),
        plot.title = element_text(size = 11))

fig <- pA / pB + plot_layout(heights = c(1.25, 1)) +
  plot_annotation(caption = paste0(
    "A: tiles = rounds with a submission file. Background lines: each EU/EEA country's series divided by its own maximum over these weeks, averaged ",
    "across countries,\nrescaled 0-1 -- shape only. B: rank score from the hubs' WIS scores (model-evaluation/latest-forecast_scores.csv), ",
    "scored rounds only. TabPFN-TS is assumed\nto be a foundation model. Built by code/05_figures/fig_foundation_models.R."),
    theme = theme(plot.caption = element_text(hjust = 0, colour = MUTED, size = 7.5)))
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
ggsave(OUT, fig, width = 10, height = 9, dpi = 200, bg = "white")
cat("figure ->", OUT, "\n")
