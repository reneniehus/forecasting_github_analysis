# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### The 2026 TESSy -> EpiPulse break, cause above effect ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# (A) the ERVISS truth-data feed, one mark per weekly publication -- the 11-week hole
# (B) the RespiCast rounds in each hub, coloured by what was published that week
# on one shared x axis, so the data outage and the forecast outage line up.
#
# Run (after code/03_hubs/extract_2026_break.R):  Rscript code/05_figures/fig_break_2026.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()

OK   <- "#0072B2"   # published / ensemble out
THIN <- "#9ecae9"   # round ran, but on ECDC's own models only
NONE <- "#D55E00"   # nothing published
INK  <- "#2b2b28"; MUTED <- "#6f6e69"; RULE <- "#d8d7d1"

d <- read_csv(file.path(params$output_dir, "break_2026.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>% filter(date >= as.Date("2026-01-01"))

XLIM <- c(as.Date("2026-01-01"), max(d$date))

# ---- |-(A) ERVISS weekly publications ----
erv <- d %>% filter(source == "ERVISS data feed") %>%
  complete(date = seq(min(date), XLIM[2], by = 7)) %>%
  mutate(published = date %in% d$date[d$source == "ERVISS data feed"],
         lab = ifelse(published, "Data published", "No data (TESSy -> EpiPulse transition)"))

pA <- ggplot(erv, aes(date, y = 1, fill = lab)) +
  geom_tile(width = 6.4, height = 1) +
  scale_fill_manual(values = c("Data published" = OK,
                               "No data (TESSy -> EpiPulse transition)" = NONE), name = NULL) +
  scale_x_date(limits = XLIM, date_breaks = "1 month", date_labels = "%b", expand = c(0.01, 0)) +
  labs(tag = "A", title = "ERVISS truth data (EU-ECDC/Respiratory_viruses_weekly_data)",
       subtitle = "Weekly publication commits. Last update 22 May 2026; next 7 Aug 2026 - 77 days (11 weeks).",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid = element_blank(),
        plot.title = element_text(colour = INK, size = 11), plot.subtitle = element_text(colour = MUTED, size = 8.6),
        plot.tag = element_text(face = "bold", size = 11.5), plot.tag.position = c(0, 1),
        legend.position = "top", legend.key.size = unit(9, "pt"),
        legend.text = element_text(size = 8.5, colour = INK), legend.margin = margin(0, 0, 2, 0),
        plot.margin = margin(4, 10, 2, 6))

# ---- |-(B) RespiCast rounds ----
rc <- d %>% filter(source != "ERVISS data feed") %>%
  group_by(source) %>%
  complete(date = seq(min(date), XLIM[2], by = 7)) %>% ungroup() %>%
  mutate(models   = ifelse(is.na(models), 0L, models),
         ensemble = ifelse(is.na(ensemble), FALSE, ensemble),
         # the question is what reached users, so classify by the ENSEMBLE, noting
         # the weeks it rested on ECDC's own two models alone
         status = case_when(!ensemble            ~ "No ensemble published",
                            models <= 2          ~ "Ensemble, ECDC in-house models only",
                            TRUE                 ~ "Ensemble, full multi-model round"),
         status = factor(status, levels = c("Ensemble, full multi-model round",
                                            "Ensemble, ECDC in-house models only",
                                            "No ensemble published")),
         source = factor(source, levels = c("RespiCast-SyndromicIndicators", "RespiCast-Covid19")))

pB <- ggplot(rc, aes(date, source, fill = status)) +
  geom_tile(width = 6.4, height = 0.62) +
  scale_fill_manual(values = c("Ensemble, full multi-model round" = OK,
                               "Ensemble, ECDC in-house models only" = THIN,
                               "No ensemble published" = NONE), name = NULL) +
  scale_x_date(limits = XLIM, date_breaks = "1 month", date_labels = "%b", expand = c(0.01, 0)) +
  labs(tag = "B", title = "RespiCast ensemble, by weekly round",
       subtitle = "COVID-19: no ensemble for 7 consecutive rounds, 24 Jun - 5 Aug. ILI/ARI: only 3 rounds missed.",
       x = "2026", y = NULL) +
  theme_minimal(base_size = 10.5) +
  theme(panel.grid = element_blank(), axis.text = element_text(colour = MUTED),
        axis.title.x = element_text(colour = MUTED, size = 9.5),
        plot.title = element_text(colour = INK, size = 11), plot.subtitle = element_text(colour = MUTED, size = 8.6),
        plot.tag = element_text(face = "bold", size = 11.5), plot.tag.position = c(0, 1),
        legend.position = "bottom", legend.key.size = unit(9, "pt"),
        legend.text = element_text(size = 8.5, colour = INK), legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(6, 10, 4, 6))

fig <- pA / pB + patchwork::plot_layout(heights = c(1, 1.55))
ggsave(file.path(params$figure_dir, "break_2026.png"), fig, width = 8.8, height = 3.9, dpi = 200, bg = "white")
cat("figure -> output/figures/break_2026.png\n")
