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

# end at the last round BOTH hubs have completed: the current week's ensemble runs
# Wednesday 23:40 UTC, so including it would show a false drop to zero
XLIM <- c(as.Date("2026-01-01"), as.Date("2026-08-19"))

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

# ---- |-(B) EU/EEA countries actually covered by the published ensemble ----
# Whether an ensemble "exists" is the wrong test: through July the ILI/ARI ensembles
# were still produced, but only for Switzerland, England and Northern Ireland, whose
# data reaches the hub through WHO FluID rather than ERVISS. Counting EU/EEA countries
# shows what member states could actually see -- which is nothing, for seven rounds.
IND <- c("ILI incidence", "ARI incidence", "COVID-19 hospitalisations")
COL <- c("ILI incidence" = "#D55E00", "ARI incidence" = "#009E73",
         "COVID-19 hospitalisations" = "#0072B2")

ec <- read_csv(file.path(params$output_dir, "ensemble_countries_2026.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date),
         indicator = ifelse(target == "hospital admissions", "COVID-19 hospitalisations", target),
         indicator = factor(indicator, levels = IND)) %>%
  filter(date >= XLIM[1])

# weeks with no ensemble at all must read as 0, not as a missing point
gridB <- expand_grid(indicator = factor(IND, levels = IND),
                     date = seq(min(ec$date), XLIM[2], by = 7)) %>%
  left_join(select(ec, indicator, date, eu_countries), by = c("indicator", "date")) %>%
  mutate(eu_countries = ifelse(is.na(eu_countries), 0L, eu_countries))

pB <- ggplot(gridB, aes(date, eu_countries, colour = indicator)) +
  annotate("rect", xmin = as.Date("2026-06-21"), xmax = as.Date("2026-08-09"),
           ymin = -Inf, ymax = Inf, fill = NONE, alpha = 0.10) +
  annotate("text", x = as.Date("2026-07-15"), y = 20, label = "no EU/EEA ensemble\n24 Jun - 5 Aug",
           size = 2.9, colour = NONE, lineheight = 0.95) +
  geom_line(linewidth = 0.6) +
  scale_colour_manual(values = COL, name = NULL) +
  scale_x_date(limits = XLIM, date_breaks = "1 month", date_labels = "%b", expand = c(0.01, 0)) +
  scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 26), expand = c(0, 0)) +
  labs(tag = "B", title = "EU/EEA countries covered by the published RespiCast ensemble",
       subtitle = "Zero for all three indicators across seven consecutive rounds.",
       x = "2026", y = "EU/EEA countries") +
  theme_minimal(base_size = 10.5) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25, colour = RULE),
        axis.text = element_text(colour = MUTED), axis.title = element_text(colour = MUTED, size = 9.5),
        plot.title = element_text(colour = INK, size = 11),
        plot.subtitle = element_text(colour = MUTED, size = 8.6),
        plot.tag = element_text(face = "bold", size = 11.5), plot.tag.position = c(0, 1),
        legend.position = "bottom", legend.key.width = unit(14, "pt"),
        legend.text = element_text(size = 8.5, colour = INK), legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(6, 10, 4, 6))

fig <- pA / pB + patchwork::plot_layout(heights = c(0.85, 2))
ggsave(file.path(params$figure_dir, "break_2026.png"), fig, width = 8.8, height = 5.0, dpi = 200, bg = "white")
cat("figure -> output/figures/break_2026.png\n")
