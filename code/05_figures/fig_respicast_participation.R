# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Supporting figure: how many models, and how many countries, feed RespiCast? ##
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Backs the summary statement about RespiCast's breadth. Deliberately plain: no
# legend box (series are labelled where they end), no chart borders, one dashed
# reference line, winter shaded so the seasonal cadence needs no explaining.
#
# RespiCast here = every hub EXCEPT the predecessor EU COVID-19 Forecast Hub. The
# 2023/24 flu and ARI hubs are included because they already published under the
# respicast-hubEnsemble name; the EU COVID-19 hub is a separate, earlier programme.
#
# Run:  Rscript code/05_figures/fig_respicast_participation.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

IND <- c("ILI incidence", "ARI incidence", "COVID-19 hospitalisations")
COL <- c("ILI incidence" = "#c2571f", "ARI incidence" = "#4a3aa7", "COVID-19 hospitalisations" = "#1f6bb8")
INK <- "#2b2b28"; MUTED <- "#6f6e69"; RULE <- "#d8d7d1"

theme_sci <- function(base = 10.5) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor  = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linewidth = 0.25, colour = RULE),
          axis.title        = element_text(colour = MUTED, size = base - 0.5),
          axis.text         = element_text(colour = MUTED),
          plot.title        = element_text(face = "plain", colour = INK, size = base + 0.5),
          plot.tag          = element_text(face = "bold", colour = INK, size = base + 1),
          plot.tag.position = c(0, 1),
          legend.position   = "none",
          plot.margin       = margin(6, 10, 6, 6))
}

# ---- |-data: RespiCast era only ----
w <- read_csv(file.path(params$output_dir, "hub_coverage_weekly.csv"), show_col_types = FALSE) %>%
  filter(!grepl("covid_archive", hub)) %>%
  mutate(week = as.Date(week), indicator = factor(indicator, levels = IND))

sub <- read_csv(file.path(params$output_dir, "hub_submissions.csv"), show_col_types = FALSE) %>%
  filter(hub != "covid_archive")

# winter bands (Oct-Mar), for shading
yrs   <- sort(unique(lubridate::year(w$week)))
# clamp to the plotted range, else a band starting before the first round is dropped
bands <- tibble(xmin = as.Date(paste0(yrs, "-10-01")),
                xmax = as.Date(paste0(yrs + 1, "-03-31"))) %>%
  filter(xmin <= max(w$week), xmax >= min(w$week)) %>%
  mutate(xmin = pmax(xmin, min(w$week)), xmax = pmin(xmax, max(w$week)))

# ---- |-(A) contributing models per week ----
# Insert NA on weeks a hub did not run, so the line BREAKS over the summer-2024 gap
# between the 2023/24 flu+ARI hubs and the current hubs, instead of interpolating it.
grid <- expand_grid(indicator = factor(IND, levels = IND),
                    week = seq(min(w$week), max(w$week), by = 7)) %>%
  left_join(select(w, indicator, week, n_models), by = c("indicator", "week"))

pA <- ggplot(grid, aes(week, n_models, colour = indicator)) +
  geom_rect(data = bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "#f2f1ec", alpha = 0.9) +
  geom_hline(yintercept = 5, linetype = "22", linewidth = 0.4, colour = MUTED) +
  # sits in the empty summer-2024 gap between the two hub generations, clear of every line
  annotate("text", x = as.Date("2024-06-25"), y = 6.0, label = "5 models", hjust = 0,
           size = 2.8, colour = MUTED) +
  geom_line(linewidth = 0.55, na.rm = TRUE) +
  scale_colour_manual(values = COL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(min(w$week), max(w$week)), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 21), expand = c(0, 0)) +
  labs(tag = "A", title = "Contributing models per weekly round", x = NULL, y = "models") +
  theme_sci() +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.key.width = unit(14, "pt"), legend.margin = margin(0, 0, 2, 0),
        legend.text = element_text(colour = INK, size = 9),
        plot.margin = margin(6, 10, 6, 6)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1.1)))

# ---- |-(B) distinct models and countries reached, per indicator ----
tot <- sub %>% group_by(indicator) %>%
  summarise(models = n_distinct(model[role == "model"]), .groups = "drop") %>%
  left_join(sub %>% filter(role %in% c("model", "ensemble")) %>%
              separate_rows(locations, sep = ",") %>% filter(locations != "") %>%
              group_by(indicator) %>% summarise(countries = n_distinct(locations), .groups = "drop"),
            by = "indicator") %>%
  mutate(indicator = factor(indicator, levels = rev(IND)))

pB <- ggplot(tot, aes(y = indicator)) +
  geom_segment(aes(x = countries, xend = models, yend = indicator), colour = RULE, linewidth = 1.1) +
  geom_point(aes(x = countries), shape = 21, size = 3, stroke = 0.8, fill = "white", colour = MUTED) +
  geom_point(aes(x = models, colour = indicator), size = 3) +
  geom_text(aes(x = models, label = models, colour = indicator), vjust = -1.25, size = 3) +
  geom_text(aes(x = countries, label = countries), vjust = -1.25, size = 3, colour = MUTED) +
  scale_colour_manual(values = COL) +
  scale_x_continuous(limits = c(0, 36), breaks = seq(0, 35, 5), expand = c(0, 0)) +
  labs(tag = "B", title = "Distinct models (filled) and countries reached (open), cumulative",
       x = "count", y = NULL) +
  theme_sci() + theme(panel.grid.major.y = element_blank(),
                      panel.grid.major.x = element_line(linewidth = 0.25, colour = RULE))

fig <- pA / pB + patchwork::plot_layout(heights = c(2.1, 1))
ggsave(file.path(params$figure_dir, "respicast_participation.png"), fig,
       width = 8.6, height = 5.4, dpi = 200, bg = "white")
cat("figure -> output/figures/respicast_participation.png\n")
