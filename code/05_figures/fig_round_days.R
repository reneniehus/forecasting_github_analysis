# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Which weekday is a round dated on, per indicator, over time? ##########
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Two panels:
#   (a) the ENSEMBLE rounds -- the hub's own weekly product. One dot per round,
#       coloured by weekday, so the Monday -> Wednesday switch is visible directly.
#   (b) every submission, to show that Sunday/Saturday dating is a contributing-team
#       habit, never the hub's own schedule.
# Run:  Rscript code/05_figures/fig_round_days.R

source("code/01_support/setup.R")
source("code/01_support/config.R"); params <- settings()
dir.create(params$figure_dir, showWarnings = FALSE, recursive = TRUE)

DAY <- function(d) lubridate::wday(d, label = TRUE, abbr = FALSE, week_start = 1)  # data.table masks wday()

ind_levels <- c("COVID-19 cases", "COVID-19 hospitalisations", "COVID-19 deaths",
                "ILI incidence", "ARI incidence")
day_col <- c(Saturday = "#8b92a2", Sunday = "#b07aa1", Monday = "#2a78d6", Wednesday = "#eb6834")

s <- read_csv(file.path(params$output_dir, "hub_submissions.csv"), show_col_types = FALSE) %>%
  mutate(origin_date = as.Date(origin_date),
         weekday     = factor(DAY(origin_date), levels = names(day_col)),
         indicator   = factor(indicator, levels = ind_levels))

# ---- |-(a) the hub's own product: ensemble rounds ----
ens <- s %>% filter(role == "ensemble") %>% distinct(indicator, origin_date, weekday)
p_ens <- ggplot(ens, aes(origin_date, indicator, colour = weekday)) +
  geom_point(shape = 15, size = 1.6) +
  scale_colour_manual(values = day_col, drop = FALSE, name = NULL) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_discrete(limits = rev(ind_levels)) +
  geom_vline(xintercept = as.Date("2024-10-21"), linetype = "dashed", colour = "#888781") +
  labs(title = "Ensemble rounds are dated Monday, then Wednesday - never any other day",
       subtitle = "One mark per published ensemble round. Dashed line = the Oct-2024 archive -> RespiCast handover.",
       x = NULL, y = NULL) +
  theme_project() + theme(legend.position = "top")

# ---- |-(b) all submissions: the team-level Sunday/Saturday habit ----
allsub <- s %>% distinct(role, origin_date, weekday)
p_all <- ggplot(allsub, aes(origin_date, weekday, colour = weekday)) +
  geom_point(shape = 15, size = 1.4, show.legend = FALSE) +
  scale_colour_manual(values = day_col, drop = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_discrete(limits = rev(names(day_col))) +
  facet_wrap(~ role, ncol = 1, strip.position = "right") +
  labs(title = "Sunday and Saturday dating is a contributing-team habit, not a hub schedule",
       subtitle = "Ensemble and baseline sit only on the hub's round day; individual models also date files the weekend before",
       x = NULL, y = NULL) +
  theme_project()

fig <- p_ens / p_all + patchwork::plot_layout(heights = c(1, 1.35))
ggsave(file.path(params$figure_dir, "round_days.png"), fig, width = 11, height = 7.5, dpi = 120)
cat("figure -> output/figures/round_days.png\n")
