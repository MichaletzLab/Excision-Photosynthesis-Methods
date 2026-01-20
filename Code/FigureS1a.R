(watpotplot_combined_S <- wat.df %>%
  ggplot(aes(x = time, y = mean_Water.pot, color = Set)) +
  # Add points
  geom_point(aes(group = interaction(species, Set))) +
  # Add error bars
  geom_errorbar(
    aes(
      ymin = mean_Water.pot - SE_wat.pot,
      ymax = mean_Water.pot + SE_wat.pot,
      group = interaction(species, Set)
    ),
    width = 0.2
  ) +
  # Add smooth line with shaded 95% confidence interval
  geom_smooth(
    aes(
      fill = Set,
      group = interaction(species, Set)
    ),
    method = "lm",
    se = TRUE
  ) +
  # Facet by species (shared y-axis)
  facet_wrap(~ species, scales = "fixed") +
  # Set colors for line and fill
  scale_color_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
  scale_fill_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
  # Labels and theme
  xlab("Time of day (hours)") +
  ylab("Water potential (MPa)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  ))
