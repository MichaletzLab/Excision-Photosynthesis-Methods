#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**

library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)

# --- A ---
qg.dat <- full.exin.data %>%
  filter(species == "Q. garryana") %>%
  group_by(Ex.int, time) %>%
  summarise(
    mean = mean(as.numeric(A), na.rm = TRUE),
    sd   = sd(as.numeric(A), na.rm = TRUE),
    n    = sum(!is.na(A)),
    se   = sd / sqrt(n),
    ci_low = mean - qt(0.975, df = n - 1) * se,
    ci_high= mean + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

qg.plot1 <- ggplot(qg.dat, aes(x = time, y = mean, color = Ex.int, group = Ex.int)) +
  geom_point() +
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    aes(color = Ex.int, fill = Ex.int)
  ) +
  theme_bw() +
  scale_color_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
  scale_fill_manual(values = c("dodgerblue1", "deeppink4"), name = "") + 
  xlab("Time of day (hours)") +
  ylab(expression('Net photosynthetic rate (μmol m'^-2*' s'^-1*')')) +
  labs(title = "Quercus garryana") +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    plot.title = element_text(face = "italic"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  )

# --- Water potential ---
wqg.dat <- full.exin.data %>%
  filter(species == "Q. garryana") %>%
  group_by(Ex.int, time) %>%
  summarise(
    mean = mean(as.numeric(calc.wp), na.rm = TRUE),
    sd   = sd(as.numeric(calc.wp), na.rm = TRUE),
    n    = sum(!is.na(calc.wp)),
    se   = sd / sqrt(n),
    ci_low = mean - qt(0.975, df = n - 1) * se,
    ci_high= mean + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

wqg.plot1 <- ggplot(wqg.dat, aes(x = time, y = mean, color = Ex.int, group = Ex.int)) +
  geom_point() +
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    aes(color = Ex.int, fill = Ex.int)
  ) +
  theme_bw() +
  scale_color_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
  scale_fill_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
  xlab("Time of day (hours)") + ylab("Water potential (MPa)") +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  )

# --- gsw ---
gqg.dat <- full.exin.data %>%
  filter(species == "Q. garryana") %>%
  group_by(Ex.int, time) %>%
  summarise(
    mean = mean(as.numeric(gsw), na.rm = TRUE),
    sd   = sd(as.numeric(gsw), na.rm = TRUE),
    n    = sum(!is.na(gsw)),
    se   = sd / sqrt(n),
    ci_low = mean - qt(0.975, df = n - 1) * se,
    ci_high= mean + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

gqg.plot1 <- ggplot(gqg.dat, aes(x = time, y = mean, color = Ex.int, group = Ex.int)) +
  geom_point() +
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    aes(color = Ex.int, fill = Ex.int)
  ) +
  theme_bw() +
  scale_color_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
  scale_fill_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
  xlab("Time of day (hours)") +
  ylab(expression('Stomatal conductance (mol m'^-2*' s'^-1*')')) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  )

# --- Combine all three ---
qg.time <- ggarrange(
  qg.plot1, wqg.plot1, gqg.plot1,
  ncol = 3, nrow = 1,
  labels = c("H", "I", "J"),
  common.legend = TRUE,
  label.x = 0.07, label.y = 1.05
)

qg.time
