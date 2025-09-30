#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**
# Load packages
library(lme4)
library(emmeans)
library(ggplot2)
library(dplyr)
library(ggpubr)

# Ensure numeric columns
full.exin.data$A <- as.numeric(full.exin.data$A)
full.exin.data$Measure.height <- as.numeric(full.exin.data$Measure.height)
full.exin.data$Air.temp <- as.numeric(full.exin.data$Air.temp)
full.exin.data$Water.pot <- as.numeric(full.exin.data$Water.pot)
full.exin.data$gsw <- as.numeric(full.exin.data$gsw)
full.exin.data$calc.wp <- as.numeric(full.exin.data$calc.wp)

#----------------#
# Fit models
#----------------#
mixed2.modA <- lmer(A ~ (1|individual/branch) + Water.pot + Measure.height + Air.temp + Ex.int*species,
                    data = full.exin.data)

mixed.modg <- lmer(gsw ~ (1|individual/branch) + Water.pot + Air.temp + Measure.height + Ex.int*species,
                   data = full.exin.data)

mixed.modwatpot <- lmer(calc.wp ~ (1|individual/branch) + gsw + Air.temp + Measure.height + Ex.int*species,
                        data = full.exin.data)

#----------------#
# Helper function to extract contrast results
#----------------#
extract_contrasts <- function(model, response_label) {
  em <- emmeans(model, pairwise ~ Ex.int | species)
  cont <- as.data.frame(summary(em$contrasts))
  
  # Keep only the contrast Excised - Intact (adjust if needed)
  cont <- cont %>%
    filter(grepl("Excised", contrast) & grepl("Intact", contrast)) %>%
    mutate(significant = p.value < 0.05)
  
  # Build final contrast data frame
  contrast_data <- data.frame(
    species = cont$species,
    estimate = cont$estimate,
    SE = cont$SE,
    significant = cont$significant,
    response = response_label
  )
  
  return(contrast_data)
}

#----------------#
# Extract results
#----------------#
contrast_data.A <- extract_contrasts(mixed2.modA, "Photosynthesis")
contrast_data.g <- extract_contrasts(mixed.modg, "Stomatal conductance")
contrast_data.watpot <- extract_contrasts(mixed.modwatpot, "Water potential")

#----------------#
# Plot function (with option to hide y-axis labels)
#----------------#
plot_contrasts <- function(contrast_data, ylab_text, show_y_labels = TRUE) {
  crit_val <- 1.96
  
  base_plot <- ggplot(contrast_data, aes(x = species, y = estimate)) +
    geom_point(position = position_dodge(width = 0.75), size = 3) +
    geom_errorbar(aes(ymin = estimate - crit_val * SE,
                      ymax = estimate + crit_val * SE),
                  width = 0,
                  position = position_dodge(width = 0.75)) +
    geom_text(aes(label = ifelse(significant, "*", "")),
              size = 6, vjust = 0, hjust = 0) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 15),
      legend.title = element_blank(),
      legend.text = element_text(face = "italic"),
      axis.text.y = if (show_y_labels) element_text(face = "italic") else element_blank()
    ) +
    ylab(ylab_text) +
    xlab("")
  
  return(base_plot)
}

#----------------#
# Create plots
#----------------#
species.A.plot <- plot_contrasts(contrast_data.A, "Effect size for photosynthesis", show_y_labels = FALSE)
species.g.plot <- plot_contrasts(contrast_data.g, expression('Effect size for stomatal conductance'), show_y_labels = FALSE)
species.watpot.plot <- plot_contrasts(contrast_data.watpot, "Effect size for water potential", show_y_labels = TRUE)

#----------------#
# Arrange plots
#----------------#
species.arranged <- ggarrange(
  species.watpot.plot,
  species.g.plot,
  species.A.plot,
  nrow = 1, ncol = 3,
  labels = c("A", "B", "C"),
  common.legend = TRUE,
  widths = c(1.75, 1, 1)
)

species.arranged
