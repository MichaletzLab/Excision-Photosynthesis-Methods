#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**
# Load Data
RRDat <- read.xlsx("Data/ExtraFiles/logRR_ExcisionData.xlsx")

# Function to calculate lnRR based on Eqn 4 of Nakagawa et al. 2022
calculate_lnRR <- function(df) {
  df <- df %>%
    mutate(across(c(AmeanEx, AmeanInt, Aex_SE, Aint_SE, gEx, gInt, gExSE, gIntSE, n), as.numeric)) %>%
    mutate(
      lnRRr = ifelse(is.na(Aex_SE), NA, log(AmeanEx / AmeanInt) + 0.5 * ((Aex_SE^2 / n) - (Aint_SE^2 / n))),
      lnRR = log(AmeanEx / AmeanInt),
      SE_lnRR = ifelse(is.na(Aex_SE), NA, sqrt((Aex_SE / (n * AmeanEx^2)) + (Aint_SE / (n * AmeanInt^2)))),
      glnRRr = ifelse(is.na(gExSE) | is.na(gEx), NA, log(gEx / gInt) + 0.5 * ((gExSE^2 / n) - (gIntSE^2 / n))),
      glnRR = ifelse(is.na(gEx), NA, log(gEx / gInt)),
      gSE_lnRR = ifelse(is.na(gExSE) | is.na(gEx), NA, sqrt((gExSE / (n * gEx^2)) + (gIntSE / (n * gInt^2))))
    )
  return(df)
}

newRRDat <- calculate_lnRR(RRDat)

newRRDat <- newRRDat %>%
  mutate(Species = factor(Species, levels = rev(sort(unique(Species)))))

# Function to create main species panel
create_mainpanel <- function(data, column) {
  data %>%
    group_by(!!sym(column)) %>%
    summarise(
      mean_lnRR = lnRR, na.rm = TRUE,
      mean_glnRR = glnRR, na.rm = TRUE,
      SE_lnRR = SE_lnRR,
      SE_glnRR = gSE_lnRR
    ) %>%
    pivot_longer(cols = c(mean_lnRR, mean_glnRR), names_to = "Response", values_to = "Value") %>%
    ggplot(aes(x = Value, y = !!sym(column), color = Response, shape = Response)) +
    geom_point(size = 3, position = position_dodge(0.9)) +
    geom_errorbarh(aes(xmin = Value - 1.96 * SE_lnRR, xmax = Value + 1.96 * SE_lnRR), position = position_dodge(0.9), height = 0.25) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    scale_color_manual(
      values = c("mean_lnRR" = "darkred", "mean_glnRR" = "lightblue"),
      labels = c("mean_lnRR" = "Photosynthesis", "mean_glnRR" = "Stomatal conductance"),
      name = "Response"
    ) +
    scale_shape_manual(
      values = c("mean_lnRR" = 16, "mean_glnRR" = 17),  # Different shapes for lnRR and glnRR
      labels = c("mean_lnRR" = "Photosynthesis", "mean_glnRR" = "Stomatal conductance"),
      name = "Response"
    ) +
    labs(x = "Log Response Ratio", y = " ") +
    theme_classic() +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      axis.text.y = element_text(face = "italic", size = 16),
      axis.text.x = element_text(size = 16),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18)
    )
}

# Function to create subpanels with independent filtering
create_subpanel <- function(data, column, show_x_title = TRUE) {
  filtered_data <- data %>%
    filter(!is.na(!!sym(column)) & !!sym(column) != "Unknown" & !!sym(column) != "Unclassified")
  plot <- filtered_data %>%
    group_by(!!sym(column)) %>%
    summarise(
      mean_lnRR = mean(lnRR, na.rm = TRUE),
      mean_glnRR = mean(glnRR, na.rm = TRUE),
      SE_lnRR = sd(lnRR, na.rm = TRUE) / sqrt(n()),
      SE_glnRR = sd(glnRR, na.rm = TRUE) / sqrt(n())
    ) %>%
    pivot_longer(cols = c(mean_lnRR, mean_glnRR), names_to = "Response", values_to = "Value") %>%
    ggplot(aes(x = Value, y = !!sym(column), color = Response, shape = Response)) +
    geom_point(size = 3, position = position_dodge(0.9)) +
    geom_errorbarh(aes(xmin = Value - 1.96 * SE_lnRR, xmax = Value + 1.96 * SE_lnRR), 
                   position = position_dodge(0.9), height = 0.25) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    scale_color_manual(
      values = c("mean_lnRR" = "darkred", "mean_glnRR" = "lightblue"),
      labels = c("mean_lnRR" = "Photosynthesis", "mean_glnRR" = "Stomatal conductance"),
      name = "Response"
    ) +
    scale_shape_manual(
      values = c("mean_lnRR" = 16, "mean_glnRR" = 17),  # Different shapes for lnRR and glnRR
      labels = c("mean_lnRR" = "Photosynthesis", "mean_glnRR" = "Stomatal conductance"),
      name = "Response"
    ) +
    labs(x = ifelse(show_x_title, "Log Response Ratio", ""), y=" ") +
    theme_classic() +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      axis.text.y = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.title = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18)
    ) +
    scale_x_continuous(limits = c(-3, 2.5))
}

# Create panels
main_panel <- create_mainpanel(newRRDat, "Species")
xylem_panel <- create_subpanel(newRRDat, "Xylem", show_x_title = FALSE)
exudates_panel <- create_subpanel(newRRDat, "Exudates", show_x_title = TRUE)
clade_panel <- create_subpanel(newRRDat, "Clade", show_x_title = FALSE)
drought_strategy_panel <- create_subpanel(newRRDat, "Drought_Strategy", show_x_title = FALSE)

# Combine all plots into one multi-panel plot using patchwork
multi_panel_plot <- (main_panel | ( clade_panel / xylem_panel / drought_strategy_panel / exudates_panel)) + 
  plot_layout(widths = c(3, 2), guides = 'collect') + 
  plot_annotation(tag_levels = 'A', tag_prefix = "", tag_suffix = "", tag_sep = ". ") & 
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold", size = 16))

# Save the plot
ggsave("multi_panel_plot.png", plot = multi_panel_plot, width = 14, height = 10, dpi = 300)


study_data <- read_excel("Data/ExtraFiles/meta.lnRR.LatLong.xlsx")
world_map <- map_data("world")
color_palette <- c(
  "Kar et al. 2021" = "#88CCEE", "Lakso 1982" = "#CC6677", "Meng and Arp 1992" = "#332288",
  "Missik et al. 2021" = "#117733", "Miyazawa et al. 2011" = "#882255", "Santiago and Mulkey 2003" = "#D55E00",
  "This Study" = "#AA4499", "Verryckt et al. 2020" = "#44AA99"
)
(map_S4 <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = study_data, aes(x = longitude, y = latitude, size = species, color = Reference), alpha = 0.7) +
    scale_color_manual(values = color_palette) +  # Use scale_color_manual for the color aesthetic
    scale_size_continuous(range = c(3, 10)) +  # Adjust the range for dot sizes
    theme_classic() +
    labs(x = "Longitude",
         y = "Latitude",
         size = "Species count",
         color = "Reference") +
    theme(axis.title.x = element_text(size = 17),  
          axis.title.y = element_text(size = 17),  
          axis.text.x = element_text(size = 16),  
          axis.text.y = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right",panel.border = element_rect(color = "black", fill = NA, size = 1)))

bioclim_data <- geodata::worldclim_global(var = 'bio', res = 10, path = tempdir())

locations <- study_data %>%
  dplyr::select(latitude, longitude)

locations_vect <- vect(locations, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
bioclim_values <- extract(bioclim_data, locations_vect)
study_data <- cbind(study_data, bioclim_values)

study_data <- study_data[, !duplicated(names(study_data))]
study_data <- study_data %>%
  rename(temperature = wc2.1_10m_bio_1, precipitation = wc2.1_10m_bio_12)

# Load the Whittaker biome data
Whittaker <- read.csv("Data/ExtraFiles/WhittakerClimateSpace_path.csv", header = TRUE)

# Define coordinates for plotting biome labels (numbers 1-9)
biomes <- data.frame(
  x = c(25.8, 18.7, 28.1, 18.5, 2.2, 28.4, 18, -8.7, 28.4),
  y = c(4150, 2800, 1425, 2100, 1500, 675, 500, 250, 200),
  label = c(1:9)
)


# Create the plot
(Fig_1b <- ggplot(study_data, aes(x = temperature, y = precipitation)) + 
    geom_path(data = Whittaker, aes(x = MAT_whittaker, y = MAP_whittaker)) +
    geom_point(aes(fill = Reference, size = species), shape = 21, alpha = 0.75) +
    scale_fill_manual(values = color_palette) +
    scale_size_continuous(range = c(3, 10)) +  # Adjust the range for dot sizes
    xlab(expression('Mean annual temperature' ~ (degree * C))) + 
    ylab("Mean annual precip. (mm)") + 
    guides(
      fill = guide_legend(override.aes = list(size = 4)),
      size = guide_legend(title = "Species Count")
    ) +
    geom_text(data = biomes, aes(label = label, x = x, y = y), size = 4) + 
    theme_bw(base_size = 13) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.title.x = element_text(size = 17),  
          axis.title.y = element_text(size = 17),  
          axis.text.x = element_text(size = 16),  
          axis.text.y = element_text(size = 16),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.position = "right",
          panel.border = element_rect(color = "black", fill = NA, size = 1)))

figS4 = ggarrange(map_S4, Fig_1b, labels=c("F","G"), ncol=2, common.legend = TRUE,legend = "right")

(figS4 = ggarrange(map_S4, Fig_1b, labels=c("F","G"), nrow=1, common.legend = TRUE,legend = "left"))
fig6 <- ggarrange(multi_panel_plot,figS4, nrow=2,heights = c(2, 1))
ggsave("Fig6.png", plot = fig6, width = 14, height = 15, dpi = 300)