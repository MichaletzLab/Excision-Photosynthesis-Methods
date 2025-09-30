#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**
# Load Data
RRDat <- read.xlsx("Data/ExtraFiles/logRR_ExcisionData.xlsx")
RRDat <- RRDat %>%
  dplyr::select(-dplyr::starts_with("Column"))

# Step 1: Filter for your 5 species of interest in full.exin.data
species_of_interest <- c("Q. garryana", "P. menziesii", 
                         "C. betulus", "B. papyrifera", "A. campestre")

df <- full.exin.data %>%
  filter(species %in% species_of_interest)

# Step 2: Calculate means and SE for A and gsw by species and Ex.int
summary_stats <- df %>%
  group_by(species, Ex.int) %>%
  summarize(
    mean_A = mean(A, na.rm = TRUE),
    se_A = sd(A, na.rm = TRUE)/sqrt(n()),
    mean_gsw = mean(gsw, na.rm = TRUE),  # use gsw column directly
    se_gsw = sd(gsw, na.rm = TRUE)/sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Step 3: Separate Intact and Excised for comparison
intact <- summary_stats %>% 
  filter(Ex.int == "Intact") %>%
  dplyr::select(species, mean_A_int = mean_A, se_A_int = se_A,
         mean_gsw_int = mean_gsw, se_gsw_int = se_gsw,
         n_int = n)

excised <- summary_stats %>% 
  filter(Ex.int == "Excised") %>%
  dplyr::select(species, mean_A_ex = mean_A, se_A_ex = se_A,
         mean_gsw_ex = mean_gsw, se_gsw_ex = se_gsw,
         n_ex = n)

# Step 4: Merge intact and excised, calculate changes
comparison <- left_join(intact, excised, by = "species") %>%
  mutate(
    A_change = mean_A_ex - mean_A_int,
    A_direction = case_when(
      A_change > 0 ~ "Increase",
      A_change < 0 ~ "Decrease",
      TRUE ~ "No Change"
    ),
    gsw_change = mean_gsw_ex - mean_gsw_int,
    gsw_direction = case_when(
      gsw_change > 0 ~ "Increase",
      gsw_change < 0 ~ "Decrease",
      TRUE ~ "No Change"
    )
  ) %>%
  arrange(species)

# Step 5: Create studyRR for lnRR calculation
studyRR <- comparison %>%
  rename(
    Species = species,
    AmeanInt = mean_A_int,
    AmeanEx = mean_A_ex,
    Aint_SE = se_A_int,
    Aex_SE = se_A_ex,
    gInt = mean_gsw_int,
    gEx = mean_gsw_ex,
    gIntSE = se_gsw_int,
    gExSE = se_gsw_ex
  ) %>%
  mutate(
    # Use average n from intact and excised as n_obs
    n_obs = round((n_int + n_ex)/2),
    Reference = "This Study",
    Xylem = NA,
    Exudates = NA,
    Clade = NA,
    Drought_Strategy = NA
  ) %>%
  dplyr::select(Species, AmeanInt, AmeanEx, Aint_SE, Aex_SE,
         gInt, gEx, gIntSE, gExSE, n_obs, Reference, Xylem, Exudates, Clade, Drought_Strategy)

# Function to calculate lnRR based on Eqn 4 of Nakagawa et al. 2022
calculate_lnRR <- function(df) {
  df %>%
    # make sure everything is numeric
    mutate(across(c(AmeanEx, AmeanInt, Aex_SE, Aint_SE,
                    gEx, gInt, gExSE, gIntSE, n), as.numeric)) %>%
    mutate(
      # Photosynthesis (A) response ratios
      lnRRr = ifelse(is.na(Aex_SE) | is.na(Aint_SE), NA,
                     log(AmeanEx / AmeanInt) +
                       0.5 * ((Aex_SE^2 / n) - (Aint_SE^2 / n))),
      lnRR = log(AmeanEx / AmeanInt),
      SE_lnRR = ifelse(is.na(Aex_SE) | is.na(Aint_SE), NA,
                       sqrt((Aex_SE^2 / (n * AmeanEx^2)) +
                              (Aint_SE^2 / (n * AmeanInt^2)))),
      
      # Stomatal conductance (gsw) response ratios
      glnRRr = ifelse(is.na(gExSE) | is.na(gIntSE), NA,
                      log(gEx / gInt) +
                        0.5 * ((gExSE^2 / n) - (gIntSE^2 / n))),
      glnRR = log(gEx / gInt),
      gSE_lnRR = ifelse(is.na(gExSE) | is.na(gIntSE), NA,
                        sqrt((gExSE^2 / (n * gEx^2)) +
                               (gIntSE^2 / (n * gInt^2))))
    )
}

newRRDat <- calculate_lnRR(RRDat)
studyRR <- studyRR %>%
  rename(n = n_obs)

studyRR <- calculate_lnRR(studyRR)

# Remove existing trait columns if they exist
studyRR <- studyRR %>%
  dplyr::select(-any_of(c("Xylem", "Exudates", "Clade", "Drought_Strategy")))

# Trait info
trait_info <- data.frame(
  Species = c("A. campestre", "B. papyrifera", "C. betulus", 
              "P. menziesii", "Q. garryana"),
  Xylem = c("Diffuse-porous", "Diffuse-porous", "Diffuse-porous", "Nonporous", "Ring-porous"),
  Exudates = c("Latex", "None", "None", "Resin", "Unknown"),
  Clade = c("Angiosperm", "Angiosperm", "Angiosperm", "Gymnosperm", "Angiosperm"),
  Drought_Strategy = c("Anisohydric", "Isohydric", "Anisohydric", "Isohydric", "Anisohydric")
)

# Merge trait info
studyRR <- studyRR %>%
  left_join(trait_info, by = "Species")


# add the significance and direction:
test_results <- full.exin.data %>%
  filter(species %in% c("Q. garryana", "P. menziesii", 
                        "C. betulus", "B. papyrifera", "A. campestre")) %>%
  group_by(species) %>%
  summarise(
    A_test = list(t.test(A ~ Ex.int, data = cur_data())),
    gsw_test = list(t.test(gsw ~ Ex.int, data = cur_data())),
    .groups = "drop"
  ) %>%
  mutate(
    A_test = purrr::map(A_test, broom::tidy),
    gsw_test = purrr::map(gsw_test, broom::tidy)
  ) %>%
  unnest(c(A_test, gsw_test), names_sep = "_") %>%
  mutate(
    mean_intact_A = purrr::map_dbl(species, ~ mean(full.exin.data$A[full.exin.data$species == .x &
                                                                      full.exin.data$Ex.int == "Intact"], na.rm = TRUE)),
    mean_excised_A = purrr::map_dbl(species, ~ mean(full.exin.data$A[full.exin.data$species == .x &
                                                                       full.exin.data$Ex.int == "Excised"], na.rm = TRUE)),
    mean_intact_gsw = purrr::map_dbl(species, ~ mean(full.exin.data$gsw[full.exin.data$species == .x &
                                                                          full.exin.data$Ex.int == "Intact"], na.rm = TRUE)),
    mean_excised_gsw = purrr::map_dbl(species, ~ mean(full.exin.data$gsw[full.exin.data$species == .x &
                                                                           full.exin.data$Ex.int == "Excised"], na.rm = TRUE)),
    Excision.Effect.on.A = case_when(
      mean_excised_A < mean_intact_A ~ "Lower",
      mean_excised_A > mean_intact_A ~ "Higher",
      TRUE ~ "No Effect"
    ),
    Excision.Effect.on.gsw = case_when(
      mean_excised_gsw < mean_intact_gsw ~ "Lower",
      mean_excised_gsw > mean_intact_gsw ~ "Higher",
      TRUE ~ "No Effect"
    ),
    SignificantA = ifelse(A_test_p.value < 0.05, "Yes", "No"),
    Significantg = ifelse(gsw_test_p.value < 0.05, "Yes", "No")
  ) %>%
  dplyr::select(species, Excision.Effect.on.A, SignificantA, Excision.Effect.on.gsw, Significantg)

# Step 3: merge back into your studyRR dataframe
studyRR <- studyRR %>%
  rename(species=Species)%>%
  left_join(test_results, by = "species")







# Columns that should be numeric
num_cols <- c("AmeanInt","AmeanEx","Aint_SE","Aex_SE",
              "gInt","gEx","gIntSE","gExSE","n","lnRRr","lnRR","SE_lnRR","glnRRr","glnRR","gSE_lnRR")

# Convert RRDat columns to numeric
newRRDat  <- newRRDat  %>%
  mutate(across(all_of(num_cols), as.numeric))

# Convert studyRR columns to numeric (already mostly numeric, just in case)
studyRR <- studyRR %>%
  mutate(across(all_of(num_cols), as.numeric))%>%
  rename(Species=species)

# Combine datasets
newRRDat <- bind_rows(newRRDat, studyRR)

newRRDat <- newRRDat %>%
  mutate(Species = factor(Species, levels = rev(sort(unique(Species)))))
  








newRRDat <- newRRDat %>%
  filter(Reference != "Kar et al. 2021") %>%
  filter(Reference != "Meng and Arp 1992")

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

multi_panel_plot <- (main_panel | ( clade_panel / xylem_panel / drought_strategy_panel / exudates_panel)) + 
  plot_layout(widths = c(3, 2), guides = 'collect') +
  plot_annotation(
    # Manually specify labels for all 5 panels
    tag_levels = list(c("C", "D", "E", "F", "G")),
    tag_prefix = "", tag_suffix = "", tag_sep = ". "
  ) &
  theme(
    legend.position = "bottom",
    plot.tag = element_text(face = "bold", size = 16)
  )

# Save the plot
ggsave("multi_panel_plot.png", plot = multi_panel_plot, width = 14, height = 10, dpi = 300)


study_data <- read_excel("Data/ExtraFiles/meta.lnRR.LatLong.xlsx")
study_data <- study_data  %>%
  filter(Reference != "Kar et al. 2021") %>%
  filter(Reference != "Meng and Arp 1992")
world_map <- map_data("world")
color_palette <- c( #removed "Kar et al. 2021" = "#88CCEE", and "Meng and Arp 1992" = "#332288"
  "Lakso 1982" = "#CC6677",
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

figS4 = ggarrange(map_S4, Fig_1b, labels=c("A","B"), ncol=2, common.legend = TRUE,legend = "right")
fig6 <- ggarrange(figS4,multi_panel_plot, nrow=2,heights = c(1, 2))
ggsave("Fig6.png", plot = fig6, width = 14, height = 15, dpi = 300)
