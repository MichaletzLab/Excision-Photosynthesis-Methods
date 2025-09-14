# set thresholds (units: per minute, matching your ΔCO2:SLP and ΔH2O:SLP)
co2_thresh <- 2.0    # µmol mol^-1 min^-1  (relaxed from LICOR default 1.2)
h2o_thresh <- 0.30   # mmol mol^-1 min^-1   (LICOR default 0.30)

full.exin.data <- full.exin.data %>%
  mutate(
    A            = as.numeric(A),
    gsw          = as.numeric(gsw),
    Stable       = as.numeric(Stable),
    `ΔCO2:SLP`   = as.numeric(`ΔCO2:SLP`),
    `ΔH2O:SLP`   = as.numeric(`ΔH2O:SLP`)
  )

full.exin.data <- full.exin.data %>%
  mutate(
    stability_class = case_when(
      !is.na(`ΔCO2:SLP`) & !is.na(`ΔH2O:SLP`) &
        abs(`ΔCO2:SLP`) <= co2_thresh & abs(`ΔH2O:SLP`) <= h2o_thresh ~ "Stable",
      !is.na(`ΔCO2:SLP`) & !is.na(`ΔH2O:SLP`) &
        (abs(`ΔCO2:SLP`) <= 2*co2_thresh & abs(`ΔH2O:SLP`) <= 2*h2o_thresh) ~ "Borderline",
      !is.na(`ΔCO2:SLP`) & !is.na(`ΔH2O:SLP`) ~ "Unstable",
      Stable == 3 ~ "Stable",
      Stable == 2 ~ "Borderline",
      TRUE        ~ "Unstable"
    )
  )

# then re-run your summarise pipeline
stability_summary <- full.exin.data %>%
  group_by(species) %>%
  summarise(
    n_points       = n(),
    n_stable       = sum(stability_class == "Stable", na.rm = TRUE),
    n_borderline   = sum(stability_class == "Borderline", na.rm = TRUE),
    n_unstable     = sum(stability_class == "Unstable", na.rm = TRUE),
    mean_co2_slope = mean(`ΔCO2:SLP`, na.rm = TRUE),
    mean_h2o_slope = mean(`ΔH2O:SLP`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stable_pct     = round(100 * n_stable / n_points, 1),
    borderline_pct = round(100 * n_borderline / n_points, 1),
    unstable_pct   = round(100 * n_unstable / n_points, 1)
  )

print(stability_summary)

summary(stability_summary)
mean(stability_summary$stable_pct)
mean(stability_summary$borderline_pct)
mean(stability_summary$mean_co2_slope)



library(dplyr)

# Vector of species–individual combinations to remove
remove_ids <- tibble::tribble(
  ~species,         ~individ,
  "A. campestre",    3,
  "B. papyrifera",   4,
  "C. betulus",      3,
  "P. menziesii",    4,
  "Q. garryana",     1
)

# Filter out the flagged rows
full.exin.data <- full.exin.data %>%
  anti_join(remove_ids, by = c("species", "individ"))

# Check remaining individuals
full.exin.data %>% 
  distinct(species, individ) %>% 
  arrange(species, individ)

