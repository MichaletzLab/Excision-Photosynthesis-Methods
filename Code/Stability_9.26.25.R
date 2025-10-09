#Instead filter based on slightly modified so that it makes the auto criteria more constant
#Sets all delta CO2 slope limits to 1 instead of some at 1 and some at 0.5.

# Step 1: create new CO2_OK column based on slope < 1
full.exin.data$`ΔCO2:SLP` <- as.numeric(full.exin.data$`ΔCO2:SLP`)
full.exin.data <- full.exin.data %>%
  mutate(CO2_OK = ifelse(`ΔCO2:SLP` > -1 & `ΔCO2:SLP` < 1, 1, 0))

# Step 2: summarise branch-level success
summary_branches <- full.exin.data %>%
  group_by(species, individual, branch) %>%
  summarise(
    co2h2o_ok = if (all(c("CO2_OK", "ΔH2O:OK") %in% names(cur_data()))) {
      all(c_across(all_of(c("CO2_OK", "ΔH2O:OK"))) == 1, na.rm = FALSE)
    } else {
      FALSE
    },
    gasexchange_ok = if (all(c("A:OK", "E:OK", "gsw:OK") %in% names(cur_data()))) {
      all(c_across(all_of(c("A:OK", "E:OK", "gsw:OK"))) == 1, na.rm = FALSE)
    } else {
      FALSE
    },
    .groups = "drop"
  ) %>%
  mutate(success = co2h2o_ok | gasexchange_ok) %>%
  group_by(species, individual) %>%
  summarise(
    total_branches = n(),
    successful_branches = sum(success, na.rm = TRUE),
    failed_branches = total_branches - successful_branches,
    .groups = "drop"
  )

# Step 3: filter the full dataset to only successful rows
full.exin.data <- full.exin.data %>%
  filter(
    # Case 1: both new CO2_OK and ΔH2O:OK exist and are 1
    (("CO2_OK" %in% names(.)) & ("ΔH2O:OK" %in% names(.)) &
       CO2_OK == 1 & `ΔH2O:OK` == 1) |
      # Case 2: all three gas exchange columns exist and are 1
      (all(c("A:OK", "E:OK", "gsw:OK") %in% names(.)) &
         `A:OK` == 1 & `E:OK` == 1 & `gsw:OK` == 1)
  )

summary_branches

