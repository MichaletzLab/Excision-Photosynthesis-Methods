#
t.test(calc.wp ~ Ex.int, data =full.exin.data)

test.intact = full.exin.data%>%
  filter(Ex.int =="Intact")
mean(test.intact$calc.wp)
watpot.lm.int=lm(calc.wp~elapsed,data=test.intact)
(summary(watpot.lm.int))

test.ex = full.exin.data%>%
  filter(Ex.int =="Excised")
mean(test.ex$calc.wp)
watpot.lm.ex=lm(calc.wp~elapsed,
                data=full.exin.data)
(summary(watpot.lm.ex))

#Test wp for each species
results <- full.exin.data %>%
  group_by(species) %>%
  summarise(
    p_value = t.test(calc.wp ~ Ex.int)$p.value,
    mean_intact = mean(calc.wp[Ex.int == "Intact"], na.rm = TRUE),
    mean_excised = mean(calc.wp[Ex.int == "Excised"], na.rm = TRUE),
    .groups = "drop"
  )
results

full.exin.data %>%
  group_by(species)%>%
  filter(Ex.int == "Intact") %>%
  summarise(mean_calcwp = mean(calc.wp, na.rm = TRUE), .groups = "drop")
full.exin.data %>%
  group_by(species)%>%
  filter(Ex.int == "Excised") %>%
  summarise(mean_calcwp = mean(calc.wp, na.rm = TRUE), .groups = "drop")

results <- full.exin.data %>%
  group_by(species) %>%
  summarise(
    p_value = t.test(gsw ~ Ex.int)$p.value,
    mean_intact = mean(gsw[Ex.int == "Intact"], na.rm = TRUE),
    mean_excised = mean(gsw[Ex.int == "Excised"], na.rm = TRUE),
    .groups = "drop"
  )
results

results <- full.exin.data %>%
  group_by(species) %>%
  summarise(
    p_value = t.test(A ~ Ex.int)$p.value,
    mean_intact = mean(A[Ex.int == "Intact"], na.rm = TRUE),
    mean_excised = mean(A[Ex.int == "Excised"], na.rm = TRUE),
    .groups = "drop"
  )
results

# Slope tests: Change from A to gs and back for the two results. 
slopes <- full.exin.delta %>%
  group_by(species) %>%
  do({
    mod <- lm(gs.ex ~ gs.int, data = .)
    coef_summary <- summary(mod)$coefficients["gs.int", ]
    
    slope <- coef_summary["Estimate"]
    se <- coef_summary["Std. Error"]
    
    # t-test for H0: slope = 1
    t_value <- (slope - 1) / se
    df <- mod$df.residual
    p_value <- 2 * pt(-abs(t_value), df)
    
    data.frame(
      slope = slope,
      std_error = se,
      t_value = t_value,
      df = df,
      p_value = p_value
    )
  })

slopes

## Mixed Models:
#See figure 4 code for relevant contrasts and species effects (all p-values in the contrast tables)

#USO p-values
#See figure 5 code for the p-values in the combined_data

# Figure 6 legend sample sizes: first run code for figure 6
# Xylem types
xylem_n <- newRRDat %>%
  filter(!is.na(Xylem) & Xylem != "Unknown") %>%
  group_by(Xylem) %>%
  summarise(n_total = sum(n, na.rm = TRUE))
xylem_n

# Exudates
exudates_n <- newRRDat %>%
  filter(!is.na(Exudates) & Exudates != "Unknown") %>%
  group_by(Exudates) %>%
  summarise(n_total = sum(n, na.rm = TRUE))
exudates_n

# Clade
clade_n <- newRRDat %>%
  filter(!is.na(Clade) & Clade != "Unknown") %>%
  group_by(Clade) %>%
  summarise(n_total = sum(n, na.rm = TRUE))
clade_n

# Drought strategy
drought_n <- newRRDat %>%
  filter(!is.na(Drought_Strategy) & Drought_Strategy != "Unknown") %>%
  group_by(Drought_Strategy) %>%
  summarise(n_total = sum(n, na.rm = TRUE))
drought_n