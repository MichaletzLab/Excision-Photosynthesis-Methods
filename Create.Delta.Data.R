#This document requires that the file Read.and.Modify.Data.R has already been run.
#Creates a new data frame with calculated values from full.exin.data required for analysis

#Load libraries
library(tidyr)

#This code takes columns from full.exin.data and splits them apart by the treatment Ex.int.
full.exin.delta <- full.exin.data %>%
  pivot_wider(
    id_cols = c("species", "obs", "individual", "branch"),
    names_from = "Ex.int",
    values_from = c("A", "Air.temp", "Measure.height", "Water.pot", "elapsed", "gsw", "Ci", "Ca", "VPDleaf")
  ) %>%
  group_by(branch) %>%
  reframe(across(where(is.numeric), list(~coalesce(.[!is.na(.)], na.rm = TRUE))),
          across(where(is.character), ~first(.[!is.na(.)]))) %>%
  ungroup() %>%
  distinct(branch, .keep_all = TRUE)  # Keep the first occurrence of each branch

# Rename all columns for consistency
new_col_names <- colnames(full.exin.delta) %>%
  stringr::str_replace(".Intact", ".int") %>%
  stringr::str_replace(".Excised", ".ex")

#Rename individual columns to match what is used in analysis document / for figures
full.exin.delta <- full.exin.delta %>%
  rename_with(~new_col_names, everything())%>%
  rename(gs.int = gsw.int)%>%
  rename(gs.ex = gsw.ex)%>%
  rename(watpot.int = Water.pot.int)%>%
  rename(watpot.ex = Water.pot.ex)%>%
  rename(height = Measure.height.int)

#Make new columns with calculated values like delta and ratios as well as the water potential values in mpa
full.exin.delta <- full.exin.delta %>%
  mutate(
    across(c(A.ex, A.int, gs.ex, gs.int, watpot.int, watpot.ex), as.numeric),
    height = as.numeric(height),
    height.cm = height * 2.54,
    watpot.int.mpa = watpot.int * -0.00689476,
    watpot.ex.mpa = watpot.ex * -0.00689476,
    Air.temp.int = as.numeric(Air.temp.int),
    Air.temp.ex = as.numeric(Air.temp.ex),
    temp = (Air.temp.int + Air.temp.ex) / 2 ) %>%
  mutate(
    delta = A.ex - A.int,
    ratio = A.ex / A.int,
    delta.gs = gs.ex - gs.int,
    p.change = delta / A.int,
    gm.int = A.int / as.numeric(Ci.int),
    gm.ex = A.ex / as.numeric(Ci.ex),
    delt.watpot.mpa = watpot.ex.mpa - watpot.int.mpa)

