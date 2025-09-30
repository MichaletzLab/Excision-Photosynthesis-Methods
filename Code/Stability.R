full.exin.data <- full.exin.data %>%
  mutate(
    A            = as.numeric(A),
    gsw          = as.numeric(gsw),
    Stable       = as.numeric(Stable),
    `ΔCO2:SLP`   = as.numeric(`ΔCO2:SLP`),
    `ΔH2O:SLP`   = as.numeric(`ΔH2O:SLP`)
  )

#New Stability checks:
all_corr <- bind_rows(
  AC.corr.dat  %>% mutate(species = "Acer campestre")%>% mutate(Qmax_d = as.numeric(Qmax_d))%>% mutate(Qmax = as.numeric(Qmax)),
  BP.corr.dat  %>% mutate(species = "Betula papyrifera")%>% mutate(Qmax_d = as.numeric(Qmax_d))%>% mutate(Qmax = as.numeric(Qmax)),
  CB.corr.dat  %>% mutate(species = "Carpinus betulus")%>% mutate(Qmax_d = as.numeric(Qmax_d))%>% mutate(Qmax = as.numeric(Qmax)),
  PM.corr.dat  %>% mutate(species = "Pseudotsuga menziesii")%>% mutate(Qmax_d = as.numeric(Qmax_d))%>% mutate(Qmax = as.numeric(Qmax)),
  QG.corr.dat  %>% mutate(species = "Quercus garryana")%>% mutate(Qmax_d = as.numeric(Qmax_d))%>% mutate(Qmax = as.numeric(Qmax))
)

# Ensure elapsed is in minutes
all_corr <- all_corr %>%
  mutate(time_min = elapsed / 60)

# Overall slope for each individual
overall_slopes <- all_corr %>%
  group_by(species, indiv) %>%
  summarise(
    overall_slope = coef(lm(A ~ time_min))[2],
    .groups = "drop"
  )

window <- 5  # minutes around 20-min mark

slopes_20min <- all_corr %>%
  filter(time_min >= 20 - window, time_min <= 20 + window) %>%
  group_by(species, indiv) %>%
  summarise(
    slope_20min = if (n() > 1) coef(lm(A ~ time_min))[2] else NA_real_,
    .groups = "drop"
  )
slope_compare <- left_join(overall_slopes, slopes_20min, by = c("species", "indiv"))

summary(slope_compare)


#### New for 2,3,4,5 mins:
library(dplyr)
library(broom)
library(purrr)

# ---- Function (unchanged) ----
slope_window_metrics <- function(df, center_min = 20, window_min = 5) {
  df <- df %>% mutate(elapsed_min = elapsed / 60)
  start <- center_min - window_min / 2
  end   <- center_min + window_min / 2
  
  win <- df %>% filter(elapsed_min >= start & elapsed_min <= end)
  if (nrow(win) < 3) {
    return(tibble(window_min = window_min,
                  slope_A_per_min = NA_real_,
                  slope_gsw_per_min = NA_real_))
  }
  
  slopeA <- coef(lm(A   ~ elapsed_min, data = win))[["elapsed_min"]]
  slopeG <- coef(lm(gsw ~ elapsed_min, data = win))[["elapsed_min"]]
  
  tibble(window_min = window_min,
         slope_A_per_min = slopeA,
         slope_gsw_per_min = slopeG)
}

# ---- Apply to multiple window sizes ----
window_sizes <- c(2, 3, 4, 5)  # windows to test

results_all <- map_dfr(
  window_sizes,
  function(w) {
    all_corr %>%
      group_by(species, indiv) %>%
      group_modify(~ slope_window_metrics(.x, center_min = 20, window_min = w)) %>%
      ungroup()
  }
)


# LI-COR thresholds
A_thresh   <- 0.1    # μmol m⁻² s⁻¹ per minute
gsw_thresh <- 0.05   # mol m⁻² s⁻¹ per minute

results_all <- map_dfr(
  window_sizes,
  function(w) {
    all_corr %>%
      group_by(species, indiv) %>%
      group_modify(~ slope_window_metrics(.x, center_min = 20, window_min = w)) %>%
      ungroup() %>%
      mutate(
        A_stable   = !is.na(slope_A_per_min)   & abs(slope_A_per_min)   <= A_thresh,
        gsw_stable = !is.na(slope_gsw_per_min) & abs(slope_gsw_per_min) <= gsw_thresh,
        both_stable= A_stable & gsw_stable
      )
  }
)


## And for the 2 data points closest to 20  minutes:

# Thresholds from LI-COR manual
A_thresh_per_min   <- 0.1   # μmol m⁻² s⁻¹ per minute
gsw_thresh_per_min <- 0.05  # mol m⁻² s⁻¹ per minute

closest_two_slopes <- all_corr %>%
  mutate(time_min = elapsed / 60) %>%
  group_by(species, indiv) %>%
  slice_min(order_by = abs(time_min - 20), n = 2, with_ties = FALSE) %>%
  summarise(
    slope_A_per_min = if (n() == 2) diff(A)   / diff(time_min) else NA_real_,
    slope_gsw_per_min = if (n() == 2) diff(gsw) / diff(time_min) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    A_stable   = !is.na(slope_A_per_min)   & abs(slope_A_per_min)   <= A_thresh_per_min,
    gsw_stable = !is.na(slope_gsw_per_min) & abs(slope_gsw_per_min) <= gsw_thresh_per_min,
    both_stable = A_stable & gsw_stable
  )

print(closest_two_slopes)
