AC.corr.dat <- read.csv("Data/Corr.dat/AC.corr.dat_plot.csv")
(plot.AC.corr <- AC.corr.dat %>%
    mutate(indiv = as.factor(indiv)) %>%  # Ensure 'individual' is a factor
    ggplot(aes(x = elapsed/60, y = A, color = indiv, group = indiv)) +  # Add group = individual
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Acer campestre", x = " ", y = "") + 
    theme_minimal() + 
    scale_color_manual(values = c("#1B9E77", "#1B9E77", "#1B9E77", "#1B9E77", "#1B9E77"), name = "") +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank(),
      plot.title = element_text(size = 16, face="italic"),  # Increase title size
      axis.title.x = element_text(size = 15),  # Increase x-axis title size
      axis.title.y = element_text(size = 15),  # Increase y-axis title size
      axis.text.x = element_text(size = 13),  # Increase x-axis text size
      axis.text.y = element_text(size = 13)   # Increase y-axis text size
    )
)

BP.corr.dat <- read.csv("Data/corr.dat/BP.corr.dat_plot.csv")
(plot.BP.corr <- BP.corr.dat %>%
    mutate(indiv = as.factor(indiv)) %>%  # Ensure 'individual' is a factor
    ggplot(aes(x = elapsed/60, y = A, color = indiv, group = indiv)) +  # Add group = individual
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Betula papyrifera", x = " ", y = "") + 
    theme_minimal() + 
    scale_color_manual(values = c("#D95F02","#D95F02","#D95F02","#D95F02","#D95F02"), name = "") + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank(),
      plot.title = element_text(size = 16, face="italic"),  # Increase title size
      axis.title.x = element_text(size = 15),  # Increase x-axis title size
      axis.title.y = element_text(size = 15),  # Increase y-axis title size
      axis.text.x = element_text(size = 13),  # Increase x-axis text size
      axis.text.y = element_text(size = 13)   # Increase y-axis text size
    )
)

CB.corr.dat <- read.csv("Data/corr.dat/CB.corr.dat_plot.csv")
(plot.CB.corr <- CB.corr.dat %>%
    mutate(indiv = as.factor(indiv)) %>%  # Ensure 'individual' is a factor
    ggplot(aes(x = elapsed/60, y = A, color = indiv, group = indiv)) +  # Add group = individual
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Carpinus betulus", x = " ", y = expression('Photosynthetic rate (μmol/m'^2*'s'^-1*')')) + 
    theme_minimal() + 
    scale_color_manual(values = c("#7570B3","#7570B3","#7570B3","#7570B3","#7570B3"), name = "") + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank(),
      plot.title = element_text(size = 16, face="italic"),  # Increase title size
      axis.title.x = element_text(size = 15),  # Increase x-axis title size
      axis.title.y = element_text(size = 15),  # Increase y-axis title size
      axis.text.x = element_text(size = 13),  # Increase x-axis text size
      axis.text.y = element_text(size = 13)   # Increase y-axis text size
    )
)

PM.corr.dat <- read.csv("Data/corr.dat/PM.corr.dat_plot.csv")
(plot.PM.corr <- PM.corr.dat %>%
    mutate(indiv = as.factor(indiv)) %>%  # Ensure 'individual' is a factor
    ggplot(aes(x = elapsed/60, y = A, color = indiv, group = indiv)) +  # Add group = individual
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Pseudotsuga menziesii", x = "Minutes", y = "") + 
    theme_minimal() + 
    scale_color_manual(values = c("#E7298A","#E7298A","#E7298A"), name = "") + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank(),
      plot.title = element_text(size = 16, face="italic"),  # Increase title size
      axis.title.x = element_text(size = 15),  # Increase x-axis title size
      axis.title.y = element_text(size = 15),  # Increase y-axis title size
      axis.text.x = element_text(size = 13),  # Increase x-axis text size
      axis.text.y = element_text(size = 13)   # Increase y-axis text size
    )
)

QG.corr.dat <- read.csv("Data/corr.dat/QG.corr.dat_plot.csv")
(plot.QG.corr <- QG.corr.dat %>%
    mutate(indiv = as.factor(indiv)) %>%  # Ensure 'individual' is a factor
    ggplot(aes(x = elapsed/60, y = A, color = indiv, group = indiv)) +  # Add group = individual
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Quercus garryana", x = "Minutes", y = "") + 
    theme_minimal() + 
    scale_color_manual(values = c("#66A61E","#66A61E","#66A61E","#66A61E","#66A61E"), name = "") + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank(),
      plot.title = element_text(size = 16, face="italic"),  # Increase title size
      axis.title.x = element_text(size = 15),  # Increase x-axis title size
      axis.title.y = element_text(size = 15),  # Increase y-axis title size
      axis.text.x = element_text(size = 13),  # Increase x-axis text size
      axis.text.y = element_text(size = 13)   # Increase y-axis text size
    )
)


(corr.plot.all=ggarrange(plot.AC.corr,plot.BP.corr,plot.CB.corr,plot.PM.corr,plot.QG.corr,nrow=3,ncol=2))


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




## Another stability test:
# thresholds (per minute)
#According to licor: Over the course of a minute, if photosynthesis changes by less than 0.1 μmol m-2 s-1 , and conductance changes by less than 0.05 mol m-2 s-1 then you have stability.
A_thresh_per_min   <- 0.1   # μmol m^-2 s^-1 per minute
gsw_thresh_per_min <- 0.05  # mol m^-2 s^-1 per minute

# function to compute slopes and stats over a 5-min window (because data was taken every 30sec) centered at 20 min
slope_window_metrics <- function(df, center_min = 20, window_min = 5) {
  # convert elapsed from seconds to minutes
  df <- df %>% mutate(elapsed_min = elapsed / 60)
  
  start <- center_min - window_min / 2
  end   <- center_min + window_min / 2
  
  win <- df %>% filter(elapsed_min >= start & elapsed_min <= end)
  
  if (nrow(win) < 3) {
    return(tibble(
      n_pts = nrow(win),
      slope_A_per_min = NA_real_,
      slope_gsw_per_min = NA_real_,
      slopeA_p = NA_real_,
      slopeG_p = NA_real_,
      slopeA_r2 = NA_real_,
      slopeG_r2 = NA_real_,
      maxmin_A = ifelse(nrow(win)==0, NA_real_, max(win$A, na.rm=TRUE) - min(win$A, na.rm=TRUE)),
      maxmin_gsw = ifelse(nrow(win)==0, NA_real_, max(win$gsw, na.rm=TRUE) - min(win$gsw, na.rm=TRUE))
    ))
  }
  
  fitA <- tryCatch(lm(A ~ elapsed_min, data = win), error = function(e) NULL)
  fitG <- tryCatch(lm(gsw ~ elapsed_min, data = win), error = function(e) NULL)
  
  slopeA <- if (!is.null(fitA)) coef(fitA)[["elapsed_min"]] else NA_real_
  slopeG <- if (!is.null(fitG)) coef(fitG)[["elapsed_min"]] else NA_real_
  
  tidyA   <- if (!is.null(fitA)) broom::tidy(fitA) else NULL
  glanceA <- if (!is.null(fitA)) broom::glance(fitA) else NULL
  tidyG   <- if (!is.null(fitG)) broom::tidy(fitG) else NULL
  glanceG <- if (!is.null(fitG)) broom::glance(fitG) else NULL
  
  slopeA_p <- if (!is.null(tidyA)) tidyA %>% filter(term == "elapsed_min") %>% pull(p.value) else NA_real_
  slopeG_p <- if (!is.null(tidyG)) tidyG %>% filter(term == "elapsed_min") %>% pull(p.value) else NA_real_
  slopeA_r2 <- if (!is.null(glanceA)) glanceA$r.squared else NA_real_
  slopeG_r2 <- if (!is.null(glanceG)) glanceG$r.squared else NA_real_
  
  tibble(
    n_pts = nrow(win),
    slope_A_per_min = as.numeric(slopeA),
    slope_gsw_per_min = as.numeric(slopeG),
    slopeA_p = slopeA_p,
    slopeG_p = slopeG_p,
    slopeA_r2 = slopeA_r2,
    slopeG_r2 = slopeG_r2,
    maxmin_A = max(win$A, na.rm=TRUE) - min(win$A, na.rm=TRUE),
    maxmin_gsw = max(win$gsw, na.rm=TRUE) - min(win$gsw, na.rm=TRUE)
  )
}

# Apply across species × indiv
results_5min <- all_corr %>%
  group_by(species, indiv) %>%
  group_modify(~ slope_window_metrics(.x, center_min = 20, window_min = 5)) %>%
  ungroup() %>%
  mutate(
    A_stable_20min   = !is.na(slope_A_per_min) & abs(slope_A_per_min) <= A_thresh_per_min,
    gsw_stable_20min = !is.na(slope_gsw_per_min) & abs(slope_gsw_per_min) <= gsw_thresh_per_min,
    both_stable_20min= A_stable_20min & gsw_stable_20min
  )

print(results_5min)

















#### New for 1,2,3,4,5 mins:
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
window_sizes <- c(2, 3, 4)  # windows to test

results_all <- map_dfr(
  window_sizes,
  function(w) {
    all_corr %>%
      group_by(species, indiv) %>%
      group_modify(~ slope_window_metrics(.x, center_min = 20, window_min = w)) %>%
      ungroup()
  }
)

# ---- Inspect results ----
print(results_all)

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

print(results_all)


