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


