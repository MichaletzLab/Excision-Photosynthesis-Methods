#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**

full.exin.data$obs=as.character(full.exin.data$obs)
full.exin.data$elapsed = as.numeric(full.exin.data$elapsed)

test.intact = full.exin.data%>%
  filter(Ex.int =="Intact")
watpot.lm.int=lm(calc.wp~elapsed,data=test.intact)
(summary(watpot.lm.int)) #Time is super significant!

test.ex = full.exin.data%>%
  filter(Ex.int =="Excised")
watpot.lm.ex=lm(calc.wp~elapsed,data=full.exin.data)
(summary(watpot.lm.ex)) #Time is not significant (borderline significant without unstable individuals)

A.watpot.lm.dat=full.exin.data%>%
  filter(Ex.int=="Intact")
A.watpot.lm=lm(A~calc.wp,data=A.watpot.lm.dat)
(summary(A.watpot.lm)) #Water potential is actually a good predictor of intact A...

wat.mean.ex <- full.exin.data %>%
  filter(Ex.int == "Excised") %>%
  group_by(obs) %>%
  summarize(
    mean_Water.pot = mean(calc.wp),
    SE_wat.pot = sd(calc.wp) / sqrt(n()))

wat.mean.int <- full.exin.data %>%
  filter(Ex.int == "Intact") %>%
  group_by(obs) %>%
  summarize(
    mean_Water.pot = mean(calc.wp),
    SE_wat.pot = sd(calc.wp) / sqrt(n()))

wat.mean.ex$Set <- "Excised"
wat.mean.int$Set <- "Intact"
(wat.df <- bind_rows(wat.mean.ex, wat.mean.int))

wat.df = wat.df%>%mutate(time=case_when(
  obs=="1" ~ "08:30",obs=="2" ~ "08:30",
  obs=="3" ~ "09:30",obs=="4" ~ "09:30",
  obs=="5" ~ "10:30",obs=="6" ~ "10:30",
  obs=="7" ~ "11:30",obs=="8" ~ "11:30",
  obs=="9" ~ "12:30",obs=="10" ~ "12:30",
  obs=="11" ~ "13:30",obs=="12" ~ "13:30",
  obs=="13" ~ "14:30",obs=="14" ~ "14:30",
  obs=="15" ~ "15:30",obs=="16" ~ "15:30"))

(watpotplot=wat.df%>%
    ggplot(aes(x =time, y=mean_Water.pot,color = Set, group=Set)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = mean_Water.pot - SE_wat.pot,
      ymax = mean_Water.pot + SE_wat.pot),width = 0.2)+
    xlab("Time of day (hours)")+ylab("Water potential (MPa)")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(watpotplot.reg = wat.df %>%
    ggplot(aes(x = time, y = mean_Water.pot, color = Set, group = Set)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, aes(group = Set, color = Set, fill = Set)) + # Set color and fill aesthetics
    theme_bw() +
    scale_color_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
    scale_fill_manual(values = c("dodgerblue1", "deeppink4"), name = "") + # Set fill colors for the bands
    xlab("Time of day (hours)") +
    ylab("Water potential (MPa)") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank()
    )
)

(watpotplot_combined <- wat.df %>%
    ggplot(aes(x = time, y = mean_Water.pot, color = Set, group = Set)) +
    # Add points
    geom_point() +
    # Add error bars
    geom_errorbar(aes(
      ymin = mean_Water.pot - SE_wat.pot,
      ymax = mean_Water.pot + SE_wat.pot
    ), width = 0.2) +
    # Add smooth line with shaded 95% confidence interval
    geom_smooth(method = "lm", se = TRUE, aes(fill = Set)) +
    # Set colors for line and fill
    scale_color_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
    scale_fill_manual(values = c("dodgerblue1", "deeppink4"), name = "") +
    # Adjust axis labels and theme
    xlab("Time of day (hours)") +
    ylab("Water potential (MPa)") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      panel.border = element_blank()
    ))
