#Asat
ratio.data = full.exin.delta%>%
  group_by(species, obs) %>%
  summarize(ratio_avg = mean(ratio))%>%
  mutate(time=case_when(
    obs=="1" ~ "08:30",
    obs=="3" ~ "09:30",
    obs=="5" ~ "10:30",
    obs=="7" ~ "11:30",
    obs=="9" ~ "12:30",
    obs=="11" ~ "13:30",
    obs=="13" ~ "14:30",
    obs=="15" ~ "15:30"))%>%
  group_by(species,time)
ratio.data = ratio.data%>%
  mutate(errorA=ratio_avg-(sd(ratio.data$ratio_avg)))


(ac.A.time <- ratio.data %>%
    filter(species == "A. campestre") %>%
    ggplot(aes(x = time, y = ratio_avg, color = "A. campestre", group="A. campestre")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = ratio_avg - errorA,
      ymax = ratio_avg + errorA
    ), width = 0.2, color = "#1B9E77") +
    theme_bw() +
    scale_color_manual(values = "#1B9E77", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab(" ") + theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(bp.A.time <- ratio.data %>%
    filter(species == "B. papyrifera") %>%
    ggplot(aes(x = time, y = ratio_avg, color = "B. papyrifera", group="B. papyrifera")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = ratio_avg - errorA,
      ymax = ratio_avg + errorA
    ), width = 0.2, color = "#D95F02") +
    theme_bw() +
    scale_color_manual(values = "#D95F02", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab(" ") +theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(cb.A.time <- ratio.data %>%
    filter(species == "C. betulus") %>%
    ggplot(aes(x = time, y = ratio_avg, color = "C. betulus", group="C. betulus")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = ratio_avg - errorA,
      ymax = ratio_avg + errorA
    ), width = 0.2, color = "#7570B3") +
    theme_bw() +
    scale_color_manual(values = "#7570B3", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab("") +theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(pm.A.time <- ratio.data %>%
    filter(species == "P. menziesii") %>%
    ggplot(aes(x = time, y = ratio_avg, color = "P. menziesii", group="P. menziesii")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = ratio_avg - errorA,
      ymax = ratio_avg + errorA
    ), width = 0.2, color = "#E7298A") +
    theme_bw() +
    scale_color_manual(values = "#E7298A", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab(" ") +theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(qg.A.time <- ratio.data %>%
    filter(species == "Q. garryana") %>%
    ggplot(aes(x = time, y = ratio_avg, color = "Q. garryana", group="Q. garryana")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = ratio_avg - errorA,
      ymax = ratio_avg + errorA
    ), width = 0.2, color = "#66A61E") +
    theme_bw() +
    scale_color_manual(values = "#66A61E", name = "") +
    theme(axis.text.x = element_text(size = 10)) +theme(legend.position = "none")+
    xlab(" ") + ylab(" ") +
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

watpot.dat=full.exin.delta%>%
  group_by(species, obs) %>%
  summarize(watpot_avg = mean(watpot.ratio))%>%
  mutate(errorW=watpot_avg-(sd(ratio.data$watpot_avg)))%>%
  mutate(time=case_when(
    obs=="1" ~ "08:30",
    obs=="3" ~ "09:30",
    obs=="5" ~ "10:30",
    obs=="7" ~ "11:30",
    obs=="9" ~ "12:30",
    obs=="11" ~ "13:30",
    obs=="13" ~ "14:30",
    obs=="15" ~ "15:30"))%>%
  group_by(species,time)
watpot.dat = watpot.dat%>%
  mutate(errorW=watpot_avg-(sd(watpot.dat$watpot_avg)))

(ac.W.time= watpot.dat %>%
    filter(species == "A. campestre") %>%
    ggplot(aes(x = time, y = watpot_avg, color = "A. campestre", group="A. campestre")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = watpot_avg - errorW,
      ymax = watpot_avg + errorW
    ), width = 0.2, color = "#1B9E77") +
    theme_bw() +
    scale_color_manual(values = "#1B9E77", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab(" ") +theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(bp.W.time= watpot.dat %>%
    filter(species == "B. papyrifera") %>%
    ggplot(aes(x = time, y = watpot_avg, color = "B. papyrifera", group="B. papyrifera")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = watpot_avg - errorW,
      ymax = watpot_avg + errorW
    ), width = 0.2, color = "#D95F02") +
    theme_bw() +
    scale_color_manual(values = "#D95F02", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab(" ") +theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(cb.W.time= watpot.dat %>%
    filter(species == "C. betulus") %>%
    ggplot(aes(x = time, y = watpot_avg, color = "C. betulus", group="C. betulus")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = watpot_avg - errorW,
      ymax = watpot_avg + errorW
    ), width = 0.2, color = "#7570B3") +
    theme_bw() +
    scale_color_manual(values = "#7570B3", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab("") +theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(pm.W.time= watpot.dat %>%
    filter(species == "P. menziesii") %>%
    ggplot(aes(x = time, y = watpot_avg, color = "P. menziesii", group="P. menziesii")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = watpot_avg - errorW,
      ymax = watpot_avg + errorW
    ), width = 0.2, color = "#E7298A") +
    theme_bw() +
    scale_color_manual(values = "#E7298A", name = "") +
    theme(axis.text.x = element_text(size = 15)) +theme(legend.position = "none")+
    xlab(" ") + ylab(" ") +theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(qg.W.time= watpot.dat %>%
    filter(species == "Q. garryana") %>%
    ggplot(aes(x = time, y = watpot_avg, color = "Q. garryana", group="Q. garryana")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = watpot_avg - errorW,
      ymax = watpot_avg + errorW
    ), width = 0.2, color = "#66A61E") +
    theme_bw() +
    scale_color_manual(values = "#66A61E", name = "") +
    theme(axis.text.x = element_text(size = 10)) +theme(legend.position = "none")+
    xlab("") + ylab(" ") +
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))


gs.dat=full.exin.delta%>%
  group_by(species, obs) %>%
  summarize(gs_avg = mean(ratio.gs))%>%
  mutate(time=case_when(
    obs=="1" ~ "08:30",
    obs=="3" ~ "09:30",
    obs=="5" ~ "10:30",
    obs=="7" ~ "11:30",
    obs=="9" ~ "12:30",
    obs=="11" ~ "13:30",
    obs=="13" ~ "14:30",
    obs=="15" ~ "15:30"))
gs.dat=gs.dat%>%
  group_by(species,time)  
gs.dat = gs.dat%>%
  mutate(errorg=gs_avg-(sd(gs.dat$gs_avg)))


(ac.g.time= gs.dat %>%
    filter(species == "A. campestre") %>%
    ggplot(aes(x =time, y=gs_avg, color = "A. campestre", group="A. campestre")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = gs_avg - errorg,
      ymax = gs_avg + errorg), width = 0.2, color = "#1B9E77") +
    theme_bw() +
    scale_color_manual(values = "#1B9E77", name = "") +
    theme(axis.text.x = element_text(size = 15)) +
    xlab(" ")+ylab(" ")+theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(bp.g.time= gs.dat %>%
    filter(species == "B. papyrifera") %>%
    ggplot(aes(x =time, y=gs_avg, color = "B. papyrifera", group="B. papyrifera")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = gs_avg - errorg,
      ymax = gs_avg + errorg
    ), width = 0.2, color = "#D95F02") +
    theme_bw() +
    scale_color_manual(values = "#D95F02", name = "") +
    theme(axis.text.x = element_text(size = 15)) +
    xlab(" ")+ylab(" ")+theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(cb.g.time= gs.dat %>%
    filter(species == "C. betulus") %>%
    ggplot(aes(x =time, y=gs_avg, color = "C. betulus", group="C. betulus")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = gs_avg - errorg,
      ymax = gs_avg + errorg
    ), width = 0.2, color = "#7570B3") +
    theme_bw() +
    scale_color_manual(values = "#7570B3", name = "") +
    theme(axis.text.x = element_text(size = 15)) +
    xlab(" ")+ylab("")+theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(pm.g.time= gs.dat %>%
    filter(species == "P. menziesii") %>%
    ggplot(aes(x =time, y=gs_avg, color = "P. menziesii", group="P. menziesii")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = gs_avg - errorg,
      ymax = gs_avg + errorg
    ), width = 0.2, color = "#E7298A") +
    theme_bw() +
    scale_color_manual(values = "#E7298A", name = "") +
    theme(axis.text.x = element_text(size = 15)) +
    xlab(" ")+ylab(" ")+theme(axis.text.x = element_blank())+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(qg.g.time= gs.dat %>%
    filter(species == "Q. garryana") %>%
    ggplot(aes(x =time, y=gs_avg, color = "Q. garryana", group="Q. garryana")) +
    geom_point() + geom_line() +
    geom_errorbar(aes(
      ymin = gs_avg - errorg,
      ymax = gs_avg + errorg
    ), width = 0.2, color = "#66A61E") +
    theme_bw() +
    scale_color_manual(values = "#66A61E", name = "") +
    theme(axis.text.x = element_text(size = 10)) +
    xlab("Time of day")+ylab(" ")+
    geom_hline(yintercept = 1, linetype = "dashed")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(ratios.time=ggarrange(ac.W.time,ac.g.time,ac.A.time,bp.W.time,bp.g.time,bp.A.time,cb.W.time,cb.g.time,cb.A.time,pm.W.time,pm.g.time,pm.A.time,qg.W.time,qg.g.time,qg.A.time,  ncol = 3, nrow = 5, legend = FALSE))

