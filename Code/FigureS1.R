pm.dat= full.exin.data%>%
  filter(species=="P. menziesii")%>%
  group_by(Ex.int, obs) %>%
  summarize(A_avg = mean(A),
            A_sd = sd(A, na.rm = TRUE))%>%
  mutate(time=case_when(
    obs=="1" ~ "08:50",obs=="2" ~ "08:50",
    obs=="3" ~ "09:50",obs=="4" ~ "09:50",
    obs=="5" ~ "10:50",obs=="6" ~ "10:50",
    obs=="7" ~ "11:50",obs=="8" ~ "11:50",
    obs=="9" ~ "12:50",obs=="10" ~ "12:50",
    obs=="11" ~ "13:50",obs=="12" ~ "13:50",
    obs=="13" ~ "14:50",obs=="14" ~ "14:50",
    obs=="15" ~ "15:50",obs=="16" ~ "15:50"))
(pm.plot=pm.dat%>%
    ggplot(aes(x =time, y=A_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw() + scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = A_avg - ifelse(Ex.int == "Intact", 1.202876, 1.008947), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = A_avg + ifelse(Ex.int == "Intact", 1.202876, 1.008947)),width = 0.2)+
    xlab(" ")+ylab("")+
    theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))



(ac.dat= full.exin.data%>%
    filter(species=="A. campestre")%>%
    group_by(Ex.int, obs) %>%
    summarize(A_avg = mean(A),
              A_sd = sd(A, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(ac.plot=ac.dat%>%
    ggplot(aes(x =time, y=A_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = A_avg - ifelse(Ex.int == "Intact", 1.134805, 0.7450558), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = A_avg + ifelse(Ex.int == "Intact", 1.134805, 0.7450558)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(bp.dat= full.exin.data%>%
    filter(species=="B. papyrifera")%>%
    group_by(Ex.int, obs) %>%
    summarize(A_avg = mean(A),
              A_sd = sd(A, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(bp.plot=bp.dat%>%
    ggplot(aes(x =time, y=A_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+  scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = A_avg - ifelse(Ex.int == "Intact", 1.331576, 1.018), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = A_avg + ifelse(Ex.int == "Intact", 1.331576, 1.018)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(cb.dat= full.exin.data%>%
    filter(species=="C. betulus")%>%
    group_by(Ex.int, obs) %>%
    summarize(A_avg = mean(A),
              A_sd = sd(A, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(cb.plot=cb.dat%>%
    ggplot(aes(x =time, y=A_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw() + scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = A_avg - ifelse(Ex.int == "Intact", 0.499928, 0.6236215), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = A_avg + ifelse(Ex.int == "Intact", 0.499928, 0.6236215)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(qg.dat= full.exin.data%>%
    filter(species=="Q. garryana")%>%
    group_by(Ex.int, obs) %>%
    summarize(A_avg = mean(A),
              A_sd = sd(A, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(qg.plot=qg.dat%>%
    ggplot(aes(x =time, y=A_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    xlab("")+ylab("")+theme(legend.position = "none")+
    geom_errorbar(aes(
      ymin = A_avg - ifelse(Ex.int == "Intact", 1.27177, 1.866506), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = A_avg + ifelse(Ex.int == "Intact", 1.27177, 1.866506)),width = 0.2)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

# Find the common y-axis limits
y_limits <- range(c(ac.dat$A_avg, bp.dat$A_avg, cb.dat$A_avg, pm.dat$A_avg, qg.dat$A_avg))

# Update each plot with the common y-axis limits
ac.plot <- ac.plot + ylim(y_limits)
bp.plot <- bp.plot + ylim(y_limits)
cb.plot <- cb.plot + ylim(y_limits)
pm.plot <- pm.plot + ylim(y_limits)
qg.plot <- qg.plot + ylim(y_limits)

# Arrange the plots
(species.figs <- ggarrange(ac.plot, bp.plot, cb.plot, pm.plot, qg.plot, ncol = 2, nrow = 3))


(wpm.dat= full.exin.data%>%
    mutate(watpot.mpa=Water.pot*-0.00689476)%>%
    filter(species=="P. menziesii")%>%
    group_by(Ex.int, obs) %>%
    summarize(mpa_avg = mean(watpot.mpa),
              mpa_sd = sd(watpot.mpa, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
head(wpm.dat)
(wpm.plot=wpm.dat%>%
    ggplot(aes(x =time, y=mpa_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    xlab(" ")+ylab("")+theme(legend.position = "none")+ #labs(title="Pseudotsuga menziesii",color=NULL)+
    geom_errorbar(aes(
      ymin = mpa_avg - ifelse(Ex.int == "Intact", 0.09294236, 0.1042878), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = mpa_avg + ifelse(Ex.int == "Intact", 0.09294236, 0.1042878)),width = 0.2)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank(),axis.text.x = element_blank()))

(wac.dat= full.exin.data%>%
    mutate(watpot.mpa=Water.pot*-0.00689476)%>%
    filter(species=="A. campestre")%>%
    group_by(Ex.int, obs) %>%
    summarize(mpa_avg = mean(watpot.mpa),
              mpa_sd = sd(watpot.mpa, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(wac.plot=wac.dat%>%
    ggplot(aes(x =time, y=mpa_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = mpa_avg - ifelse(Ex.int == "Intact", 0.1775867, 0.05792067), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = mpa_avg + ifelse(Ex.int == "Intact", 0.1775867, 0.05792067)),width = 0.2)+
    xlab(" ")+ylab("")+#labs(title="Acer campestre",color=NULL)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),axis.text.x = element_blank(),panel.border = element_blank(),legend.position="top",legend.direction="horizontal"))

(wbp.dat= full.exin.data%>%
    mutate(watpot.mpa=Water.pot*-0.00689476)%>%
    filter(species=="B. papyrifera")%>%
    group_by(Ex.int, obs) %>%
    summarize(mpa_avg = mean(watpot.mpa),
              mpa_sd = sd(watpot.mpa, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(wbp.plot=wbp.dat%>%
    ggplot(aes(x =time, y=mpa_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = mpa_avg - ifelse(Ex.int == "Intact", 0.226555, 0.129868), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = mpa_avg + ifelse(Ex.int == "Intact", 0.226555, 0.129868)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+ #labs(title="Betula papyrifera",color=NULL)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(wcb.dat= full.exin.data%>%
    mutate(watpot.mpa=Water.pot*-0.00689476)%>%
    filter(species=="C. betulus")%>%
    group_by(Ex.int, obs) %>%
    summarize(mpa_avg = mean(watpot.mpa),
              mpa_sd = sd(watpot.mpa, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(wcb.plot=wcb.dat%>%
    ggplot(aes(x =time, y=mpa_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = mpa_avg - ifelse(Ex.int == "Intact", 0.1915502, 0.04086272), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = mpa_avg + ifelse(Ex.int == "Intact", 0.1915502, 0.04086272)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+ #labs(title="Carpinus betulus",color=NULL)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(wqg.dat= full.exin.data%>%
    mutate(watpot.mpa=Water.pot*-0.00689476)%>%
    filter(species=="Q. garryana")%>%
    group_by(Ex.int, obs) %>%
    summarize(mpa_avg = mean(watpot.mpa),
              mpa_sd = sd(watpot.mpa, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(wqg.plot=wqg.dat%>%
    ggplot(aes(x =time, y=mpa_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = mpa_avg - ifelse(Ex.int == "Intact", 0.2014454, 0.06166861), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = mpa_avg + ifelse(Ex.int == "Intact", 0.2014454, 0.06166861)),width = 0.2)+
    xlab("")+ylab("")+theme(legend.position = "none")+ #labs(title="Quercus garryana",color=NULL)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))
# Find the common y-axis limits
wy_limits <- range(c(wac.dat$mpa_avg, wbp.dat$mpa_avg, wcb.dat$mpa_avg, wpm.dat$mpa_avg, wqg.dat$mpa_avg))

# Update each plot with the common y-axis limits
wac.plot <- wac.plot + ylim(wy_limits)
wbp.plot <- wbp.plot + ylim(wy_limits)
wcb.plot <- wcb.plot + ylim(wy_limits)
wpm.plot <- wpm.plot + ylim(wy_limits)
wqg.plot <- wqg.plot + ylim(wy_limits)
(species.figs.wat=ggarrange(wac.plot,wbp.plot,wcb.plot,wpm.plot,wqg.plot,  ncol = 2, nrow = 3, labels=c("ac","bp","cb","pm","qg")))


(gpm.dat= full.exin.data%>%
    filter(species=="P. menziesii")%>%
    group_by(Ex.int, obs) %>%
    summarize(g_avg = mean(gsw),
              g_sd = sd(gsw, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
head(gpm.dat)
(gpm.plot=gpm.dat%>%
    ggplot(aes(x =time, y=g_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw() + scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = g_avg - ifelse(Ex.int == "Intact", 0.003455497, 0.005671264), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = g_avg + ifelse(Ex.int == "Intact", 0.003455497, 0.005671264)),width = 0.2)+
    xlab(" ")+ylab("")+
    theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))



(gac.dat= full.exin.data%>%
    filter(species=="A. campestre")%>%
    group_by(Ex.int, obs) %>%
    summarize(g_avg = mean(gsw),
              g_sd = sd(gsw, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(gac.plot=gac.dat%>%
    ggplot(aes(x =time, y=g_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = g_avg - ifelse(Ex.int == "Intact", 0.008486228, 0.006150449), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = g_avg + ifelse(Ex.int == "Intact", 0.008486228, 0.006150449)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(gbp.dat= full.exin.data%>%
    filter(species=="B. papyrifera")%>%
    group_by(Ex.int, obs) %>%
    summarize(g_avg = mean(gsw),
              g_sd = sd(gsw, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(gbp.plot=gbp.dat%>%
    ggplot(aes(x =time, y=g_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+  scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = g_avg - ifelse(Ex.int == "Intact", 0.02327269, 0.01532579), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = g_avg + ifelse(Ex.int == "Intact", 0.02327269, 0.01532579)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(gcb.dat= full.exin.data%>%
    filter(species=="C. betulus")%>%
    group_by(Ex.int, obs) %>%
    summarize(g_avg = mean(gsw),
              g_sd = sd(gsw, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(gcb.plot=gcb.dat%>%
    ggplot(aes(x =time, y=g_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw() + scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = g_avg - ifelse(Ex.int == "Intact", 0.002858918, 0.006297775), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = g_avg + ifelse(Ex.int == "Intact", 0.002858918, 0.006297775)),width = 0.2)+
    xlab(" ")+ylab("")+theme(legend.position = "none",axis.text.x = element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

(gqg.dat= full.exin.data%>%
    filter(species=="Q. garryana")%>%
    group_by(Ex.int, obs) %>%
    summarize(g_avg = mean(gsw),
              g_sd = sd(gsw, na.rm = TRUE))%>%
    mutate(time=case_when(
      obs=="1" ~ "08:50",obs=="2" ~ "08:50",
      obs=="3" ~ "09:50",obs=="4" ~ "09:50",
      obs=="5" ~ "10:50",obs=="6" ~ "10:50",
      obs=="7" ~ "11:50",obs=="8" ~ "11:50",
      obs=="9" ~ "12:50",obs=="10" ~ "12:50",
      obs=="11" ~ "13:50",obs=="12" ~ "13:50",
      obs=="13" ~ "14:50",obs=="14" ~ "14:50",
      obs=="15" ~ "15:50",obs=="16" ~ "15:50")))
(gqg.plot=gqg.dat%>%
    ggplot(aes(x =time, y=g_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    xlab("Time of day")+ylab(" ")+theme(legend.position = "none")+
    geom_errorbar(aes(
      ymin = g_avg - ifelse(Ex.int == "Intact", 0.01302309, 0.0181749), #Note I have calculated these separately as sd(qg.dat$A_avg[qg.dat$Ex.int=="Intact"]) and then with Excised.
      ymax = g_avg + ifelse(Ex.int == "Intact", 0.01302309, 0.0181749)),width = 0.2)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank()))

gy_limits <- range(c(gac.dat$g_avg, gbp.dat$g_avg, gcb.dat$g_avg, gpm.dat$g_avg, gqg.dat$g_avg))

# Update each plot with the common y-axis limits
gac.plot <- gac.plot + ylim(gy_limits)
gbp.plot <- gbp.plot + ylim(gy_limits)
gcb.plot <- gcb.plot + ylim(gy_limits)
gpm.plot <- gpm.plot + ylim(gy_limits)
gqg.plot <- gqg.plot + ylim(gy_limits)



(species.time=ggarrange(wac.plot,gac.plot,ac.plot,wbp.plot,gbp.plot,bp.plot,wcb.plot,gcb.plot,cb.plot,wpm.plot,gpm.plot,pm.plot,wqg.plot,gqg.plot,qg.plot,  ncol = 3, nrow = 5, common.legend = TRUE))

