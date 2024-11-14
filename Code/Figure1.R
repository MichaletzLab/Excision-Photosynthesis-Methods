#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**

(qg.dat= full.exin.data%>%
  filter(species=="Q. garryana")%>%
  group_by(Ex.int, obs,time) %>%
  summarize(A_avg = mean(as.numeric(A))))
(qg.plot1=qg.dat%>%
    ggplot(aes(x =time, y=A_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    xlab("Time of day (hours)")+ylab(expression('Net photosynthetic rate (μmol m'^-2*'s'^-1*')'))+labs(color=NULL, title="Quercus garryana")+
    geom_errorbar(aes(
      ymin = A_avg - ifelse(Ex.int == "Intact", 1.27177, 1.866506),
      ymax = A_avg + ifelse(Ex.int == "Intact", 1.27177, 1.866506)),width = 0.2)+
    theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15),legend.text = element_text(size = 15), plot.title = element_text(face="italic"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)))

(wqg.dat= full.exin.data%>%
    filter(species=="Q. garryana")%>%
    group_by(Ex.int, obs,time) %>%
    summarize(mpa_avg = mean(calc.wp)))

(wqg.plot1=wqg.dat%>%
    ggplot(aes(x =time, y=mpa_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    geom_errorbar(aes(
      ymin = mpa_avg - ifelse(Ex.int == "Intact", 0.2014454, 0.06166861), 
      ymax = mpa_avg + ifelse(Ex.int == "Intact", 0.2014454, 0.06166861)),width = 0.2)+
    xlab("Time of day (hours)")+ylab("Water potential (MPa)")+labs(color=NULL)+
    theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15),legend.text = element_text(size = 15))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)))

(gqg.dat= full.exin.data%>%
    filter(species=="Q. garryana")%>%
    group_by(Ex.int, obs, time) %>%
    summarize(g_avg = mean(as.numeric(gsw))))

(gqg.plot1=gqg.dat%>%
    ggplot(aes(x =time, y=g_avg,color = Ex.int, group=Ex.int)) +    geom_line()+geom_point()+
    theme_bw()+ scale_color_manual(values=c("dodgerblue1", "deeppink4"),name="")+
    xlab("Time of day (hours)")+ylab(expression('Stomatal conductance (mol m'^-2*'s'^-1*')'))+
    geom_errorbar(aes(
      ymin = g_avg - ifelse(Ex.int == "Intact", 0.01302309, 0.0181749),
      ymax = g_avg + ifelse(Ex.int == "Intact", 0.01302309, 0.0181749)),width = 0.2)+
    theme(axis.text.x=element_text(size=12),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15),legend.text = element_text(size = 15))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),panel.border = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)))

(qg.time=ggarrange(qg.plot1,wqg.plot1,gqg.plot1,ncol=3,nrow=1,labels=c("H","I","J"), common.legend=TRUE,label.x = 0.11))