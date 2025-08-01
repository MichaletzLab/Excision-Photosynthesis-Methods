#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**

full.exin.data$VPDleaf = as.numeric(full.exin.data$VPDleaf)
full.exin.data$Ca = as.numeric(full.exin.data$Ca)

full.exin.data=full.exin.data%>%
  mutate(carvpd=(1.6*A/(Ca/sqrt(VPDleaf))))

F=as.formula(y~x)
(medlyn.fig.int= full.exin.data%>%
    filter(carvpd>0)%>%
    filter(Ex.int=="Intact")%>%
    ggplot(aes(x =carvpd, y=gsw, color=species)) + geom_point(shape=18)+
    theme_classic()+theme(axis.text.x=element_text(size=15))+
    xlab(expression(1.6*A/(Ca*sqrt(VPD))))+ylab(expression('Stomatal conductance (mol / (m'^2*'s)'*''))+
    stat_smooth(aes(fill=species,color=species),method="lm",formula=F)+
    stat_regline_equation(label.x=c(0.006,0.006,0.006,0.006,0.006),label.y=c(0.225 ,0.208,0.192,0.175,0.16),aes(label =  paste(..eq.label.., sep = "~~~~")),formula=F,size=4)+
    theme(legend.position = "none")+
    scale_color_brewer(palette="Dark2",labels = c("Quercus garryana", "Pseudotsuga menziesii", "Carpinus betulus", "Betula papyrifera", "Acer campestre"))+scale_fill_brewer(palette="Dark2",labels = c("Quercus garryana", "Pseudotsuga menziesii", "Carpinus betulus", "Betula papyrifera", "Acer campestre")))

(medlyn.fig.ex= full.exin.data%>%
    filter(carvpd>0)%>%
    filter(Ex.int=="Excised")%>%
    ggplot(aes(x =carvpd, y=gsw, color=species)) + geom_point(shape=16)+
    theme_classic()+theme(axis.text.x=element_text(size=15))+
    xlab(expression(1.6*A/(Ca*sqrt(VPD))))+ylab(expression('Stomatal conductance (mol / (m'^2*'s)'*''))+
    stat_smooth(aes(fill=species,color=species),method="lm",formula=F)+
    stat_regline_equation(label.x=c(0.006,0.006,0.006,0.006,0.006),label.y=c(0.225 ,0.208,0.192,0.175,0.16),aes(label =  paste(..eq.label..)),formula=F,size=4)+
    theme(legend.position = "none")+
    scale_color_brewer(palette="Dark2",labels = c("Quercus garryana", "Pseudotsuga menziesii", "Carpinus betulus", "Betula papyrifera", "Acer campestre"))+scale_fill_brewer(palette="Dark2",labels = c("Quercus garryana", "Pseudotsuga menziesii", "Carpinus betulus", "Betula papyrifera", "Acer campestre")))

# Calculate the slopes, standard errors, and set Ex.int for Intact
slopes.int <- full.exin.data %>%
  filter(carvpd > 0) %>%
  filter(Ex.int == "Intact") %>%
  group_by(species) %>%
  do({
    model <- lm(gsw ~ carvpd, data = .)
    slope <- coef(model)[[2]]
    se_slope <- summary(model)$coefficients[2, "Std. Error"]
    ex.int <- "Intact"
    data.frame(slope = slope, se_slope = se_slope, ex.int = ex.int)
  })

# Calculate the slopes, standard errors, and set Ex.int for Excised
slopes.ex <- full.exin.data %>%
  filter(carvpd > 0) %>%
  filter(Ex.int == "Excised") %>%
  group_by(species) %>%
  do({
    model <- lm(gsw ~ carvpd, data = .)
    slope <- coef(model)[[2]]
    se_slope <- summary(model)$coefficients[2, "Std. Error"]
    ex.int <- "Excised"
    data.frame(slope = slope, se_slope = se_slope, ex.int = ex.int)
  })
combined_data = bind_rows(slopes.int,slopes.ex)

# Plot the graph with different colors and shapes for Intact and Excised, and species on the vertical axis
(g1.plot=(ggplot(combined_data, aes(x = species, y = slope, shape = ex.int, fill=species)) +
            geom_point(position = position_dodge(width = 0.75), size = 3) +
            geom_errorbar(aes(ymin = slope - se_slope, ymax = slope + se_slope),width = 0,
                          position = position_dodge(width = 0.75)) +
            theme_classic() +theme(axis.text.y = element_text(size = 12, face="italic"),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12),
                                   axis.title.y = element_text(size = 15),legend.title= element_blank(), legend.text = element_text(face="italic")) +
            ylab(expression('g'[1])) + xlab(" ") +
            scale_shape_manual(values = c(21,23)) +
            scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"),
                              labels = c("Quercus garryana", "Pseudotsuga menziesii", "Carpinus betulus", "Betula papyrifera", "Acer campestre")) +
            labs(fill = "species",shape = "Ex.int") +
            coord_flip()+guides(fill = guide_legend(override.aes = list(shape = c(22,22,22,22,22))))))

med.2=ggarrange(medlyn.fig.int,medlyn.fig.ex,labels=c("A","B"))
(medlyn.fig=ggarrange(med.2,g1.plot,widths=c(1.5,1),labels=c("","C")))

