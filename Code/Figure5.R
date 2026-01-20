#**First read in the three R scripts: Read.and.Modify.Data, Create.Delta.Data, and Stability_9.26.25.R**

full.exin.data$VPDleaf = as.numeric(full.exin.data$VPDleaf)
full.exin.data$Ca = as.numeric(full.exin.data$Ca)
full.exin.data$RHcham=as.numeric(full.exin.data$RHcham)
full.exin.data$Tleaf = as.numeric(full.exin.data$Tleaf)
full.exin.data$A = as.numeric(full.exin.data$A)

D = 1.6 #kPA calculated from 25C and 50% RH

full.exin.data=full.exin.data%>%
  mutate(carvpd=(1.6*A/(Ca/sqrt(D))))

F=as.formula(y~x)
medlyn.fig.int= full.exin.data%>%
    filter(carvpd>0)%>%
    filter(Ex.int=="Intact")%>%
    ggplot(aes(x =carvpd, y=gsw, color=species)) + geom_point(shape=18)+
    theme_classic()+theme(axis.text.x=element_text(size=15))+
    xlab(expression(1.6*A/(Ca*sqrt(D))))+ylab(expression("Stomatal conductance (mol m"^{-2}*" s"^{-1}*")"))+
    stat_smooth(aes(fill=species,color=species),method="lm",formula=F)+
    stat_regline_equation(label.x=c(0.006,0.006,0.006,0.006,0.006),label.y=c(0.225 ,0.208,0.192,0.175,0.16),aes(label =  paste(..eq.label.., sep = "~~~~")),formula=F,size=4)+
    theme(legend.position = "none")+
    scale_color_brewer(palette="Dark2",labels = c("Quercus garryana", "Pseudotsuga menziesii", "Carpinus betulus", "Betula papyrifera", "Acer campestre"))+scale_fill_brewer(palette="Dark2",labels = c("Quercus garryana", "Pseudotsuga menziesii", "Carpinus betulus", "Betula papyrifera", "Acer campestre"))

(medlyn.fig.ex= full.exin.data%>%
    filter(carvpd>0)%>%
    filter(Ex.int=="Excised")%>%
    ggplot(aes(x =carvpd, y=gsw, color=species)) + geom_point(shape=16)+
    theme_classic()+theme(axis.text.x=element_text(size=15))+
    xlab(expression(1.6*A/(Ca*sqrt(D))))+ylab(expression("Stomatal conductance (mol m"^{-2}*" s"^{-1}*")"))+
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
    p_value = summary(model)$coefficients[2, "Pr(>|t|)"]
    data.frame(slope = slope, se_slope = se_slope, ex.int = ex.int, p_value = p_value)
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
    p_value = summary(model)$coefficients[2, "Pr(>|t|)"]
    data.frame(slope = slope, se_slope = se_slope, ex.int = ex.int,p_value = p_value)
  })
combined_data = bind_rows(slopes.int,slopes.ex)

crit_val <- 1.96 
# Plot the graph with different colors and shapes for Intact and Excised, and species on the vertical axis
(g1.plot=(ggplot(combined_data, aes(x = species, y = slope-sqrt(D), shape = ex.int, fill=species)) +
            geom_point(position = position_dodge(width = 0.75), size = 3) +
            geom_errorbar(aes(ymin = slope-sqrt(D) - se_slope*crit_val, ymax = slope-sqrt(D) + se_slope*crit_val),width = 0,
                          position = position_dodge(width = 0.75)) +
            theme_classic() +theme(axis.text.y = element_text(size = 12, face="italic"),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12),
                                   axis.title.y = element_text(size = 15),legend.title= element_blank(), legend.text = element_text(face="italic")) +
            ylab(expression(g[1]~(kPa^0.5))) + xlab(" ") +
            scale_shape_manual(values = c(21,23)) +
            scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"),
                              labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
            labs(fill = "species",shape = "Ex.int") +
            coord_flip()+guides(fill = guide_legend(override.aes = list(shape = c(22,22,22,22,22))))))

med.2=ggarrange(medlyn.fig.int,medlyn.fig.ex,labels=c("A","B"))
(medlyn.fig=ggarrange(med.2,g1.plot,widths=c(1.5,1),labels=c("","C")))

g1_effect <- full.exin.data %>%
  filter(carvpd > 0) %>%
  group_by(species) %>%
  do({
    model <- lm(gsw ~ carvpd * Ex.int, data = .)
    p_val <- summary(model)$coefficients["carvpd:Ex.intIntact", "Pr(>|t|)"]
    data.frame(p_value_excision = p_val)
  })

g1_effect

