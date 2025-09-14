#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**

full.exin.data$A = as.numeric(full.exin.data$A)
full.exin.data$Measure.height = as.numeric(full.exin.data$Measure.height)
full.exin.data$Air.temp = as.numeric(full.exin.data$Air.temp)
full.exin.data$Water.pot = as.numeric(full.exin.data$Water.pot)
full.exin.data$gsw = as.numeric(full.exin.data$gsw)
full.exin.data$calc.wp = as.numeric(full.exin.data$calc.wp)

mixed2.modA <- lmer(A ~ (1|individual/branch) +Water.pot+ Measure.height+Air.temp+ Ex.int*species, data = full.exin.data)
(summary2_tableA <- summary(mixed2.modA))

mixed.modg <- lmer(gsw ~ (1|individual/branch) +Water.pot+ Air.temp+ Measure.height+ Ex.int*species, data = full.exin.data)
(summary_tableg <- summary(mixed.modg))

mixed.modwatpot <- lmer(calc.wp ~ (1|individual/branch) +gsw+ Air.temp+ Measure.height+ Ex.int*species, data = full.exin.data)
(summary_tablewatpot <- summary(mixed.modwatpot))


(fixed_effects_p_values.A <- summary2_tableA$coefficients[, "Pr(>|t|)"])
(contrasts <- emmeans(mixed2.modA, pairwise ~ Ex.int | species))

# Create a data frame with the provided contrast results for all individuals
# contrast_data.A <- data.frame(
#   species = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"),
#   estimate = c(-2.922,  -0.550, -0.806, 1.658, -6.473),
#   significant = c(TRUE, FALSE, FALSE, TRUE, TRUE), 
#   SE=c(0.580,0.576,0.588,0.522,0.690))

# Create a data frame with the provided contrast results for stability corrected data:
contrast_data.A <- data.frame(
  species = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"),
  estimate = c(-2.277,  -1.066, -0.989, 0.938, -6.314),
  significant = c(TRUE, TRUE, TRUE, TRUE, TRUE), #These all become TRUE when unstable individuals are removed
  SE=c(0.492,0.469,0.479,0.430,0.493))

(species.A<-ggplot(contrast_data.A, aes(x = species, y = estimate, fill = species, shape = species)) +
    geom_point(position = position_dodge(width = 0.75), size = 3) +
    geom_errorbar(
      aes(ymin = estimate - SE, ymax = estimate + SE),
      width = 0,
      position = position_dodge(width = 0.75)) +
    theme_classic() +
    theme(axis.text.y = element_text(face="italic"),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 15),legend.title= element_blank(),legend.text = element_text(face = "italic")) +
    ylab("Effect size for photosynthesis") + xlab(" ") +  
    scale_shape_manual(values = c(25,23,22,24,21), labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
    scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E"),labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
    geom_hline(yintercept=0, linetype="dashed")+
    labs(fill = "species", shape = "species") +
    coord_flip())

(species.A.plot=species.A + geom_text(aes(label = ifelse(significant, "*", "")), size = 6, vjust = 0, hjust = 0))

(fixed_effects_p_values.g <- summary_tableg$coefficients[, "Pr(>|t|)"])
(contrasts <- emmeans(mixed.modg, pairwise ~ Ex.int | species))

# # Create a data frame with the provided contrast results for full data
# contrast_data.g <- data.frame(
#   species = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"),
#   estimate = c(-0.02615, -0.00928, -0.01578, 0.00957, -0.05427),
#   significant = c(TRUE, FALSE, TRUE, TRUE, TRUE),
#   SE=c(0.00504,0.00501,0.00512,0.00448,0.00610))

# Create a data frame with the provided contrast results for stability corrected data
contrast_data.g <- data.frame(
  species = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"),
  estimate = c(-0.02338, -0.01788, -0.01474, 0.00849, -0.04924),
  significant = c(TRUE, TRUE, TRUE, FALSE, TRUE),
  SE=c(0.00572,0.00545,0.00557,0.00499,0.00693))

(species.g=(ggplot(contrast_data.g, aes(x = species, y = estimate, fill = species, shape = species)) +
              geom_point(position = position_dodge(width = 0.75), size = 3) +
              geom_errorbar(
                aes(ymin = estimate - SE, ymax = estimate + SE),
                width = 0,
                position = position_dodge(width = 0.75)
              ) +
              theme_classic() +
              theme(axis.text.y = element_text(face="italic"),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 9),axis.title.y = element_text(size = 15),legend.title= element_blank(),legend.text = element_text(face = "italic")) +
              ylab(expression('Effect size for stomatal conductance')) +  
              xlab(" ") + 
              scale_shape_manual(values = c(25,23,22,24,21),labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
              scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E"),labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
              geom_hline(yintercept=0, linetype="dashed")+
              labs(fill = "species", shape = "species") +
              coord_flip()))  # Flip the coordinates to make species vertical

(species.g.plot=species.g + geom_text(aes(label = ifelse(significant, "*", "")), size = 6, vjust = 0, hjust = 0))

(fixed_effects_p_values.watpot <- summary_tablewatpot$coefficients[, "Pr(>|t|)"])
(contrasts <- emmeans(mixed.modwatpot, pairwise ~ Ex.int | species))

# Create a data frame with the provided contrast results for full data
# contrast_data.watpot <- data.frame(
#   species = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"),
#   estimate = c(0.806,  0.794,0.849, 0.479, 1.260),
#   significant = c(TRUE, TRUE,TRUE, TRUE, TRUE),
#   SE=c(0.0698,0.0676,0.0682,0.0675,0.0778))

# Create a data frame with the provided contrast results for stability corrected data
contrast_data.watpot <- data.frame(
  species = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"),
  estimate = c(0.867,  0.743,0.805, 0.509, 1.307),
  significant = c(TRUE, TRUE,TRUE, TRUE, TRUE),
  SE=c(0.0779,0.0769,0.0763,0.0753,0.0866))

(species.watpot=(ggplot(contrast_data.watpot, aes(x = species, y = estimate, fill = species, shape = species)) +
                   geom_point(position = position_dodge(width = 0.75), size = 3) +
                   geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE),width = 0,position = position_dodge(width = 0.75)) +
                   theme_classic() +
                   theme(axis.text.y = element_text(size = 12, face="italic"),axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 10),axis.title.y = element_text(size = 15),legend.title= element_blank(),legend.text = element_text(face = "italic")) +
                   ylab("Effect size for water potential") + 
                   xlab(" ") +
                   scale_shape_manual(values = c(25,23,22,24,21),labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
                   scale_fill_manual(values = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E"),labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
                   geom_hline(yintercept=0, linetype="dashed")+
                   labs(fill = "species", shape = "species") +
                   coord_flip()))

(species.watpot.plot=species.watpot + geom_text(aes(label = ifelse(significant, "*", "")), size = 6, vjust = 0, hjust = 0))

(species.arranged=ggarrange(species.watpot.plot,species.g.plot,species.A.plot,nrow=1,ncol=3,labels=c("A","B","C"),common.legend=TRUE,widths = c(1.5, 1, 1)))
