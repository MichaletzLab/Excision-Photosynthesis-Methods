#**First read in the two R scripts: Read.and.Modify.Data and Create.Delta.Data**

(Aint.vs.Aex = full.exin.delta %>%
  filter(A.ex > 0) %>%
  filter(A.int > 0) %>%
  ggscatter(x = "A.int", y = "A.ex", color = "species", add = "reg.line", conf.int = TRUE) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(face = "italic")) +
  xlab(expression('Intact photosynthesis (μmol m'^-2*'s'^-1*')')) +
  ylab(expression('Excised photosynthesis (μmol m'^-2*'s'^-1*')')) +
  geom_abline(slope = 1, linetype = "dashed") +
  xlim(0, 20) +
  ylim(0, 20) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"), labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"), labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"))
)

(gint.vs.gex = full.exin.delta %>%
    ggscatter(x = "gs.int", y = "gs.ex", color = "species", add = "reg.line", conf.int = TRUE) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(face = "italic")) +
    xlab(expression('Intact stomatal conductance (mol m'^-2*'s'^-1*')')) +
    ylab(expression('Excised stomatal conductance (mol m'^-2*'s'^-1*')')) +
    geom_abline(slope = 1, linetype = "dashed") +
    xlim(0, 0.25) +
    ylim(0, 0.25) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"), labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"), labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"))
)

(wpint.vs.wpex = full.exin.delta %>%
    ggscatter(x = "watpot.int.mpa", y = "watpot.ex.mpa", color = "species", add = "reg.line", conf.int = TRUE) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), legend.text = element_text(face = "italic")) +
    xlab(expression('Intact water potential (MPa)')) +
    ylab(expression('Excised water potential (MPa)')) +
    geom_abline(slope = 1, linetype = "dashed") +
    xlim(-4, 0) +
    ylim(-4, 0) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"), labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana")) +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"), labels = c("Acer campestre", "Betula papyrifera", "Carpinus betulus", "Pseudotsuga menziesii", "Quercus garryana"))
)

(ex.int.arranged = ggarrange(wpint.vs.wpex, gint.vs.gex, Aint.vs.Aex, nrow = 1, ncol = 3, labels = c("A", "B", "C"), common.legend = TRUE))