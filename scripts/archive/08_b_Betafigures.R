###################Beta coefficient figures######
library(ggplot2)
###load the files with B coeff and CI 

beta_coefficient$Variables <- as.factor(beta_coefficient$Variables)
beta_coefficient_RSF$Variables <- as.factor(beta_coefficient_RSF$Variables)
beta_coefficient_RSF$Model <- as.factor(beta_coefficient_RSF$Model)

####plot
ggplot(beta_coefficient, aes(x= Variables, y= b)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = 0), linetype = 2, colour = 'black') +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), 
                colour="black", width = 0,
                position = position_dodge(width = 0.5)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  labs(y = "Selection", x = "Covariates") +
  theme(panel.background =element_rect(colour = "black", fill= "transparent", size = 1) ###do it this way
        , plot.background = element_rect(fill = "transparent", colour = NA)
        , panel.grid.major = element_blank() # get rid of major grid
        , panel.grid.minor = element_blank() # get rid of minor grid
        , legend.background = element_rect(fill = "transparent") # get rid of legend bg
        , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        , axis.line = element_line(colour = "black")
        , axis.text.y = element_text(color="black", size=24)
        , axis.text.x = element_text(color="black", size=18, angle = 45, hjust = 1)
        , text = element_text(size=20))

beta_coefficient$Variables <- factor(beta_coefficient$Variables, levels = c("Intercept","NDVI:Temperature","Temperature:Rocky","Temperature:Lichen", "Precipitation:Forest", "Precipitation:Wetland", "NDVI:Rocky", "NDVI:Wetland",
                          "NDVI:Lichen","Precipitation","Temperature","Water","Rocky","Forest",
                          "Wetland","Lichen","NDVI"))

ggplot(beta_coefficient, aes(Variables, b)) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "#00AFBB") +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), 
                colour="black", width = 0,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  ylim(-0.25,0.25) +
  xlab("") +
  ylab("Beta coefficient (+/- 95% CI)") +
  coord_flip()+
  theme(legend.position = c(0.55,0.85),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#00AFBB"),
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(vjust=10,size=14),
        axis.text = element_text(size=12),
        strip.text  = element_text(size = 16),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


 ####RSF plot
beta_coefficient_RSF$Variables <- factor(beta_coefficient_RSF$Variables, levels = c("Constant","NDVI:Temperature","Temperature:Forest","Temperature:Rocky","Temperature:Wetland","Temperature:Lichen",
                                                                                    "Precipitation:Rocky","Precipitation:Wetland","Precipitation:Forest","SWE:Precipitation",
                                                                                    "SWE:Rocky","SWE:Wetland","SWE:Lichen","SWE","Precipitation","Temperature","Water","Rocky",
                                                                                    "Forest","Wetland","Lichen","NDVI"))

ggplot(beta_coefficient_RSF, aes(Variables, b, shape = Model, group = Model)) +
  geom_point(aes(color = Model), position = position_dodge(width = 0.5), size = 4.5) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), 
                colour="black", width = 0,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  ylim(-0.6,1.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  xlab("") +
  ylab("Beta coefficient (+/- 95% CI)") +
  coord_flip()+
  theme(legend.position = c(0.85,0.1),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = alpha('white', 1)),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text  = element_text(size = 16),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))


