######################
### Graphs for presentation ####
ggplot(predict_df %>% subset(group == -1), aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +theme_classic() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_y_continuous(limits = c(0,0.003))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(guide = 'none', name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(guide = 'none', name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("Relative probability of selection")


ggplot(predict_df %>% subset(group == 0 | group == -1), aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +theme_classic() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_y_continuous(limits = c(0,0.003))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(guide = 'none', name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(guide = 'none', name = "Temperature", values=c("blue", "orange"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("Relative probability of selection")

ggplot(predict_df, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +theme_classic() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)) +
  scale_y_continuous(limits = c(0,0.003))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(guide = 'none', name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(guide = 'none', name = "Temperature", values=c("blue", "orange", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("Relative probability of selection")



b <- ggplot(predict_df_forest_temp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +theme_classic() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_forest,
                     limits = c(min(scaled_breaks_forest), max(scaled_breaks_forest))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c( "blue", "orange", "red"),labels = unscaled_labels_temp) +
  xlab ("") + ylab("Relative probability of selection")

### Legend
ba <- b + theme(legend.position =  c("top"),
                legend.box = "horizontal",
                legend.title = element_text(size=20),
                legend.text = element_text(size=18))



###LICHEN RSF STOP
ggplot(predict_df_lichen_temp %>% subset(group == -1), aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3)  +theme_classic() +
  theme(legend.position = c("none"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_y_continuous(limits = c(0,0.0025))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c("blue", "#999999", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("Relative probability of selection")

gplot(predict_df_lichen_temp %>% subset(group == -1 | group == 0), aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3)  +theme_classic() +
  theme(legend.position = c("none"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_y_continuous(limits = c(0,0.0025))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c("blue", "orange", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("")

ggplot(predict_df_lichen_temp, aes(x, predicted, fill = group)) +
  geom_line(aes(linetype = group), size = 0.8) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3)  +theme_classic() +
  theme(legend.position = c("none"),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  scale_y_continuous(limits = c(0,0.0025))+
  scale_x_continuous(labels = unscaled_proportion_labels, breaks = scaled_breaks_lichen,
                     limits = c(min(scaled_breaks_lichen), max(scaled_breaks_lichen))) +
  scale_linetype_discrete(name = "Temperature", labels = unscaled_labels_temp)+
  scale_fill_manual(name = "Temperature", values=c("blue", "orange", "red"),labels = unscaled_labels_temp)+
  xlab ("") + ylab("")
