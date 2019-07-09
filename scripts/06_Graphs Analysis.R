###First analysis 
plot(obsDaymet$swe, y = NULL)
?plot
library(ggplot2)
par(mar=c(4,4,1,1))
ggplot(obsDaymet, aes(swe)) +
  geom_histogram(binwidth = 5) 

encamp <- subset(Observed, state == '1')
mov <- subset (Observed, state == '2')
mean(encamp$angle, na.rm = TRUE)
mean(mov$angle, na.rm = TRUE)

library(measurements)
encamp$step <- conv_unit(encamp$step, "m", "km")
Obs <- Observed
Obs$step <- conv_unit(Obs$step, "m", "km")
ggplot(encamp, aes(step)) +
  geom_histogram() 

Obs$step <- subset(Obs, step <= 10)
ggplot(Obs,aes(step), fill = state)) +
  geom_histogram(aes(color = state))

mean(obsDaymet$tmin)
temp <- obsDaymet %>%
  group_by(Year)%>%
  summarise(mean_min_temp = mean(tmin),
            mean_max_temp = mean(tmax),
            mean_swe = mean(swe),
            mean_prcp = mean(prcp))
temp$mean_min_temp <- round(temp$mean_min_temp, digits = 1)

mean(obsDaymet$tmax)
is.na(obsDaymet$tmin)

mr2009a25 <- Observed %>% filter(Animal_ID=='mr2009a25') 
mr2009a252011 <- mr2009a25 %>% filter(Year=='2011') 
write.csv(mr2009a252011,'~/Documents/Emilie_project/Git/emilie_nlcaribou/output/mr2009a252011.csv')


mr2009a15 <- dataDisp %>% filter(ANIMAL_ID=='mr2009a15') 
mr2009a152010 <- mr2009a15 %>% filter(year=='2010') 
write.csv(mr2009a152010,'~/Documents/Emilie_project/Git/emilie_nlcaribou/output/mr2009a152010.csv')
save(mr2009a152010, file="~/Documents/Emilie_project/Git/emilie_nlcaribou/output/mr2009a152010.csv")
write.csv(Observed,'~/Documents/Emilie_project/Git/emilie_nlcaribou/output/RandomPoints/Observed.csv')


library(raster)
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
lc <- shapefile("~/Documents/Emilie_project/Git/GIS/NL-Bounds.shp")

ggplot(lc) +
  geom_polygon(aes(long,lat, group = group)) +
  geom_point(data = Observed,
             aes (Easting, Northing, color = Animal_ID)) +
  theme (legend.position = 'none',
         plot.title = element_text(size = 20))


randomtest <- subset(randomtest, Animal_ID == "mr2012a01")
unique(randomtest$state)
aggregate(randomtest$state, randomtest[,c("state")], length)
randomtest$state <- as.factor(randomtest$state)
summary(randomtest$state)

indiv <- allNDVI %>%              ###some indiv move more than 30km but in 2 days for ex.
  group_by(Animal_ID, Year)%>%
  summarise(fixes = n(),
            numday = uniqueN(FixDate),
            Distancemig = unique(Displace))
randomtest <- subset(allNDVI, Randoms == 1)
randomtest <- subset(randomtest, Animal_ID == "mr2012a01")
randomtest$state <- as.factor(randomtest$state)

ggplot(randomtest, aes(x = state, y = NDVI)) +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()

###create new dataframe with two states in columns and ndvi values
Ndvitest <- randomtest[,TimeIDYear := paste(Time, IDYear, sep = '_')]
Obsall = subset(Obsall, select = c(9:10,21,23,27))

ndvitest <- randomtest %>%
  group_by(Time, state, Animal_ID) %>% 
  summarise(NDVI = mean(NDVI))


cols <- c("NDVI", "state")

new <- setDT(ndvitest)[, ..cols]

dcast(new, NDVI ~ state)

t.test(ndvitest$state, ndvitest$NDVI)

testndvi <- dcast (randomtest,)

ttest1 <- subset(ndvitest, state == 1)
ttest2 <- subset (ndvitest, state == 2)

shapiro.test(ttest1$NDVI)
shapiro.test(ttest2$NDVI)
t.test(ttest1$NDVI, ttest2$NDVI)

a1 <- lme4::lmer(NDVI ~ state + Year + (1|Animal_ID), data = allNDVI[Randoms == 1]) 
PQL <- nlme::glmmPQL(NDVI ~ state + Year + (1|Animal_ID), data = allNDVI[Randoms == 1], verbose = FALSE)
