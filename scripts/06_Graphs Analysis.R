###First analysis 
plot(obsDaymet$swe, y = NULL)
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
ggplot(Obs,aes(step), fill = state) +
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
###############################
library(ggplot2)
####Plot variables/time
allNDVIobs$Year<-as.factor(allNDVIobs$Year)
all2010<-subset(allNDVIobs, Year == "2010")
all2011<-subset(allNDVIobs, Year == "2011")
all2012<-subset(allNDVIobs, Year == "2012")
all2013<-subset(allNDVIobs, Year == "2013")
####Temperature
tmax1<- ggplot(all2010, aes(x=JDate, y=tmax, colour=as.factor(state), group = as.factor(state))) +
geom_smooth(span = 0.3, method = 'gam', formula = y ~ s(x, bs = "cs"))+
  theme_bw() +
  labs(title = "Year 2010",color = "State")+
  theme(legend.position = c(0.7, 0.2),
    axis.title.y = element_text(size = 12, color = 'black'),
    axis.text = element_text(size = 10, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 1))

tmax2<- ggplot(all2011, aes(x=JDate, y=tmax, colour=as.factor(state), group = as.factor(state))) +
  geom_smooth(span = 0.3, method = 'gam', formula = y ~ s(x, bs = "cs"))+
  theme_bw() +
  labs(title = "Year 2011", color = "State")+
  theme(legend.position = c(0.8, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))

tmax3<- ggplot(all2012, aes(x=JDate, y=tmax, colour=as.factor(state), group = as.factor(state))) +
  geom_smooth(span = 0.3, method = 'gam', formula = y ~ s(x, bs = "cs"))+
  theme_bw() +
  labs(title = "Year 2012",color = "State")+
  theme(legend.position = c(0.7, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))

tmax4<- ggplot(all2013, aes(x=JDate, y=tmax, colour=as.factor(state), group = as.factor(state))) +
  geom_smooth(span = 0.3, method = 'gam', formula = y ~ s(x, bs = "cs"))+
  theme_bw() +
  labs(title = "Year 2013", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))
multiplot(tmax1,tmax2,tmax3,tmax4, cols = 2) 
####density temperature pts by state,year

density1<- ggplot(all2010, aes(x=tmax, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2010", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))
density2<- ggplot(all2011, aes(x=tmax, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2011", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))
density3<- ggplot(all2012, aes(x=tmax, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2012", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))
density4<- ggplot(all2013, aes(x=tmax, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2013", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))

multiplot(density1,density2,density3,density4, cols = 2) 
   
###swe
swe1<- ggplot(all2010, aes(x=swe, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2010", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))

swe2<- ggplot(all2011, aes(x=swe, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2011", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))

swe3<- ggplot(all2012, aes(x=swe, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2012", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))
swe4<- ggplot(all2013, aes(x=swe, colour=as.factor(state), group = as.factor(state))) +
  geom_density()+
  theme_bw() +
  labs(title = "Year 2013", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))
multiplot(swe1,swe2,swe3,swe4, cols = 2) 

swe1<-ggplot(all2010, aes(x=JDate, y=swe, colour=as.factor(state), group = as.factor(state))) +
  geom_smooth(span = 0.3, method = 'gam', formula = y ~ s(x, bs = "cs"))+
  theme_bw() +
  labs(color = "State")+
  theme(legend.position = c(0.7, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))

p2 <- ggplot() + geom_smooth(aes(y = swe, x = JDate, fill = state), data = all2010)
p2

swe2<-ggplot() +
  geom_smooth(aes(y=swe, x= JDate, fill = state), data = all2010, stat="identity",alpha = .5)+
  theme_bw() +
  labs(color = "State")+
  theme(legend.position = c(0.7, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))


###prcp
prcp1<- ggplot(all2011, aes(x=JDate, y=prcp, colour=as.factor(state), group = as.factor(state))) +
  geom_smooth()+
  theme_bw() +
  labs(title = "Year 2010", color = "State")+
  theme(legend.position = c(0.9, 0.2),
        axis.title.y = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 10, color = 'black'),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = 'black',
          fill = NA,
          size = 1))

coord_cartesian(ylim = c(0,25))
####
dftmin <- summarySEwithin(allNDVIobs, measurevar = "tmin", idvar = "Animal_ID",
                        withinvars = "state", "Year", na.rm = FALSE, conf.interval = .95)
plotmin<-ggplot(dftmin, aes(x=Year, y=tmin, colour=state, group = state)) +
  geom_line() +
  geom_point(shape=20, size=3, fill="white") +
  ylim(-5,-2)+
  theme_bw()

###calculate mean tmax by states 
library(data.table)
library(Rmisc)
meanyear2011<-subset(allNDVIobs, Year == 2011)
allNDVIobs$Year<-as.factor(allNDVIobs$Year)
dfwc <- summarySEwithin(allNDVIobs, measurevar = "tmax", idvar = "Animal_ID",
                            withinvars = "state", "Year", na.rm = FALSE, conf.interval = .95)

p1<-ggplot(dfwc, aes(x=Year, y=tmax, colour=state, group = state)) +
  geom_line() +
  geom_errorbar(width=0.2, aes(ymin=tmax-sd, ymax=tmax+sd)) +
  geom_point(shape=20, size=3, fill="white") +
  theme_bw()
ylim(0,8)+
meanyear2012<-subset(allNDVIobs, Year == 2012)
dfwc2012 <- summarySEwithin(meanyear2012, measurevar = "tmax", idvar = "Animal_ID",
                            withinvars = "state", na.rm = FALSE, conf.interval = .95)

p2<-ggplot(dfwc2012, aes(x=state, y=tmax)) +
  geom_line(aes(group=1)) +
  geom_errorbar(width=0.2, aes(ymin=tmax-ci, ymax=tmax+ci)) +
  geom_point(shape=20, size=3, fill="white") +
  ylim(1,7.5)+
  theme_bw()

meanyear2012<-subset(allNDVIobs, Year == 2012)
dfwc2012 <- summarySEwithin(meanyear2012, measurevar = "tmax", idvar = "Animal_ID",
                            withinvars = "state", na.rm = FALSE, conf.interval = .95)

p2<-ggplot(dfwc2012, aes(x=state, y=tmax)) +
  geom_line(aes(group=1)) +
  geom_errorbar(width=0.2, aes(ymin=tmax-ci, ymax=tmax+ci)) +
  geom_point(shape=20, size=3, fill="white") +
  ylim(1,7.5)+
  theme_bw()

meanyear2013<-subset(allNDVIobs, Year == 2013)
dfwc2013 <- summarySEwithin(meanyear2013, measurevar = "tmax", idvar = "Animal_ID",
                            withinvars = "state", na.rm = FALSE, conf.interval = .95)

p3<-ggplot(dfwc2013, aes(x=state, y=tmax)) +
  geom_line(aes(group=1)) +
  geom_errorbar(width=0.2, aes(ymin=tmax-ci, ymax=tmax+ci)) +
  geom_point(shape=20, size=3, fill="white") +
  ylim(1,7.5)+
  theme_bw()

meanyear2010<-subset(allNDVIobs, Year == 2010)
dfwc2010 <- summarySEwithin(meanyear2010, measurevar = "tmax", idvar = "Animal_ID",
                            withinvars = "state", na.rm = FALSE, conf.interval = .95)

p4<-ggplot(dfwc2010, aes(x=state, y=tmax)) +
  geom_line(aes(group=1)) +
  geom_errorbar(width=0.2, aes(ymin=tmax-ci, ymax=tmax+ci)) +
  geom_point(shape=20, size=3, fill="white") +
  ylim(1,7.5)+
  theme_bw()
multiplot(p2,p3,p4, cols = 2)



