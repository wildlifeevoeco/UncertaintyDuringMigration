gc()
rm(list = ls())

###Load packages
libs <- c('raster','lubridate','amt','data.table','tidyr','ggplot2')
lapply(libs, require, character.only = TRUE)

###Load spatial data
Filesraster<-list.files('C:/Users/emitn/OneDrive/Documents/Stage/Stage Canada/landcover/tifraster')
Conifer<-raster("Conifer100.tif")
MixedWood<-raster("MixedWood100.tif")
Anthro<-raster("Anthro100.tif")
Broadleaf<-raster("Broadleaf100.tif")
Lichen<-raster("Lichen100.tif")
Rocky<-raster("Rocky100.tif")
Water<-raster("Water100.tif")
Wetland<-raster("Wetland100.tif")
Forest<-Conifer+Broadleaf+MixedWood

####test with only 6 individuals
#dat <- readRDS("~/Git/emilie_nlcaribou/output/Data extraction/cariboumodel.Rds") %>% 
 # drop_na() %>%     #####filter !is.na did not work
#  select(x = "EASTING", y = "NORTHING",
 #        t = "datetime", id = "ANIMAL_ID") %>% 
#  filter(id %in% c("sc2006019", "sc2007036", "sc2007038", "mr2009a09", "mr2009a02", "sc2007090"))    # for example 2

dat <- readRDS("~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.Rds")

caribouclean2010 <- caribouclean %>% filter(Year=='2010')
saveRDS(caribouclean2010, '~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean2010.Rds')

utm21N <- '+proj=utm +zone21 ellps=WGS84'


# load the data
dat <- readRDS('~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean2010.Rds') %>% 
  # remove NAs
  filter(!is.na('X')) %>%
  select(x='Easting', y='Northing',
         t = 'Time', id = 'Animal_ID')
# subset out mr2009a02
dat_1 <- dat %>% filter(id=='mr2009a03') 


#### 4 - Make a track with time and locations ----

# make datetime column POSIX
dat_1$t <- as.POSIXct(dat_1$t)

# make track
dat_1 <- mk_track(dat_1, .x=x, .y=y, .t=t, crs = sp::CRS("+init=epsg:5070"))

#### 5 - Resample track ----

# check sampling rate to look for spread of resampling rates
summarize_sampling_rate(dat_1)  
# 

stps <- amt::track_resample(dat_1$t_, rate = hours(5), tolerance = minutes(10)) %>%
  filter_min_n_burst(min_n = 3) %>% steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE)

#check if vector is ascending/descending
all(diff(dat_1$x_) >= 0) #FALSE
all(diff(dat_1$y_) >= 0) #FALSE
all(diff(dat_1$t_) >= 0) #TRUE
 
# look at structure of steps:
# x1_-x2_ = start and end of steps; t1_-t2_ = time of start and end of step; sl = step length;
# ta = turn angle (DEGREES); dt_ = difference in time between steps; burst_ = burst to which step belongs
str(stps)



##Katrien code
CreateTrack <- function(x.col, y.col, date.col, crs, ID, NumbRandSteps, sl_distr, ta_distr) {
  trk <- track (x.col, y.col, date.col, ID, crs) %>%
  steps ()
  trk$dt <- difftime (trk$t2_, trk$t1_, unit ='hours')
  trk <- subset (trk, trk$dt_ > 1.9 & trk$dt_ < 2.1, drop = T)
  trk %>%
    random_steps(n = NumbRandSteps, sl_distr, ta_distr)
}

CTracks <- caribouclean[, CreateTrack(10, x.col = Easting, y.col = Northing, date.col = ts, ID = Animal_ID, crs = utm21N,
                                      sl_distr = "gamma", ta_distr = "vonmises"),
                        by = Animal_ID]

test2<-dat    
#####Change formats 
test2$id<-as.character(test2$id)
test2$t<- as.POSIXct(test2$t, format = "%Y-%m-%d %H:%M:%OS")       ######After this, 7 NA appears in the column t
test2<-na.omit(test2, cols = "t", invert = FALSE)    

####split data#############################
test2$Date<-as.Date(test2$t)
test2$Time<-format(test2$t, format = "%H:%M:%S")

out<-strsplit(as.character(test2$t), ' ')
do.call(rbind, out)
head(out)
data.frame(test2$new, do.call(rbind, out))
###########################################

test2 <- test2 %>% nest(-id)
test2 <- test2 %>%
  mutate(trk = lapply(data, function(d) {
    mk_track(d, x, y, t, crs = sp::CRS("+init=epsg:4326")) %>%
      transform_coords(sp::CRS("+init=epsg:5070"))
  }))




##Number of relocations with 12h fix rate
###Number of individuals each year, number of herd
##Period of study