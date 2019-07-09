gc()
rm(list = ls())

## Load packages ##

libs <- c('raster', 'lubridate', 'amt', 'data.table', 'tidyr', 'ggplot2')
lapply(libs, require, character.only=TRUE)

## Landcover raster 
Filesraster<-list.files('~/Emilie_project/Landcover')
Conifer<-raster("Conifer100.tif")
MixedWood<-raster("MixedWood100.tif")
Anthro<-raster("Anthro100.tif")
Broadleaf<-raster("Broadleaf100.tif")
Lichen<-raster("Lichen100.tif")
Rocky<-raster("Rocky100.tif")
Water<-raster("Water100.tif")
Wetland<-raster("Wetland100.tif")

Forest<-Conifer+Broadleaf+MixedWood

##First test
caribouclean <- readRDS("~/Documents/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.Rds")
cariboucleanMR <- cariboucleanMR %>% filter(Year=='2011') 
datem <- cariboucleanMR %>% 
  drop_na() %>%     #####filter !is.na did not work
  select(x = "Easting", y = "Northing",
         t = "Time", id = "Animal_ID") 

####test with only 1 individuals  ##only using caribouclean data
#datem <- readRDS("~/Documents/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.Rds") %>% 
#  drop_na() %>%     #####filter !is.na did not work
#  select(x = "Easting", y = "Northing",
#         t = "Time", id = "Animal_ID") 

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

dat_1_em <- datem %>% filter(id=='mr2009a25') 

head(dat_1_em)
#### Make a track with time and locations
# make datetime column POSIX
dat_1_em$t <- as.POSIXct(dat_1_em$t, format="%Y-%m-%d %H:%M:%S", tz='GMT')
#dat_1_em <- dat_1_em[dat_1_em$t >= "2011-01-01" & dat_1_em$t <= "2011-12-31",]

dat_1_em <- drop_na(dat_1_em)


# make track
dat_1_em <- mk_track(dat_1_em, .x=x, .y=y, .t=t, crs = sp::CRS("+init=epsg:5070"), order_by_ts=TRUE, check_duplicates = TRUE)
  
##Resampling track 
summarize_sampling_rate(dat_1_em)  ##pb with the results I think
dat_1_em <- dat_1_em[order(dat_1_em$t_),]

stps <- amt::track_resample(dat_1_em, rate=hours(5), tolerance=minutes(10)) %>%
  filter_min_n_burst(min_n=3) %>% steps_by_burst() %>%
  time_of_day(include.crepuscule=FALSE)

all(diff(dat_1_em$x_) >= 0)   ##TRUE
all(diff(dat_1_em$y_) >= 0)   ##FALSE
all(diff(dat_1_em$t_) >= 0)   ##FALSE





#### Test May 2019 
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