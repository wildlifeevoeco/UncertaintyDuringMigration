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


####test with only 1 individuals
dat <- readRDS("~/Emilie_project/Git/emilie_nlcaribou/output/caribouclean.Rds") %>% 
  drop_na() %>%     #####filter !is.na did not work
  select(x = "Easting", y = "Northing",
         t = "Time", id = "Animal_ID") 

dat[, difTime := as.numeric(difftime(ts, shift(ts), unit = 'mins')), by = "id"]

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

CreateTrack <- function(x.col, y.col, date.col, crs, ID, NumbRandSteps, sl_distr, ta_distr) {
  #print(ID)
  #create track from dataset
  trk <- track(x.col, y.col, date.col, ID, crs) %>% 
    #function turns locs into steps
    steps() 
  #remove any steps that span more than 2hr
  trk$dt_ <- difftime(trk$t2_, trk$t1_, unit='hours')
  trk <- subset(trk, trk$dt_ > 1.9 & trk$dt_ < 2.1, drop = T)
  #generate random steps
  trk %>%
    random_steps(n = NumbRandSteps, sl_distr, ta_distr)
}

CTracks <- caribouclean[, CreateTrack(10, x.col = Easting, y.col = Northing, date.col = Time, ID = Animal_ID, crs = utm21N,
                               sl_distr = "gamma", ta_distr = "vonmises"), 
                 by = Animal_ID]



dat_1 <- dat %>% filter(id=='mr2009a02') 

#### Make a track with time and locations
# make datetime column POSIX
dat_1$t <- as.POSIXct(dat_1$t)
# make track
dat_1 <- mk_track(dat_1, .x=x, .y=y, .t=t, crs = sp::CRS("+init=epsg:5070"))


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