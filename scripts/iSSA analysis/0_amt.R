gc()
rm(list = ls())

library(amt)
library(raster)
library(lubridate)
library(tidyverse) # not in paper
library(ggplot2)
library(parallel)
library(data.table)

####test with only 6 individuals
dat <- readRDS("~/Git/emilie_nlcaribou/output/Data extraction/cariboumodel.Rds") %>% 
  drop_na() %>%     #####filter !is.na did not work
  select(x = "EASTING", y = "NORTHING",
         t = "datetime", id = "ANIMAL_ID") %>% 
  filter(id %in% c("sc2006019", "sc2007036", "sc2007038", "mr2009a09", "mr2009a02", "sc2007090"))    # for example 2


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