###~~~Extraction of NDVI data~~~###

#########  Extracting spatial data from rasters to points  ##############
library(raster)
library(data.table)
libs <- c('data.table', 'rgdal')
caribouData<-readRDS('~/Git/emilie_nlcaribou/output/Data extraction/cleaned-locs.Rds')  

### remove 276 Julian day 
caribouData <- caribouData[JDate >= 50 & JDate <= 275]
##### Now to extract NDVI, which has a unique layer for each day:

#### Subset down to only one year (this step needs to be done one year at a time)

Data2006<-subset(caribouData, year=="2006")

#### I first give it a point ID, to be able to re-sort it again later
Data2006$ptID<-c(1:nrow(Data2006))

#### Now I order the dataframe by Julian Day
DataSort2006<-Data2006[order(Data2006$JDate),]

#### Set the working directory to a folder with all the NDVI files for that year
#### (doesn't seem to work without setting the WD)
setwd('E:/NDVI/DailyNDVI_IRG/2013')
setwd('C:/Users/emitn/OneDrive/Documents/Stage/Stage Canada/NDVI/DailyNDVI_IRG/2006')

#### Use List files to get the names of the files
Files2013<-list.files('E:/NDVI/DailyNDVI_IRG/2013')
Files2006<-list.files('C:/Users/emitn/OneDrive/Documents/Stage/Stage Canada/NDVI/DailyNDVI_IRG/2006')

#### This then iterates the "raster" function to read in and make
#### a raster object for each day
NDVIrasters2006<-sapply(Files2006, raster)

#### Here's the loop to extract the data for each day. It subsets the data to only include day i,
#### and extracts the value of the raster which represents the data for day i.

#### This generates a vector of NDVI values for each day ("NDVI2013070", "NDVI2013071", "NDVI2013072", etc)
#### The first loop only loops over data until day 99
for (i in min(DataSort2006$JDate):99){
  assign(paste("NDVI2006",i,sep=""),extract(x=eval(parse(text=paste("NDVIrasters2006$Pred20060",i,".tif",sep=""))),
                                            y=data.frame(subset(DataSort2006,JDate==i)$EASTING,
                                                         subset(DataSort2006,JDate==i)$NORTHING)))
}

#### This code is the same but for day 100 +
#### These are different because the file names before 100 have an extra 0 before them so
#### they won't match up with i (e.g., 71 versus 071). That's why the code above is "Pred20130"
#### and the one below is "Pred2013"

for (i in 100:max(DataSort2006$JDate)){
  print(i)
  assign(paste("NDVI2006",i,sep=""),extract(x=eval(parse(text=paste("NDVIrasters2006$Pred2006",i,".tif",sep=""))),
                                            y=data.frame(subset(DataSort2006,JDate==i)$EASTING,
                                                         subset(DataSort2006,JDate==i)$NORTHING)))
}


#### This code takes all the individual vectors produced above and joins them into a
#### single vector. The first run of the loop creates an object with the first vector
#### (NDVIval2013), then all future times through the loop it combines the result
#### of the previous loop with the data from the next day:

t <- 1    
for(i in min(DataSort2006$JDate):max(DataSort2006$JDate)){
  if (t==1){
    NDVIval2006 <- eval(parse(text=paste("NDVI2006",i,sep="")))
  } else {
    NDVIval2006 <- c(NDVIval2006, eval(parse(text=paste("NDVI2006",i,sep=""))))
  }
  t <- t+1
}


#### Then this vector can be re-attached to the original dataframe as the vector
#### of NDVI values
DataSort2006$NDVI<-NDVIval2006

#### This can then be resorted by the point ID created earlier to put the data
#### back into the order it was in previously:
DataReSort2006<-DataSort2006[order(DataSort2006$ptID),]

##Calculate number of herd that I have by year (!!Midridge!! and year 2006!!)

length(unique(DataReSort2006$HERD))

###Replacing NAs with latest non-NA value (NDVI column)
library(dplyr)
library(data.table)
##Check if there is NA values
summary(DataReSort2013$NDVI)

fill.NAs<-function(x) {is_na<-is.na(x); x[Reduce(function(i,j) if (is_na[j]) i else j, seq_len(length(x)), accumulate=T)]}
DataReSort2013NA<-fill.NAs(c(DataReSort2013$NDVI))

DataReSort2013$NDVI<-DataReSort2013NA
summary(DataReSort2013$NDVI)  ##0 NA

##Save NDVI data for each year 
saveRDS(DataReSort2013, '~/Git/emilie_nlcaribou/output/Data extraction/NDVI/NDVI_NA/DataReSort2013.Rds')
save(DataReSort2013, file="~/Git/emilie_nlcaribou/output/Data extraction/NDVI/NDVI_NA/DataReSort2013.csv")

glimpse(out)
out$state<-as.factor(out$state)
out[, mean(step), by =.(state)]
step1<-subset(out, state == "1")
mean(step1$angle, na = TRUE)
step2<-subset(out,state == "2")
mean(step2$angle, na = TRUE)
Data2006<-subset(caribouData, year=="2006")

?bind_rows
?cbind
allNDVI<-rbind(DataReSort2006, DataReSort2007, DataReSort2008, DataReSort2009, DataReSort2010, DataReSort2011, DataReSort2012, DataReSort2013)
