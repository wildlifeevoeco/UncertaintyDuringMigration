###~~~Extraction of NDVI data~~~###

#########  Extracting spatial data from rasters to points  ##############
library(raster)
library(data.table)
library(dplyr)

libs <- c('data.table', 'rgdal', 'dplyr')

caribouData<-readRDS('~/Git/emilie_nlcaribou/output/Data extraction/out.Rds')  
caribouData<-out

### remove 276 Julian day 
caribouData <- caribouData[JDate >= 50 & JDate <= 275]
##### Now to extract NDVI, which has a unique layer for each day:

#### Subset down to only one year (this step needs to be done one year at a time)

Data2013<-subset(caribouData, year=="2013")

#### I first give it a point ID, to be able to re-sort it again later
Data2013$ptID<-c(1:nrow(Data2013))

#### Now I order the dataframe by Julian Day
DataSort2013<-Data2013[order(Data2013$JDate),]

#### Set the working directory to a folder with all the NDVI files for that year
#### (doesn't seem to work without setting the WD)
setwd('C:/Users/emitn/OneDrive/Documents/Stage/Stage Canada/NDVI/DailyNDVI_IRG/2013')

#### Use List files to get the names of the files
Files2013<-list.files('C:/Users/emitn/OneDrive/Documents/Stage/Stage Canada/NDVI/DailyNDVI_IRG/2013')

#### This then iterates the "raster" function to read in and make
#### a raster object for each day
NDVIrasters2013<-sapply(Files2013, raster)

#### Here's the loop to extract the data for each day. It subsets the data to only include day i,
#### and extracts the value of the raster which represents the data for day i.

#### This generates a vector of NDVI values for each day ("NDVI2013070", "NDVI2013071", "NDVI2013072", etc)
#### The first loop only loops over data until day 99
for (i in min(DataSort2013$JDate):99){
  assign(paste("NDVI2013",i,sep=""),extract(x=eval(parse(text=paste("NDVIrasters2013$Pred20130",i,".tif",sep=""))),
                                            y=data.frame(subset(DataSort2013,JDate==i)$EASTING,
                                                         subset(DataSort2013,JDate==i)$NORTHING)))
}

#### This code is the same but for day 100 +
#### These are different because the file names before 100 have an extra 0 before them so
#### they won't match up with i (e.g., 71 versus 071). That's why the code above is "Pred20130"
#### and the one below is "Pred2013"

for (i in 100:max(DataSort2013$JDate)){
  print(i)
  assign(paste("NDVI2013",i,sep=""),extract(x=eval(parse(text=paste("NDVIrasters2013$Pred2013",i,".tif",sep=""))),
                                            y=data.frame(subset(DataSort2013,JDate==i)$EASTING,
                                                         subset(DataSort2013,JDate==i)$NORTHING)))
}


#### This code takes all the individual vectors produced above and joins them into a
#### single vector. The first run of the loop creates an object with the first vector
#### (NDVIval2013), then all future times through the loop it combines the result
#### of the previous loop with the data from the next day:

t <- 1    
for(i in min(DataSort2013$JDate):max(DataSort2013$JDate)){
  if (t==1){
    NDVIval2013 <- eval(parse(text=paste("NDVI2013",i,sep="")))
  } else {
    NDVIval2013 <- c(NDVIval2013, eval(parse(text=paste("NDVI2013",i,sep=""))))
  }
  t <- t+1
}


#### Then this vector can be re-attached to the original dataframe as the vector
#### of NDVI values
DataSort2013$NDVI<-NDVIval2013

#### This can then be resorted by the point ID created earlier to put the data
#### back into the order it was in previously:
DataReSort2013<-DataSort2013[order(DataSort2013$ptID),]

##Calculate number of herd that I have by year (!!Midridge!! and year 2006!!)

length(unique(DataReSort2013$HERD))

##Check if there is NA values
summary(DataReSort2013$NDVI)

###remove NA from NDVI column
DataReSort2013<-na.omit(DataReSort2013, cols = "NDVI", invert = FALSE)

###Replacing NAs with latest non-NA value (NDVI column)
fill.NAs<-function(x) {is_na<-is.na(x); x[Reduce(function(i,j) if (is_na[j]) i else j, seq_len(length(x)), accumulate=T)]}
DataReSort2013NA<-fill.NAs(c(DataReSort2013$NDVI))

DataReSort2013$NDVI<-DataReSort2013NA
summary(DataReSort2013$NDVI)  ##0 NA

##Save NDVI data for each year 
saveRDS(DataReSort2013, '~/Git/emilie_nlcaribou/output/Data extraction/NDVI/NDVI_NA/DataReSort2013.Rds')
save(DataReSort2013, file="~/Git/emilie_nlcaribou/output/Data extraction/NDVI/NDVI_NA/DataReSort2013.csv")

##combine all tabs
allNDVI<-rbind(DataReSort2006, DataReSort2007, DataReSort2008, DataReSort2009, DataReSort2010, DataReSort2011, DataReSort2012, DataReSort2013)

saveRDS(allNDVI, '~/Git/emilie_nlcaribou/output/Data extraction/NDVI/NDVI_NA/allNDVI.Rds')

###know which rows is duplicate
allNDVI$ANIMAL_ID[duplicated(caribouData$ANIMAL_ID)]
allNDVI<-allNDVI[!duplicated(allNDVI$V1), ]

