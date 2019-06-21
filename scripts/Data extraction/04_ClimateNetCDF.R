# install.packages('tidync')
library(tidync)
library(sf)
library(dplyr)

##Extraction climate date from nc4 format (D.Fisher and Bartlein)
# library(ncdf4)
# library(RColorBrewer)
# library(lattice)
# library(raster)
# library(rasterVis)
# library(proj4)
# library(chron)

## select where netCDF are located
# (since we're in an R project, we don't need to set the working directory)
#getwd()
#workdir <- "S:/Local-git/emilie_nlcaribou/Weather"
#setwd(workdir)

# Bounding box of 100% MCP
box <- readRDS('output/caribou-bbox.Rds')

daymetEPSG <- '+proj=lcc +datum=WGS84 +lat_1=25 n +lat_2=60n +lat_0=42.5n +lon_0=100w'


transBox <- st_bbox(st_transform(st_as_sfc(box) , daymetEPSG))

# set path and filename
ncname <- "Weather/daymet_v3_tmax_2008_na.nc4"

nc <- tidync(ncname) 

filtNC <- nc %>%
  hyper_filter(x = between(x, transBox[['xmin']], transBox[['xmax']]),
               y = between(y, transBox[['ymin']], transBox[['ymax']]))


readNC <- hyper_tibble(filtNC)


## (Just to check)
NL <- st_transform(st_read('input/NL-Bounds/NL-Bounds.shp'), daymetEPSG)

library(ggplot2)
library(data.table)
DT <- data.table(readNC)
ggplot(DT[time == min(time)]) + 
  geom_point(aes(x, y, color = tmax)) + 
  geom_sf(data = NL, alpha = 0.25)
###

#ncfname <- paste(ncname, ".nc4", sep = "")
#dname <- "prcp"  # note: tmp means temperature (not temporary)

# open a NetCDF file
ncin <- nc_open(ncfname)
print(ncin)


## get longitude and latitude 
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

tunits

gc()
###to do with lab computer (get a variable)
prcp_array <- ncvar_get(ncin,dname) ###size too big, not working
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(prcp_array)


###############################not working
# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

##close netCDF file
nc_close(ncin)
ls()

###Convert time variable
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

##Replace netCDF fillvalues with NA
tmp_array [tmp_array == fillvalue$value] <- NA

length(na.omit(as.vector(tmp_array[,,1]))) ##calculate number of non-missing values


# get a single slice or layer (January)
m <- 1
tmp_slice <- tmp_array [,,m]
# slice map
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))
##############################



###Test2
workdir <- "S:/Local-git/emilie_nlcaribou/Weather"
setwd(workdir)
year <- "2008"
ncname <- "prcp"
ncfname <- paste(ncname, year, ".nc4", sep = "")
ncin <- nc_open(ncfname, write = F)

x <- ncvar_get(ncin, "x")
y <- ncvar_get(ncin, "y")
yearday <- ncvar_get(ncin, "yearday")
nc_close(ncin)

##Process for one year
getNCDFClimate <- function (var, year) {
  ncfname <- paste(var, year, ".nc4", sep="")
  ncin <- nc_open(ncfname, write = T)
  var.array <- ncvar_get (ncin, var)
  nc_close(ncin)
  var.vec.long <- as.vector(var.array)
  return (var.vec.long)
}

vec.prcp <- getNCDFClimate("prcp", "2008")
vec.prcp <- getNCDFClimate("prcp", "2009")






