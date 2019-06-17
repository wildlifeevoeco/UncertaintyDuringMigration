##Extraction climate date from nc4 format (D.Fisher and Bartlein)
library(ncdf4)
library(RColorBrewer)
library(lattice)
library(raster)
library(rasterVis)
library(proj4)
library(chron)

## select where netCDF are located
getwd()
workdir <- "~/Emilie_project/Git/emilie_nlcaribou/Climate"
setwd(workdir)

# set path and filename
ncname <- "daymet_v3_prcp_2006_na"
ncfname <- paste(ncname, ".nc4", sep = "")
dname <- "prcp"  # note: tmp means temperature (not temporary)

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
prcp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(prcp_array)

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
tmp_array[tmp_array==fillvalue$value] <- NA

length(na.omit(as.vector(tmp_array[,,1]))) ##calculate number of non-missing values


# get a single slice or layer (January)
m <- 1
tmp_slice <- tmp_array[,,m]
# slice map
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))





