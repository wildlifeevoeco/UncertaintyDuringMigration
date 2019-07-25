library(raster)

rasterOptions(tmpdir = 'output/temp-files')
Filesraster<-list.files('S:/Local-git/ewc/input/covariates/NL')

####load habitat raster
Bro<-raster('Broadleaf100.tif')
Con<-raster('Conifer100.tif')
Lic<-raster('Lichen100.tif')
Mix<-raster('MixedWood100.tif')
Roc<-raster('Rocky100.tif')
Wat<-raster('Water100.tif')
Wet<-raster('Wetland100.tif')
Forest<-Bro+Con+Mix

###Combine poly + layer to only extract values and run RSF into this polygon of MiddleRidge herd
##need to run mcp code before
cropBroadleaf <-crop(Bro,poly)
cropConifer <-crop(Con,poly)
cropMixedWood <-crop(Mix,poly)
cropForest<-cropBroadleaf+cropConifer+cropMixedWood
cropLichen <-crop(Lic,poly)
cropWetland <- crop(Wet,poly)
cropRocky <- crop(Roc,poly)
cropWater <- crop(Wat,poly)

plot(cropForest)
plot(poly, add=TRUE)


####model RSF in stopover
RSFCaribouSum<-mclogit(cbind(Randoms,PtID)~((Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + ((Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ Forest + Wetland + Lichen + Rocky + Water+ scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_stop)

####create the raster layer with RSF results
rain <- 0
swe<- -3
tmax<- 0
NDVI<-0
nosnowRSF<-exp(cropForest*coef(RSFCaribouSum)[1]+rain*coef(RSFCaribouSum)[2]+swe*coef(RSFCaribouSum)[3]+tmax*coef(RSFCaribouSum)[4]+cropLichen*coef(RSFCaribouSum)[5]+cropWetland*coef(RSFCaribouSum)[6]+cropRocky*coef(RSFCaribouSum)[7]+cropWater*coef(RSFCaribouSum)[8]+NDVI*coef(RSFCaribouSum)[9]+cropForest*rain*coef(RSFCaribouSum)[10]+cropForest*swe*coef(RSFCaribouSum)[11]+cropForest*tmax*coef(RSFCaribouSum)[12]+cropLichen*rain*coef(RSFCaribouSum)[13]+cropLichen*swe*coef(RSFCaribouSum)[14]+cropLichen*tmax*coef(RSFCaribouSum)[15])
              
Legend
plot(rainRSF)
plot(norainRSF)
par(new=TRUE)
plot(cropWater,col=c("transparent","blue"),legend=F)


plot(warmRSF)
par(new=TRUE)
plot(cropWater,col=c("transparent","blue"),legend=F)
mtext("UTM X", side = 1, padj = 3, adj = 0.4)
mtext("UTM Y", side = 2, padj = 2, outer = T)
mtext("A", side = 3,  at = 0.05)


plot(coldRSF)
par(new=TRUE)
plot(cropWater,col=c("transparent","blue"),legend=F)
mtext("UTM X", side = 1, padj = 3, adj = 0.4)
mtext("UTM Y", side = 2, padj = 2, outer = T)
mtext("B", side = 3,  at = 0.05)



plot(snowRSF)
plot(nosnowRSF)
rainRSF<-exp(Forest*coef(RSFCaribouSum)[2]+3)
plot(rainRSF)           

notrainRSF<-exp(Forest*coef(RSFCaribouSum)[2]-3)
plot(notrainRSF)
par(new=TRUE)
plot(Wat,col=c("transparent","blue"),legend=F)


####mvt RSF
####model RSF in mvt
RSFCaribouSum<-mclogit(cbind(Randoms,PtID)~((Forest)*(scale(prcp) + scale(swe) + scale(tmax))) + ((Lichen)*(scale(prcp)+scale(swe)+scale(tmax)))+ Forest + Wetland + Lichen + Rocky + Water+ scale(tmax) + scale(prcp) + scale(swe) + scale(NDVI), data = allNDVI_mvt)

####create the raster layer with RSF results
rain <- 0
swe<- 0
tmax<- -3
NDVI<-0
coldRSF_mvt<-exp(cropForest*coef(RSFCaribouSum)[1]+rain*coef(RSFCaribouSum)[2]+swe*coef(RSFCaribouSum)[3]+tmax*coef(RSFCaribouSum)[4]+cropLichen*coef(RSFCaribouSum)[5]+cropWetland*coef(RSFCaribouSum)[6]+cropRocky*coef(RSFCaribouSum)[7]+cropWater*coef(RSFCaribouSum)[8]+NDVI*coef(RSFCaribouSum)[9]+cropForest*rain*coef(RSFCaribouSum)[10]+cropForest*swe*coef(RSFCaribouSum)[11]+cropForest*tmax*coef(RSFCaribouSum)[12]+cropLichen*rain*coef(RSFCaribouSum)[13]+cropLichen*swe*coef(RSFCaribouSum)[14]+cropLichen*tmax*coef(RSFCaribouSum)[15])

plot(rainRSF_mvt)
plot(norainRSF_mvt)

plot(warmRSF_mvt)
par(new=TRUE)
plot(cropWater,col=c("transparent","blue"),legend=F)
mtext("UTM X", side = 1, padj = 3, adj = 0.4)
mtext("UTM Y", side = 2, padj = 2, outer = T)
mtext("B", side = 3,  at = 0.05)

plot(coldRSF_mvt)
par(new=TRUE)
plot(cropWater,col=c("transparent","blue"),legend=F)
mtext("UTM X", side = 1, padj = 3, adj = 0.4)
mtext("UTM Y", side = 2, padj = 2, outer = T)
mtext("B", side = 3,  at = 0.05)

# ALWAYS RUN THIS:
removeTmpFiles()
