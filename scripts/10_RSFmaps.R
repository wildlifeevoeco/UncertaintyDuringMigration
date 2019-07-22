Filesraster<-list.files('S:/Local-git/ewc/input/covariates/NL')

Ant<-raster('Anthro100.tif')
Bro<-raster('Broadleaf100.tif')
Con<-raster('Conifer100.tif')
Lic<-raster('Lichen100.tif')
Mix<-raster('MixedWood100.tif')
Roc<-raster('Rocky100.tif')
Scr<-raster('Scrub100.tif')
Wat<-raster('Water100.tif')
Wet<-raster('Wetland100.tif')
Forest<-Bro+Con+Mix

RSFCaribouSum<-mclogit(cbind(Randoms,PtID)~Forest*((prcp)+(swe) +(tmax)) + Lichen*((prcp)+(swe)+(tmax))+ Forest + Wetland + Lichen + Rocky + Water + tmax + prcp + swe + NDVI, data = allNDVI_stop)


SumRSF<-exp(Forest*coef(RSFCaribouSum)[1]+Fo*coef(RSFCaribouSum)[3]+Con*coef(RSFCaribouSum)[4]+Lic*coef(RSFCaribouSum)[5]+
              Mix*coef(RSFCaribouSum)[6]+Roc*coef(RSFCaribouSum)[7]+Scr*coef(RSFCaribouSum)[8])

Legend
plot(rainRSF)
par(new=TRUE)
plot(Wat,col=c("transparent","blue"),legend=F)

plot(SumRSF)

rainRSF<-exp(Forest*coef(RSFCaribouSum)[2]+3)
plot(rainRSF)           

notrainRSF<-exp(Forest*coef(RSFCaribouSum)[2]-3)
plot(notrainRSF)
par(new=TRUE)
plot(Wat,col=c("transparent","blue"),legend=F)
