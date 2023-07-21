######Heatmap code by ML###
library(lme4)  ### Or mclogit
library(raster)
library(mclogit)
library(RColorBrewer)


### Models ---- not need to scale the models 
m1 <- mclogit(cbind(Randoms,PtID)~ NDVISc + sweSc + tmaxSc + prcpSc + LichenSc +
                WetlandSc + ForestSc + RockySc + WaterSc + sweSc:WetlandSc +
                prcpSc:RockySc + tmaxSc:ForestSc + tmaxSc:LichenSc + tmaxSc:WetlandSc +
                prcpSc:sweSc + NDVISc:tmaxSc , data = allNDVI_mvt)

m1 <-glmer(HMM ~ NDVISc  + tmaxSc + prcpSc + LichenSc + WetlandSc + 
             ForestSc + RockySc + WaterSc + LichenSc*NDVISc + WetlandSc*NDVISc + RockySc*NDVISc
           + LichenSc*tmaxSc + RockySc*tmaxSc + WetlandSc*prcpSc + ForestSc*prcpSc + NDVISc*tmaxSc + (1|Animal_ID) + (1|Year), data = allNDVIobs, family = "binomial")
saveRDS(m1, 'S:/Local-git/emilie_nlcaribou/output/m1.Rds')
saveRDS(m2, 'S:/Local-git/emilie_nlcaribou/output/m2.Rds')
# Make heatmap dataframes

### Makes a dataframe that is the extent of variable 1 on the x and variable 2 on the y.

### This script is for two interactions (FPT and KDE), so you only need to run one if only making a single plot
HeatmapdataFPT <- data.frame(
  x = rep(seq(min(allNDVI_mvt$ForestSc, na.rm = FALSE),
              max(allNDVI_mvt$ForestSc, na.rm = FALSE),
              by = ((max(allNDVI_mvt$ForestSc, na.rm = FALSE) - min(allNDVI_mvt$ForestSc, na.rm = FALSE))/200)), 201),
  y = rep(seq(min(allNDVI_mvt$tmaxSc, na.rm = FALSE),
              max(allNDVI_mvt$tmaxSc, na.rm = FALSE),
              by = ((max(allNDVI_mvt$tmaxSc, na.rm = FALSE) - min(allNDVI_mvt$tmaxSc, na.rm = FALSE))/200)), each = 201))

HeatmapdataKDE <- data.frame(
  x = rep(seq(min(allNDVIobs$Forest, na.rm = FALSE),
              max(allNDVIobs$Forest, na.rm = FALSE),
              by = ((max(allNDVIobs$Forest, na.rm = FALSE) - min(allNDVIobs$Forest, na.rm = FALSE))/200)), 201),
  y = rep(seq(min(allNDVIobs$prcp, na.rm = FALSE),
              max(allNDVIobs$prcp, na.rm = FALSE),
              by = ((max(allNDVIobs$prcp, na.rm = FALSE) - min(allNDVIobs$prcp, na.rm = FALSE))/200)), each = 201))



# Model
## need to match up coefficents with the data, like in the kfold script. So 3 = moranScaled, 4 = mean of the ther variable,
## I think you can drop this if it's scaled it should be zero anyway.
## 2 = FPT scaled, 5 and 6 are the interactions
HeatmapdataFPT$z <- (((coef(m1)[7] * HeatmapdataFPT$x) +
                       (coef(m1)[3] * HeatmapdataFPT$y) +
                       (coef(m1)[12] * (HeatmapdataFPT$x) * (HeatmapdataFPT$y))))

HeatmapdataKDE$z <- (((fixef(m1)[7] * HeatmapdataKDE$x) +
                        (fixef(m1)[4] * HeatmapdataKDE$y) +
                        (fixef(m1)[17] * (HeatmapdataKDE$x) * (HeatmapdataKDE$y))))

## Rescale the data
HeatmapdataFPT$xnew <- (HeatmapdataFPT$x - (min(HeatmapdataFPT$x)))/(max(HeatmapdataFPT$x) - min(HeatmapdataFPT$x))
HeatmapdataFPT$ynew <- (HeatmapdataFPT$y - (min(HeatmapdataFPT$y)))/(max(HeatmapdataFPT$y) - min(HeatmapdataFPT$y))

HeatmapdataKDE$xnew <- (HeatmapdataKDE$x - (min(HeatmapdataKDE$x)))/(max(HeatmapdataKDE$x) - min(HeatmapdataKDE$x))
HeatmapdataKDE$ynew <- (HeatmapdataKDE$y - (min(HeatmapdataKDE$y)))/(max(HeatmapdataKDE$y) - min(HeatmapdataKDE$y))

## Put the XYZ data together
RastdataFPT <- cbind(HeatmapdataFPT$xnew, HeatmapdataFPT$ynew, HeatmapdataFPT$z)
RastdataKDE <- cbind(HeatmapdataKDE$xnew, HeatmapdataKDE$ynew, HeatmapdataKDE$z)

## Make a raster from the XYZ data
HeatmapFPT <- rasterFromXYZ(RastdataFPT)
HeatmapKDE <- rasterFromXYZ(RastdataKDE)

plot(HeatmapFPT, xlab = "Lichen", ylab ="Temperature")
# Labels - may need to change depending on your data, puts the location of scaled data at the right place
labelx<-seq(-0.35, 9.64,2)
labely<-seq(-3.47, 2.59,1.5)
ForestLoc<-(labelx-(min(allNDVI_mvt$ForestSc)))/(max(allNDVI_mvt$ForestSc)-min(allNDVI_mvt$ForestSc))
FPTLoc<-(labely-(min(allNDVI_mvt$tmaxSc)))/(max(allNDVI_mvt$tmaxSc)-min(allNDVI_mvt$tmaxSc))


# Contour lines - only if necessary
ContKDE <- rasterToContour(HeatmapKDE, nlevels = 5)
ContFPT <- rasterToContour(HeatmapFPT, nlevels = 5)

# Palette - feel froo to change, this goes white to green
rbPal <- colorRampPalette(c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529'))(75)
cols <- brewer.pal(10, "BrBG")
cols
rbPal <- colorRampPalette(c("#003C30","#01665E","#35978F","#80CDC1","#C7EAE5","#F6E8C3","#DFC27D","#BF812D","#8C510A","#543005"))(75)
### Output ----
#png("graphics/Fig_Heatmap_LichenNDVI_HMM2.png", width = 2.5, height = 2.5, units = "in",res=300)
#par(mfrow = c(1,1))
#x11(width=9, height=6, pointsize=12)
par(oma=c(0,0,0,0),mar=c(3.5, 5, 2, 2),mfrow=c(1,1),pch=16)
#par(mfrow=c(1,1), mar=c(3.5, 5.5, 3, 4) + 0.1 ) 
legend_im <- as.raster(matrix(rev(rbPal), ncol = 1))
image(HeatmapFPT, col = rbPal, axes = FALSE, xlim = c(0, 1.15),xlab=NA, ylab=NA,
      zlim = c(min(c(minValue(HeatmapFPT))), maxValue(HeatmapFPT)))
axis(1,at=ForestLoc,labels=labelx,cex.axis=0.8)
segments(0,0,1,0)
axis(2,at=FPTLoc,labels=labely,cex.axis=0.8, las = 1)
axis(2,at=c(-100,100),labels=NA,cex.axis=0.8)
mtext("Proportion of forest around movement locations", side = 1, padj = 2.5, adj = 2,cex=1.2)
mtext("Temperature", side = 2, padj = 3,line=0,outer=TRUE,cex=1.2)
mtext("C", side = 3,  at = 0.05,cex=1.2)
rasterImage(legend_im, 1.02, 0.2, 1.09, 0.8)
text(labels=round(maxValue(HeatmapFPT), digits = 2), x = 1.17, y = 0.8, bty="n",xpd=NA)
text(labels=round(minValue(HeatmapFPT), digits = 2), x = 1.17, y = 0.2, bty="n",xpd=NA)

########
labelx<-seq(0,1)
labely<-seq(0,26)
ForestLoc<-(labelx-(min(allNDVIobs$Forest)))/(max(allNDVIobs$Forest)-min(allNDVIobs$Forest))
KDELoc<-(labely-(min(allNDVIobs$prcp)))/(max(allNDVIobs$prcp)-min(allNDVIobs$prcp))


par(oma=c(0,0,0,0),mar=c(3.5, 5, 2, 2),mfrow=c(1,1),pch=16)
#par(mfrow=c(1,1), mar=c(3.5, 5.5, 3, 4) + 0.1 ) 
legend_im <- as.raster(matrix(rev(rbPal), ncol = 1))
image(HeatmapKDE, col = rbPal, axes = FALSE, xlim = c(0, 1.15),xlab=NA,
      zlim = c(min(c(minValue(HeatmapKDE))), maxValue(HeatmapKDE)))
#axis(1,at=ForestLoc,labels=labelx,cex.axis=0.8) 
axis(1,at=seq(0,1, by=10.5), labels = FALSE)
text(seq(0,1), par("usr")[3] - 0.02, labels = labelx, srt = 0, pos = 1, xpd = TRUE)
#axis(1,at=ForestLoc, seq(0,1),labels=labelx,cex.axis=0.5)
segments(0,0,1,0)
axis(2,at=KDELoc,labels=labely,cex.axis=0.8)
axis(2,at=c(-100,100),labels=NA,cex.axis=0.8)
mtext("Proportion of Forest", side = 1, padj = 1., adj = 0.4)
mtext("Precipitation", side = 2, padj = 4.5,line=0,outer=TRUE,cex=1)
mtext("C", side = 3,  at = 0.05)
rasterImage(legend_im, 1.02, 0.2, 1.09, 0.8)
text(labels=round(maxValue(HeatmapKDE), digits = 2), x = 1.15, y = 0.8, bty="n",xpd=NA)
text(labels=round(minValue(HeatmapKDE), digits = 2), x = 1.17, y = 0.2, bty="n",xpd=NA)




