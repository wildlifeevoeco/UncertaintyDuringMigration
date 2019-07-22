######Heatmap code by ML###
library(lme4)  ### Or mclogit
library(raster)


### Models ----
m1 <-
  mclogit(cbind(Randoms,PtID)~(ForestSc)*((prcpSc) + (sweSc) + (tmaxSc)) + (LichenSc)*((prcpSc)+(sweSc)+(tmaxSc))+(ForestSc) + (WetlandSc) + (LichenSc) + (RockySc) + (WaterSc) + (tmaxSc) + (prcpSc) + (sweSc) + (NDVISc), data = data)

m2 <-
  mclogit(cbind(Randoms,PtID)~(ForestSc)*((prcpSc) + (sweSc) + (tmaxSc)) + (LichenSc)*((prcpSc)+(sweSc)+(tmaxSc))+(ForestSc) + (WetlandSc) + (LichenSc) + (RockySc) + (WaterSc) + (tmaxSc) + (prcpSc) + (sweSc) + (NDVISc), data = data)


# Make heatmap dataframes

### Makes a dataframe that is the extent of variable 1 on the x and variable 2 on the y.

### This script is for two interactions (FPT and KDE), so you only need to run one if only making a single plot
HeatmapdataFPT <- data.frame(
  x = rep(seq(min(data$ForestSc, na.rm = FALSE),
              max(data$ForestSc, na.rm = FALSE),
              by = ((max(data$ForestSc, na.rm = FALSE) - min(data$ForestSc, na.rm = FALSE))/200)), 201),
  y = rep(seq(min(data$prcpSc, na.rm = FALSE),
              max(data$prcpSc, na.rm = FALSE),
              by = ((max(data$prcpSc, na.rm = FALSE) - min(data$prcpSc, na.rm = FALSE))/200)), each = 201))

HeatmapdataKDE <- data.frame(
  x = rep(seq(min(data$LichenSc, na.rm = FALSE),
              max(data$LichenSc, na.rm = FALSE),
              by = ((max(data$LichenSc, na.rm = FALSE) - min(data$LichenSc, na.rm = FALSE))/200)), 201),
  y = rep(seq(min(data$Tmax, na.rm = FALSE),
              max(data$Tmax, na.rm = FALSE),
              by = ((max(data$Tmax, na.rm = FALSE) - min(data$Tmax, na.rm = FALSE))/200)), each = 201))



# Model
## need to match up coefficents with the data, like in the kfold script. So 3 = moranScaled, 4 = mean of the ther variable,
## I think you can drop this if it's scaled it should be zero anyway.
## 2 = FPT scaled, 5 and 6 are the interactions
HeatmapdataFPT$z <- (((coef(m1)[1] * HeatmapdataFPT$x) +
                       (coef(m1)[2] * HeatmapdataFPT$y) +
                       (coef(m1)[10] * (HeatmapdataFPT$x) * (HeatmapdataFPT$y))))

HeatmapdataKDE$z <- ((fixef(m1)[1]) + (fixef(m1)[3] * HeatmapdataKDE$x) +
                       (fixef(m1)[2] * mean(DT$fptScaled)) +
                       (fixef(m1)[4] * HeatmapdataKDE$y) +
                       (fixef(m1)[6] * (HeatmapdataKDE$x) * (HeatmapdataKDE$y)) +
                       (fixef(m1)[5] * (HeatmapdataKDE$x) * mean(DT$fptScaled)))

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

plot(HeatmapFPT, xlab = "coefficient?", ylab ="coefficient?")
# Labels - may need to change depending on your data, puts the location of scaled data at the right place
labelx<-seq(0,10,1)
labely<-seq(-1,5,1)
ForestLoc<-(labelx-(min(data$ForestSc)))/(max(data$ForestSc)-min(data$ForestSc))

FPTLoc<-(labely-(min(data$prcpSc)))/(max(data$prcpSc)-min(data$prcpSc))

KDELoc<-(labels-(min(DT$areaRatioScaled)))/(max(DT$areaRatioScaled)-min(DT$areaRatioScaled))

# Contour lines - only if necessary
ContKDE <- rasterToContour(HeatmapKDE, nlevels = 5)
ContFPT <- rasterToContour(HeatmapFPT, nlevels = 5)

# Palette - feel froo to change, this goes white to green
rbPal <- colorRampPalette(c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529'))(75)

### Output ----
png("graphics/Fig2_Heatmap.png", width = 6.81, height = 3.4, units = "in",res=300)
par(mar = c(3.5, 3, 1, 0), mfrow = c(1, 2))
legend_im <- as.raster(matrix(rev(rbPal), ncol = 1))
image(HeatmapFPT, col = rbPal, axes = FALSE, xlim = c(0, 1.15),xlab=NA,
      zlim = c(min(c(minValue(HeatmapFPT))), maxValue(HeatmapFPT)))
axis(1,at=ForestLoc,labels=labelx,cex.axis=0.8)
segments(0,0,1,0)
axis(2,at=FPTLoc,labels=labely,cex.axis=0.8, las = 1)
axis(2,at=c(-100,100),labels=NA,cex.axis=0.8)
mtext("Proportion of forest", side = 1, padj = 3, adj = 0.4)
mtext("Variation of precipitation", side = 2, padj = 2, outer = T)
mtext("A", side = 3,  at = 0.05)
par(mar = c(3.5, 1.5, 1, 0))
image(HeatmapKDE, col = rbPal, axes = FALSE, xlim = c(0, 1.25), xlab=NA,
      zlim = c(min(c(minValue(HeatmapKDE), minValue(HeatmapFPT))),
               max(c(maxValue(HeatmapKDE), maxValue(HeatmapFPT)))))
lines(ContKDE)
axis(1,at=moranLoc,labels=labels,cex.axis=0.8)
axis(2,at=KDELoc,labels=labels,cex.axis=0.8, las = 1)
axis(2,at=c(-100,100),labels=NA,cex.axis=0.8)
segments(0,0,1,0)
mtext("Forage patch aggregation", side = 1, padj = 3, adj = 0.4)
mtext("Range-use ratio", side = 2, padj = 26.8, outer = T)
rasterImage(legend_im, 1.02, 0.2, 1.09, 0.8)
mtext("B", side = 3,  at = 0.05)
text(x = 1.18, y = 0.8, round(max(c(maxValue(HeatmapKDE), maxValue(HeatmapFPT))), digits = 2))
text(x = 1.18, y = 0.2, round(min(c(minValue(HeatmapKDE), minValue(HeatmapFPT))), digits = 2))
dev.off()

