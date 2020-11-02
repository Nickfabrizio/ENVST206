#Work with Raster data
library(raster)
library(ggplot2)
library(rgdal)


dirR <- "/Users/nickfabrizio/Documents/ENVST206 Data/Activity8Data/a08/oneida"
rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

plot(rdatB2/10000)

rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000
plotRGB(rgbS, scale=2)
plotRGB(rgbS, stretch="lin")
plotRGB(rgbS, stretch="lin",maxpixels=rgbS@nrows*rgbS@ncols)

#Question 4
fcm <- stack(rdatB8,rdatB4,rdatB3)/10000
plotRGB(fcm, stretch="lin",maxpixels=rgbS@nrows*rgbS@ncols)
rgbS@nrows*rgbS@ncols
#Analyzing raster data
NDVI <- (rdatB8 - rdatB4) / (rdatB8 + rdatB4)
plot(NDVI)

algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)),
       bty="n", cex=0.75)

landExtract <-  data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"), each=120)),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))

#Question 6
help(rep)

#Looking at Patterns in Remote Sensing Data
allbands <-  stack(rdatB2, rdatB3, rdatB4,rdatB8)/10000
ExtractOut <- raster::extract(allbands,landExtract[,2:3])
colnames(ExtractOut) <- c("B02","B03","B04","B08")
rasterEx <- cbind(landExtract,ExtractOut)
head(rasterEx)

ggplot(data=rasterEx, aes(x=B02, y=B03, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

#Question 7
ggplot(data=rasterEx, aes(x=B02, y=B08, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

ggplot(data=rasterEx, aes(x=B03, y=B08, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

ggplot(data=rasterEx, aes(x=B04, y=B08, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

#Question 8


NDVIExtractOut <- raster::extract(NDVI,landExtract[,2:3])
colnames(NDVIExtractOut) <- c("B02","B03","B04","B08")
NDVIrasterEx <- cbind(landExtract,NDVIExtractOut)

ggplot(data = NDVIrasterEx, aes(x=landcID, y=NDVIExtractOut))+
  geom_violin(fill=rgb(0.933,0.953,0.98))+
  geom_boxplot(width=0.15,size=0.25, fill="grey90")+
  labs(x="Land Class", y= "NDVI")+
  theme_classic()



