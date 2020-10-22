install.packages("rgdal")
datH <- readOGR("/Users/nickfabrizio/Documents/ENVST206 Data/FinalProjectData/qgis/storms/storms.shp")

hdf <- data.frame(HURRINAME = datH@data$name,
                  HurricaneLat = datH@data$latitude,
                  HurricaneYear = datH@data$year)
plot (hdf$HurricaneYear, hdf$HurricaneLat)

plot(datH[datH@data$name == "Arthur",])

Latmean <- aggregate(hdf$HurricaneLat, by=list(hdf$HURRINAME, hdf$HurricaneYear), FUN="mean")

colnames(Latmean) <- c("HURRINAME", "HurricaneYear", "LatMean")

plot(Latmean$HurricaneYear, Latmean$LatMean,
     pch = 20,
     ylab = "Hurricane Latitude Mean",
     xlab = "Hurricane Yeaer")

mean(Latmean$LatMean)
sd(Latmean$LatMean)
