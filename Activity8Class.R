dirR <- "/Users/nickfabrizio/Documents/ENVST206 Data/Activity8Data/a08/oneida"

#install.packages("raster")

library(raster)
library(rgdal)
library(ggplot2)

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
