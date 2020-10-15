#Set Up Spatial Packages
#install.packages(c("sp", "rgdal", "dplyr"))
library(sp)
library(rgdal)
library(dplyr)

#Reading in vector data
g1966 <- readOGR("/Users/nickfabrizio/Documents/ENVST206 Data/Activity6Data/a06/GNPglaciers/GNPglaciers_1966.shp")
g2015 <- readOGR("/Users/nickfabrizio/Documents/ENVST206 Data/Activity6Data/a06/GNPglaciers/GNPglaciers_2015.shp")

str(g2015)
plot(g1966, col = "lightblue2", border="grey50")

head(g2015@data)
g1966@proj4string

g1966@data$GLACNAME
g2015@data$GLACNAME

g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))


#Vector Data analysis: glacier retreat

gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)
gAll <- full_join(gdf66,gdf15, by="GLACNAME")

gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")

spplot(g1966, "gdiff", main="% change in area", col="transparent")

vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")


#Question 7

plot(gAll$area66, gAll$gdiff,
     pch = 19,
     col = "royalblue4",
     ylab = "Percent Change in Area",
     xlab = "Glacier Area in 1966 (Meters Squared)")

#Question 8

mean(gAll$gdiff, na.rm = TRUE)
sd(gAll$gdiff, na.rm = TRUE)

#Question 9

boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier", ]
boulder15 <-g2015[g2015@data$GLACNAME == "Boulder Glacier", ]

plot(boulder66, main="Boulder Glacier: Largest Percent Loss (84.7%)", col="skyblue")
plot(boulder15, col="tomato3", add=TRUE)
legend("topleft", #position
             c("Boulder Glacier in 1996", "Boulder Glacier in 2015"), #labels
             col= c("skyblue", "tomato3"), #colors
             pch=19, #point shape
             lwd=1,
             bty="n")

pumpelly66 <-g1966[g1966@data$GLACNAME == "Pumpelly Glacier", ] 
pumpelly15 <-g2015[g2015@data$GLACNAME == "Pumpelly Glacier", ] 
plot(pumpelly66, main="Pumpelly Glacier: Smallest Percent Loss (10.3%)", col="black")
plot(pumpelly15, col="gold3", add=TRUE)
legend("topleft", #position
       c("Pumpelly Glacier in 1996", "Pumpelly Glacier in 2015"), #labels
       col= c("black", "gold3"), #colors
       pch=19, #point shape
       lwd=1,
       bty="n")


