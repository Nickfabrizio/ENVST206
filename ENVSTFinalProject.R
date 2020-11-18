#install.packages(c("sp","rgdal","dplyr"))
library(sp)
library(rgdal)
library(dplyr)

datH <- readOGR("/Users/nickfabrizio/Documents/ENVST206 Data/FinalProjectData/qgis/storms/storms.shp")

#Creation of Dataframes

hdf <- data.frame(HURRINAME = datH@data$name,
                  HurricaneLat = datH@data$latitude,
                  HurricaneYear = as.numeric(datH@data$year),
                  HurricaneTimeStamp = datH@data$timestamp,
                  Status = datH@data$status)
datH@data[["status"]]

#install.packages("lubridate")
#install.packages("magrittr")
library(lubridate)
library(magrittr)

hdate <- as.Date(hdf$HurricaneTimeStamp, "%Y-%m-%dT%H:%M:%SZ")
head(hdate)

hdf$HMon <- month(hdate)
hdf$HDOY <- yday(hdate)
hdf$DD <- hdf$HurricaneYear + ((hdf$HDOY - 1)/ifelse(leap_year(hdf$HurricaneYear),366,365))

hdfHU <- hdf %>% 
  filter(Status=="HU")

hdfHL <- hdfHU %>%
  group_by(HURRINAME, HurricaneYear) %>% 
  summarise(DD=max(DD))

hdfLL <- hdfHU %>%
  group_by(HURRINAME, HurricaneYear) %>% 
  summarise(MaxLat=max(HurricaneLat))

hdfML <- hdfHU %>%
  group_by(HURRINAME, HurricaneYear) %>%
  summarise(MeanLat=mean(HurricaneLat))

hdfNY <- hdfHU %>%
  group_by(HurricaneYear) %>%
  summarise(n_distinct(HURRINAME))
names(hdfNY)[2] <- "HurricaneFreq"

AllH1 <- full_join(hdfLL, hdfHL, by=c("HURRINAME", "HurricaneYear"))
AllH2 <- full_join(AllH1, hdfML, by=c("HURRINAME", "HurricaneYear"))

#Creation of Regression: Number & Year
hdfNY.Reg <- lm(hdfNY$HurricaneFreq ~ hdfNY$HurricaneYear)
hdfNY.Res <- rstandard(hdfNY.Reg)
summary(hdfNY.Reg)

#Q-Q Plot: Number & Year
qqnorm(hdfNY.Res)
qqline(hdfNY.Res)

#Residual Plot: Number & Year & Regression
plot(hdfNY$HurricaneYear, hdfNY.Res,
     main= "Residual Plot: Frequency & Year",
     ylab="Residuals",
     xlab="Year")
abline(h=0)

#Scatter Plot of: Number & Year
plot(hdfNY$HurricaneYear, hdfNY$HurricaneFreq,
     pch = 19,
     col = "tomato3",
     main = "Number of Hurricanes vs. Year",
     ylab = "Number of Hurricanes",
     xlab = "Year")
abline(hdfNY.Reg, lwd=2)

#Creation of Regression: Highest Value & DD
hdfLL.Reg <- lm(AllH2$MaxLat ~ AllH2$DD)
hdfLL.Res <- rstandard(hdfLL.Reg)
summary(hdfLL.Reg)

#Q-Q Plot: Highest Value & DD
qqnorm(hdfLL.Res)
qqline(hdfLL.Res)

#Residual Plot: Highest Value & DD
plot(AllH2$DD, hdfLL.Res)
abline(h=0)

#Scatter Plot of: Highest Value & DD
plot(AllH2$DD, AllH2$MaxLat,
     pch = 19,
     col = "skyblue3",
     main= "Highest Latitude North vs. Time",
     ylab = "Latitude of Hurricanes (Degresse North)",
     xlab = "Year")
abline(hdfLL.Reg, lwd=2)

#Creation of Regression: Mean & DD
hdfML.Reg <- lm(AllH2$MeanLat ~ AllH2$DD)
hdfML.Res <- rstandard(hdfML.Reg)
summary(hdfML.Reg)

#Q-Q Plot: Mean & DD
qqnorm(hdfML.Res)
qqline(hdfML.Res)

#Residual Plot: Mean & DD
plot(AllH2$DD, hdfML.Res)
abline(h=0)
  
#Scatter Plot of: Mean & DD
plot(AllH2$DD, AllH2$MeanLat,
     pch = 19,
     col = "forestgreen",
     main = "Mean Latitude vs. Time",
     ylab = "Latitude of Hurricanes (Degresse North)",
     xlab = "Year")
abline(hdfML.Reg, lwd=2)


