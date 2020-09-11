datW <- read.csv("/Users/nickfabrizio/Downloads/ENVST206 Data/Activity 2 Data/noaa2011124.csv")

#Question 2
str(datW)
f <- as.factor(datW$LATITUDE)
c <- c(3)
str(datW)
V <- c(f, c, 2.5, 10)

mean(datW$PRCP, na.rm=TRUE)
sd(datW$PRCP, na.rm=TRUE)
datW$NAME <- as.factor(datW$NAME)
#Question 3
help(hist)

levels(datW$NAME)
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp
datW$siteN <- as.numeric(datW$NAME)

hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

#Question 4
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="blue",
     border="white")

sdAvgTemp<- aggregate(datW$TAVE, by=list(datW$NAME), FUN="sd",na.rm=TRUE)
sdAvgTemp

help(dnorm)

pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
                                                         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Question 6
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation", 
     ylab="Relative frequency",
     col="red1",
     border="white")

#Question 7
yearsSITE <- aggregate(datW$PRCP, by=list(datW$year, datW$NAME), FUN="sum",na.rm=TRUE)
yearsSITE

#Question 8
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation", 
     ylab="Relative frequency",
     col="red",
     border="white")
hist(datW$PRCP[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Daily Precipitation", 
     ylab="Relative frequency",
     col="blue",
     border="white")

#Question 9
pnorm(70,
      mean(datW$PRCP[datW$siteN == 1],na.rm=TRUE),
      sd(datW$PRCP[datW$siteN == 1],na.rm=TRUE))
pnorm(70,
      mean(datW$PRCP[datW$siteN == 3],na.rm=TRUE),
      sd(datW$PRCP[datW$siteN == 3],na.rm=TRUE))
