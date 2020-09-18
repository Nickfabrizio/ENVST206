#activity 2

heights <- c(3,2,3)

datW <- read.csv("/Users/nickfabrizio/Downloads/ENVST206 Data/Activity 2 Data/noaa2011124.csv")

datW$PRCP_cm <- datW$PRCP/10

mean(datW$PRCP_cm, na.rm=TRUE)

datW$PRCP_cm[1]
datWPRCP_cm[1]
datW$PRCP_cm[2:3]
datW$PRCP_cm[2:3]
yr1930 <- datW$PRCP_cm[datW$year == 1930]
yrs1950 <- datW$PRCP_cm[datW$year < 1950]

dat50s <- datW[datW$year <= 1950, ]
dat50s <- datW[datW$year <= 1950, 2:3]
dat50s
hits(datW$TMAX[datW$year == 1930 & datW$NAME == "MORRISVILLE 6 SW, NY US"])

help(hints)
help(hist)

datW$NAME <- as.factor(datW$NAME)
