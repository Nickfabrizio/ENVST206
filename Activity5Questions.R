datW <- read.csv("/Users/nickfabrizio/Documents/ENVST206 Data/Activity5Data/noaa2011124.csv")
datW$NAME <- as.factor(datW$NAME)
nameS <- levels(datW$NAME)

#Fundamentals of plotting data
datP <- na.omit(data.frame(PRCP = datW$PRCP,
                           NAME = datW$NAME,
                           year = datW$year))
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)
colnames(precip) <- c("NAME","year","totalP")
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x
pr <- precip[precip$ncount >=364, ]
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]
plot(ca$year, ca$totalP)
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year")
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
axis(2, seq(200,800, by=200), las=2 )
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
axis(2, seq(0,1600, by=400), las=2 )
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
legend("topleft",
       c("California", "New York"),
       col = c("black", "tomato3"),
       pch=19,
       lwd=1,
       bty="n")

#Question 3
datT <- na.omit(data.frame(NAME=datW$NAME,
                              year=datW$year,
                              TMAX=datW$TMAX))
Tmax <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="mean", na.rm=TRUE)
colnames(Tmax) <- c("NAME","year", "MeanTmax")
Tmax$ncount <- aggregate(datT$TMAX, by=list(datT$NAME,datT$year), FUN="length")$x
tm <- Tmax[Tmax$ncount >=364, ]
nd <- tm[tm$NAME == nameS[3], ]
nyT <- tm[tm$NAME == nameS[5], ]

plot(nd$year, nd$MeanTmax,
     type = "b",
     pch = 19,
     ylab = "Mean Maximum Tempreature (Celsius)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 25))
axis(2, seq(0,25, by=5), las=2 )
points(nyT$year, nyT$MeanTmax,
       type = "b",
       pch = 19,
       col = "gold2")
legend("topleft",
       c("North Dakota", "New York"),
       col = c("black", "gold2"),
       pch=19,
       lwd=1,
       bty="n")

#Using packages in R
#install.packages("ggplot2")
library(ggplot2)

#Plotting in ggplot2
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point()+
  geom_path()+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("#7FB3D5","#34495E", "#E7B800", "#FC4E07","#26A69A"))

#Question 5
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("#003f5c", "#D35400", "#CC33CC", "#3CD54B", "#ffa600"))

#Exploring different visualization in ggplot2

ggplot(data = datW, aes(x=NAME, y=TMIN))+
  geom_violin(fill=rgb(0.933,0.953,0.98))+
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+
  theme_classic()
sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#Question 8
sub2 <- datW[datW$NAME == nameS[1] & datW$ year == 1974,]
sub2$DATE <- as.Date(sub$DATE,"%Y-%m-%d")
ggplot(data=sub2, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub2, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")

#Question 9


datM <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMIN=datW$TMIN))
MinTemp <- aggregate(datM$TMIN, by=list(datM$year,datM$NAME), FUN="length")
colnames(MinTemp) <- c("year","NAME","MinTemp")

sub4 <- datW[datW$NAME == nameS[1] & datW$year >=2000,]

ggplot(data = sub4, aes(x=as.factor(year), y=TMIN))+
  geom_violin(fill=rgb(0.933,0.953,0.98))+
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+
  theme_classic()+
  labs(x="Year", y="Minimum Temperature (C)")


