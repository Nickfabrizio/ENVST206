datB <- read.csv("/Users/nickfabrizio/Documents/ENVST206 Data/Activity4Data/beaver_dam.csv")

plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

dam.mod <- lm(datB$area.ha ~ datB$dams.n)
dam.res <- rstandard(dam.mod)
qqnorm(dam.res)
qqline(dam.res)
shapiro.test(dam.res)

plot(datB$dams.n, dam.res, 
     xlab = "beaver damns", 
     ylab = "standardized residual")
abline (h=0)

summary(dam.mod )
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")
abline(dam.mod, lwd=2)

pheno <- read.csv("/Users/nickfabrizio/Documents/ENVST206 Data/Activity4Data/red_maple_pheno.csv")
par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum Temperature (Celsius)")
plot(pheno$Lat,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Latitude")

plot(pheno$elev,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Elevation")

plot(as.factor(pheno$siteDesc),pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Site Type")

#Check for Multi-collinearity
plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

#Run the regression
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)
mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
mlFitted <- fitted(mlr)

#Question 6
mlr.res <- rstandard(mlr)
qqnorm(mlr.res)
qqline(mlr.res)
plot(mlFitted, mlr.res, 
     xlab = "Fitted Regression", 
     ylab = "standardized residual")
abline(h=0)

