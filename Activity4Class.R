datB <- read.csv("/Users/nickfabrizio/Documents/ENVST206 Data/Activity4Data/beaver_dam.csv")

plot(datB$dams.n, datB$area.ha, pch = 19)

dam.mod <- lm(datB$area.ha ~ datB$dams.n)

dam.res <- rstandard(dam.mod)

qqnorm(dam.res)
qqline(dam.res)

shapiro.test(dam.res)

plot(datB$dams.n, dam.res, pch = 11)
abline(h= 0)

summary(dam.mod)
