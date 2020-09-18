ch4 <- read.csv("/Users/nickfabrizio/Downloads/ENVST206 Data/Activity 3 Data/lemming_herbivory.csv")
ch4$herbivory <- as.factor(ch4$herbivory)
plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")

shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])

bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

t.test(ch4$CH4_Flux ~ ch4$herbivory)

datI <- read.csv("/Users/nickfabrizio/Downloads/ENVST206 Data/Activity 3 Data/insect_richness.csv")
datI$urbanName <- as.factor(datI$urbanName)

#Running ANOVA
in.mod <- lm(datI$Richness ~ datI$urbanName)
in.aov <- aov(in.mod)
summary(in.aov)
tukeyT <- TukeyHSD(in.aov)
tukeyT
plot(tukeyT, cex.axis=0.75)
tapply(datI$Richness, datI$urbanName, "mean")
shapiro.test(datI$Richness)
bartlett.test(datI$Richness ~ datI$urbanType)

#Chi-squared goodness of fit test
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE)
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")
