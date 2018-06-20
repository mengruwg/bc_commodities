library(lubridate)
library(zoo)
library(dplyr)
library(REdaS)
library(psych)
library(readr)

setwd("C:/Users/cenge/Desktop/Money Credit Finance/Paper/bc_commodities/data/raw_data")
datCOM <- readRDS("datCOM.rds")
#ohne futures
datCOM2 <-subset(datCOM, select= -c(Brent1yr, Brent2yr, Brent3yr, Brent6m, WTI1yr, WTI2yr, WTI3yr, WTI6m))
#Ohne fehlende Daten
datCOM2 <- subset(datCOM2, select= -c(IronIndex, IronIndex2, PalladiumPrice, SP.Nat.GasIndex, IronPrice, SteelIndex, SteelPrice))

#create averages per month
custommean <- function(x) {
  mean(x, na.rm = TRUE)
}


datCOM2$Date_q <- as.yearqtr(datCOM2$Date)
datCOM2$Date_m <- as.yearmon(datCOM2$Date)


datCOM_means_q <- aggregate(datCOM2[,2:34], list(datCOM2$Date_q), custommean)
datCOM_means_m <- aggregate(datCOM2[,2:34], list(datCOM2$Date_m), custommean)
datCOM_means_m<- na.omit(datCOM_means_m)
colnames(datCOM_means_m)

bart_spher(datCOM_means_m[,2:34])
KMOS(x=datCOM_means_m[,2:34])
VSS.scree(datCOM_means_m[,2:34])
pca.d<-principal(datCOM_means_m[,2:34], nfactors=3)
print(pca.d, sort=T, cut=0.55, digits=2)

#Run PCA again without the indices
datCOM_means_m2<- subset(datCOM_means_m, select=-c(BBIndex, Prec.met.Index, BBIndustrialIndex, BBEnergyIndex, SPIndustrialIndex, SPIndex, SP.prec.metIndex, SP.crude.oilIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, SP.EnergyIndex, SP.CopperIndex, SP.AluminiumIndex,SP.GoldIndex))

bart_spher(datCOM_means_m2[,2:19])
KMOS(x=datCOM_means_m2[,2:19])
VSS.scree(datCOM_means_m2[,2:19])
pca.d2<-principal(datCOM_means_m2[,2:19], nfactors=3)
print(pca.d2, sort=T, cut=0.53, digits=2)

#PCA, just the indices
datCOM_means_m3<- subset(datCOM_means_m, select=c(BBIndex, Prec.met.Index, BBIndustrialIndex, BBEnergyIndex, SPIndustrialIndex, SPIndex, SP.prec.metIndex, SP.crude.oilIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, SP.EnergyIndex, SP.CopperIndex, SP.AluminiumIndex,SP.GoldIndex))
datCOM_means_m3<- na.omit(datCOM_means_m3)
bart_spher(datCOM_means_m3[,2:15])
KMOS(x=datCOM_means_m3[,2:15])
VSS.scree(datCOM_means_m3[,2:15])
pca.d3<-principal(datCOM_means_m3[,2:15], nfactors=2)
print(pca.d3, sort=T, cut=0.68, digits=2)


#Again with more indices deleted
datCOM_means_m4<- subset(datCOM_means_m, select=-c(ZincIndex, AluIndex, LeadIndex, TinIndex, CopperIndex, NickelIndex, BBIndex, Prec.met.Index, BBIndustrialIndex, BBEnergyIndex, SPIndustrialIndex, SPIndex, SP.prec.metIndex, SP.crude.oilIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, SP.EnergyIndex, SP.CopperIndex, SP.AluminiumIndex,SP.GoldIndex))
datCOM_means_m4<- na.omit(datCOM_means_m4)
bart_spher(datCOM_means_m4[,2:13])
KMOS(x=datCOM_means_m4[,2:13])
VSS.scree(datCOM_means_m4[,2:13])
pca.d4<-principal(datCOM_means_m4[,2:13], nfactors=2)
print(pca.d4, sort=T, cut=0.55, digits=2)

#Output von scores erstellen für VAR
#Indices Scores
round(head(pca.d3$scores),3)
dat.scores<- data.frame(pca.d3$scores)
colnames(dat.scores)<- c("me.ag", "en.in.oi.")

datcom3 <- cbind(datCOM_means_m2$Group.1, dat.scores)
colnames(datcom3) <- c("Date","En.In.Oi.", "Me.Ag")

#Datei erstellen für Indices PCA Scores
write_rds(datcom3, "Ind_PCA.rds")
datcom3$Date2 <- as.yearqtr(datcom3$Date)
datcom4<- aggregate(datcom3[,2:4], list(datcom3$Date2), custommean)
write_rds(datcom4, "Ind_PCA_q.rds")










