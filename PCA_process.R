library(lubridate)
library(zoo)
library(dplyr)
library(REdaS)
library(psych)

datCOM <- readRDS("C:/Users/cenge/Desktop/Money Credit Finance/Paper/bc_commodities/data/commodities/datCOM.RDS")
#ohne futures
datCOM2<-subset(datCOM, select= -c(Brent1yr, Brent2yr, Brent3yr, Brent6m, WTI1yr, WTI2yr, WTI3yr, WTI6m))
datCOM2<- subset(datCOM2, select=-c(IronIndex, IronIndex2, PalladiumPrice, SP.Nat.GasIndex.x,SP.Nat.GasIndex.y, IronPrice, SteelIndex, SteelPrice))

#create averages per month
custommean <- function(x) {
  mean(x, na.rm = TRUE)
}



datCOM2$Date_q <- as.yearqtr(datCOM2$Date)
datCOM2$Date_m <- as.yearmon(datCOM2$Date)



datCOM_means_q <- aggregate(datCOM2[,2:33], list(datCOM2$Date_q), custommean)
datCOM_means_m <- aggregate(datCOM2[,2:33], list(datCOM2$Date_m), custommean)
datCOM_means_m<- na.omit(datCOM_means_m)

bart_spher(datCOM_means_m[,2:33])
KMOS(x=datCOM_means_m[,2:33])
VSS.scree(datCOM_means_m[,2:33])
pca.d<-principal(datCOM_means_m[,2:33], nfactors=30)
print(pca.d, sort=T, cut=0.5, digits=2)
