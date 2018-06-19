library(lubridate)
library(zoo)
library(dplyr)
library(REdaS)
library(psych)
library(readr)

datCOM <- readRDS("data/raw_data/commodities.rds")
#ohne futures
datCOM2 <-subset(datCOM, select= -c(Brent1yr, Brent2yr, Brent3yr, Brent6m, WTI1yr, WTI2yr, WTI3yr, WTI6m))
#Ohne fehlende Daten
datCOM2 <- subset(datCOM2, select= -c(IronIndex, IronIndex2, PalladiumPrice, SP.Nat.GasIndex.x,SP.Nat.GasIndex.y, IronPrice, SteelIndex, SteelPrice))

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
pca.d<-principal(datCOM_means_m[,2:33], nfactors=3)
print(pca.d, sort=T, cut=0.52, digits=2)

#Run PCA again without the variables that did not fit a component. 
datCOM_means_m2<- subset(datCOM_means_m, select=-c(CopperIndex, LeadPrice, CopperPrice, ZincPrice, AluPrice, TinIndex))

bart_spher(datCOM_means_m2[,2:27])
KMOS(x=datCOM_means_m2[,2:27])
VSS.scree(datCOM_means_m2[,2:27])
pca.d2<-principal(datCOM_means_m2[,2:27], nfactors=3)
print(pca.d2, sort=T, cut=0.52, digits=2)

round(head(pca.d2$scores),3)
dat.scores<- data.frame(pca.d2$scores)
colnames(dat.scores)<- c("OGI", "Energy.agri", "Metals")

datcom3 <- cbind(datCOM_means_m2$Group.1, dat.scores)
colnames(datcom3) <- c("Date","OGI", "Energy.agri", "Metals")

write_rds(datcom3, "data/comm_PCA.rds")
