library(lubridate)
library(zoo)
library(dplyr)
library(REdaS)
library(psych)

datCOM_all <- readRDS("data/raw_data/comm_all.rds")

# ohne futures
datCOM <- subset(datCOM_all, select = -c(
  Brent1yr,
  Brent2yr,
  Brent3yr,
  Brent6m,
  WTI1yr,
  WTI2yr,
  WTI3yr,
  WTI6m))

# ohne fehlende Daten
datCOM <- subset(datCOM, select = -c(
  IronIndex,
  IronIndex2,
  PalladiumPrice,
  SP.Nat.GasIndex,
  IronPrice,
  TinIndex,
  SteelIndex,
  SteelPrice))

# create averages per month
custommean <- function(x) {
  mean(x, na.rm = TRUE)
}

datCOM$Date_q <- as.yearqtr(datCOM$Date)
datCOM$Date_m <- as.yearmon(datCOM$Date)


datCOM_means_q <- aggregate(datCOM[, 2:33], list(datCOM$Date_q), custommean)
datCOM_means_m <- aggregate(datCOM[, 2:33], list(datCOM$Date_m), custommean)
datCOM_means_m <- na.omit(datCOM_means_m)
names(datCOM_means_m)[1] <- names(datCOM_means_q)[1] <- "Date"

bart_spher(datCOM_means_m[, 2:33])
KMOS(x = datCOM_means_m[, 2:33])
VSS.scree(datCOM_means_m[, 2:33])
pca.d <- principal(datCOM_means_m[, 2:33], nfactors = 3)
print(pca.d,
      sort = T,
      cut = 0.53,
      digits = 2)

# Run PCA again without the indices
datCOM_means_m2 <- subset(datCOM_means_m, select = -c(
  BBIndex,
  Prec.met.Index,
  BBIndustrialIndex,
  BBEnergyIndex,
  SPIndustrialIndex,
  SPIndex,
  SP.prec.metIndex,
  SP.crude.oilIndex,
  SP.AgriIndex,
  SP.Agri.LiveIndex,
  SP.LivestockIndex,
  SP.EnergyIndex,
  SP.CopperIndex,
  SP.AluminiumIndex,
  SP.GoldIndex))

bart_spher(datCOM_means_m2[, 2:18])
KMOS(x = datCOM_means_m2[, 2:18])
VSS.scree(datCOM_means_m2[, 2:18])
pca.d2 <- principal(datCOM_means_m2[, 2:18], nfactors = 3)
print(pca.d2,
      sort = T,
      cut = 0.53,
      digits = 2)

# PCA, just the indices
datCOM_means_m3 <- subset(datCOM_means_m, select = c(
  BBIndex,
  Prec.met.Index,
  BBIndustrialIndex,
  BBEnergyIndex,
  SPIndustrialIndex,
  SPIndex,
  SP.prec.metIndex,
  SP.crude.oilIndex,
  SP.AgriIndex,
  SP.Agri.LiveIndex,
  SP.LivestockIndex,
  SP.EnergyIndex,
  SP.CopperIndex,
  SP.AluminiumIndex,
  SP.GoldIndex))

datCOM_means_m3 <- na.omit(datCOM_means_m3)
bart_spher(datCOM_means_m3[, 2:15])
KMOS(x = datCOM_means_m3[, 2:15])
VSS.scree(datCOM_means_m3[, 2:15])
pca.d3 <- principal(datCOM_means_m3[, 2:15], nfactors = 2)
print(pca.d3,
      sort = T,
      cut = 0.68,
      digits = 2)

# Again with fewer indices
datCOM_means_m4 <- subset(datCOM_means_m, select = -c(
  ZincIndex,
  AluIndex,
  LeadIndex,
  CopperIndex,
  NickelIndex,
  BBIndex,
  Prec.met.Index,
  BBIndustrialIndex,
  BBEnergyIndex,
  SPIndustrialIndex,
  SPIndex,
  SP.prec.metIndex,
  SP.crude.oilIndex,
  SP.AgriIndex,
  SP.Agri.LiveIndex,
  SP.LivestockIndex,
  SP.EnergyIndex,
  SP.CopperIndex,
  SP.AluminiumIndex,
  SP.GoldIndex))

datCOM_means_m4 <- na.omit(datCOM_means_m4)
bart_spher(datCOM_means_m4[, 2:13])
KMOS(x = datCOM_means_m4[, 2:13])
VSS.scree(datCOM_means_m4[, 2:13])
pca.d4 <- principal(datCOM_means_m4[, 2:13], nfactors = 2)
print(pca.d4,
      sort = T,
      cut = 0.55,
      digits = 2)

# Output von scores erstellen für VAR
round(head(pca.d3$scores), 3)
dat.scores <- data.frame(pca.d3$scores)
colnames(dat.scores) <- c("metal_agric", "energy_indus")

datCOM_indices <- cbind(datCOM_means_m2$Date, dat.scores)
colnames(datCOM_indices)[1] <- "Date"

#Datei erstellen für Indices PCA Scores
saveRDS(datCOM_indices, "data/comm_pca_indices.rds")
