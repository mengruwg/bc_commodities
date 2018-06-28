library(zoo)
library(REdaS)
library(psych)

source("R/5_pca_hist_setup.R")

# Für Variablen ab 1970:
dat1970 <- na.omit(dat1970)
bart_spher(dat1970[, 2:10])
KMOS(x = dat1970[, 2:10])
VSS.scree(dat1970[, 2:10])
pca.1970 <- principal(dat1970[, 2:10], nfactors = 2)
print(pca.1970,
      sort = T,
      cut = 0.53,
      digits = 2)

# Für Variablen ab 1970:
dat1975 <- na.omit(dat1975)
bart_spher(dat1975[, 2:11])
KMOS(x = dat1975[, 2:11])
VSS.scree(dat1975[, 2:11])
pca.1975 <- principal(dat1975[, 2:11], nfactors = 2)
print(pca.1975,
      sort = T,
      cut = 0.53,
      digits = 2)

# Für ab 1980
dat1980 <- na.omit(dat1980)
bart_spher(dat1980[, 2:14])
KMOS(x = dat1980[, 2:14])
VSS.scree(dat1980[, 2:14])
pca.1980 <- principal(dat1980[, 2:14], nfactors = 2)
print(pca.1980,
      sort = T,
      cut = 0.53,
      digits = 2)

# Für ab 1985
dat1985 <- na.omit(dat1985)
bart_spher(dat1985[, 2:17])
KMOS(x = dat1985[, 2:17])
VSS.scree(dat1985[, 2:17])
pca.1985 <- principal(dat1985[, 2:17], nfactors = 2)
print(pca.1985,
      sort = T,
      cut = 0.53,
      digits = 2)

# Speichern ab den 1980er da wir diese am besten verwenden können.

#round(head(pca.1980$scores), 3) # hier noch keine Transformationen
dat.scores.1980 <- data.frame(pca.1980$scores)
pca.scores.1980 <- cbind(dat1980$Date, dat.scores.1980)
colnames(pca.scores.1980) <- c("TIME", "commodities", "ind_metal")

saveRDS(pca.scores.1980, "data/comm_pca_1980.rds")
