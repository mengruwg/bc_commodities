library(zoo)
library(reshape2)
library(ggplot2)

comm_all <- readRDS("data/raw_data/comm_all.rds")

stationarise_comm <- function(x, log = TRUE, diff = TRUE) {
  if(log) x <- log(x)
  
  if(diff) {
    x <- diff(x)
  }
  
  return(x)
}

comm_all$Date <- as.yearqtr(comm_all$Date)
comm_all <- comm_all[order(comm_all$Date), ]

comm_aus <- comm_all[c("Date", "SPIndustrialIndex", "SP.Agri.LiveIndex")]
comm_chl <- comm_all[c("Date", "SP.GoldIndex", "SP.CopperIndex")]
comm_nor <- comm_all[c("Date", "SP.EnergyIndex")]
comm_zaf <- comm_all[c("Date", "SP.prec.metIndex")]

comm_aus <- comm_aus[complete.cases(comm_aus), ]
comm_chl <- comm_chl[complete.cases(comm_chl), ]
comm_nor <- comm_nor[complete.cases(comm_nor), ]
comm_zaf <- comm_zaf[complete.cases(comm_zaf), ]

plot(ts(comm_aus))
plot(ts(comm_chl))
plot(ts(comm_nor))
plot(ts(comm_zaf))

comm_aus[-1, 2:3] <- apply(comm_aus[2:3], 2, stationarise_comm)
comm_aus <- comm_aus[-1, ]
comm_chl[-1, 2:3] <- apply(comm_chl[2:3], 2, stationarise_comm)
comm_chl <- comm_chl[-1, ]
comm_nor[-1, 2] <- stationarise_comm(comm_nor$SP.EnergyIndex)
comm_nor <- comm_nor[-1, ]
comm_zaf[-1, 2] <- stationarise_comm(comm_zaf$SP.prec.metIndex)
comm_zaf <- comm_zaf[-1, ]

names(comm_aus) <- c("Date", "ind_met", "agr_liv")
names(comm_chl) <- c("Date", "gold", "copper")
names(comm_nor) <- c("Date", "energy")
names(comm_zaf) <- c("Date",  "prec_met")

# Quarterly
comm_aus <- comm_aus[seq(3, nrow(comm_aus), 4), ]
comm_chl <- comm_chl[seq(3, nrow(comm_chl), 4), ]
comm_nor <- comm_nor[seq(3, nrow(comm_nor), 4), ]
comm_zaf <- comm_zaf[seq(3, nrow(comm_zaf), 4), ]

out <- list(comm_aus, comm_chl, comm_nor, comm_zaf)
names(out) <- c("AUS", "CHL", "NOR", "ZAF")

saveRDS(out, "data/raw_data/comm_country_stationary.rds")
