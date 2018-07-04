comm_all <- readRDS("data/raw_data/comm_all.rds")
#Centering the Data.
#Create extra columns with centered values
comm_all[,50:97] <- scale(comm_all[,2:49], scale=T)
#Get column names from original
cols <- colnames(comm_all[2:49])
#Subset only centered values.
comm_centered <- cbind(comm_all$Date, comm_all[,50:97])
#use names from earlier
colnames(comm_centered) <- c("Date", cols)
#delete old commodities dataframe and colnames
rm(comm_all)
rm(cols)

custommean <- function(x) {
  mean(x, na.rm = TRUE)
}

# Transform to quarterly

b <- comm_centered
b$Date_q <- as.yearqtr(b$Date)
b <- aggregate(b[, 2:49], list(b$Date_q), custommean)
names(b)[1] <- "Date"
saveRDS(b, "data/raw_data/comm_mean_qu.rds")

#futures
futures_q <- subset(b, select = c(Date,
                                  Brent1yr,
                                  Brent2yr,
                                  Brent3yr,
                                  Brent6m,
                                  WTI1yr,
                                  WTI2yr,
                                  WTI3yr,
                                  WTI6m))
saveRDS(futures_q, "data/raw_data/comm_mean_qu_futures.rds")

#indices
indices_q <- subset(b, select = c(Date, BBIndex, Prec.met.Index,
                                  BBIndustrialIndex, BBEnergyIndex, SPIndustrialIndex,
                                  SPIndex, SP.prec.metIndex, SP.crude.oilIndex,
                                  SP.Nat.GasIndex, SP.AgriIndex, SP.Agri.LiveIndex,
                                  SP.LivestockIndex, SP.EnergyIndex, SP.CopperIndex,
                                  SP.AluminiumIndex, SP.GoldIndex))
saveRDS(indices_q, "data/raw_data/comm_mean_qu_indices.rds")

#commodities
commodities_q <- subset(b, select = -c(Brent1yr, Brent2yr, Brent3yr, Brent6m,
                                       WTI1yr, WTI2yr, WTI3yr, WTI6m,
                                       BBIndex, Prec.met.Index, BBIndustrialIndex, BBEnergyIndex,
                                       SPIndustrialIndex, SPIndex, SP.prec.metIndex, SP.crude.oilIndex,
                                       SP.Nat.GasIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex,
                                       SP.EnergyIndex, SP.CopperIndex, SP.AluminiumIndex, SP.GoldIndex))
saveRDS(commodities_q, "data/raw_data/comm_mean_qu_resources.rds")
