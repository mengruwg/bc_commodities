library(readr)
library(readxl)

library(readr)

data_commodities <- read_delim("data_commodities.csv", 
                               "\t", escape_double = FALSE, col_types = cols(`CC21AMMN Steel Index` = col_double(), 
                                                                             `CC21BMMN Iron Index` = col_number(), 
                                                                             Date = col_number(), Date_1 = col_number(), 
                                                                             Date_17 = col_number(), Date_18 = col_number(), 
                                                                             Date_19 = col_number(), Date_2 = col_number(), 
                                                                             Date_20 = col_number(), Date_21 = col_number(), 
                                                                             Date_22 = col_number(), Date_23 = col_number(), 
                                                                             Date_3 = col_number(), Date_4 = col_number(), 
                                                                             Date_5 = col_number(), Date_6 = col_number(), 
                                                                             Date_7 = col_number(), `HRC1 Steel Comdty` = col_double(), 
                                                                             `LMAHDS03 Alu Comdty` = col_double(), 
                                                                             `LMNIDS03 Alu Comdty` = col_double(), 
                                                                             `LMSNDS03 Lead Comdty` = col_double(), 
                                                                             `NLSAH Alu Index` = col_double(), 
                                                                             `NLSCA Copper Index` = col_double(), 
                                                                             `NLSNI Nickel Index` = col_double(), 
                                                                             `NLSSN Tin Index` = col_double(), 
                                                                             `NLSZS Zinc Index` = col_double()), 
                               locale = locale(decimal_mark = ",", grouping_mark = "."), 
                               trim_ws = TRUE, skip = 1)

data_indices <- read_delim("data_indices.csv", 
                           "\t", escape_double = FALSE,  
                           locale = locale(decimal_mark = ",", grouping_mark = "."), 
                           trim_ws = TRUE, skip = 1)

library(readr)
data_oilfutures <- read_delim("data_oilfutures.csv", 
                               ";", escape_double = FALSE, col_types = cols(`Brent + 2Y` = col_number(), 
                                                                            `Brent + 3Y` = col_number(), Date = col_number(), 
                                                                            Date_1 = col_number(), Date_2 = col_number(), 
                                                                            Date_3 = col_number(), Date_4 = col_number(), 
                                                                            Date_5 = col_number(), Date_6 = col_number(), 
                                                                            Date_7 = col_number(), X2 = col_double()), 
                               locale = locale(decimal_mark = ",", grouping_mark = "."), 
                               trim_ws = TRUE, skip = 1)


#Date = col_date(format = "%d.%m.%Y"), Date_1 = col_date(format = "%d.%m.%Y"), Date_3 = col_date(format = "%d.%m.%Y"), Date_5 = col_date(format = "%d.%m.%Y"), Date_6 = col_date(format = "%d.%m.%Y"), Date_7 = col_date(format = "%d.%m.%Y"), Date_8 = col_date(format = "%d.%m.%Y"),Date_9 = col_date(format = "%d.%m.%Y"),Date_10 = col_date(format = "%d.%m.%Y"),Date_11 = col_date(format = "%d.%m.%Y"),Date_12 = col_date(format = "%d.%m.%Y"),Date_13 = col_date(format = "%d.%m.%Y"),Date_14 = col_date(format = "%d.%m.%Y"),Date_15 = col_date(format = "%d.%m.%Y"),Date_16 = col_date(format = "%d.%m.%Y"),Date_17 = col_date(format = "%d.%m.%Y"),Date_18 = col_date(format = "%d.%m.%Y"),Date_19 = col_date(format = "%d.%m.%Y"),Date_20 = col_date(format = "%d.%m.%Y"), Date_21 = col_date(format = "%d.%m.%Y"),Date_22 = col_date(format = "%d.%m.%Y"), Date_23 = col_date(format = "%d.%m.%Y")
#col_types = cols(Date = col_date(format = "%d.%m.%Y"), 

library(REdaS)
library(readr)
library(rlang)
library(lubridate)
library(zoo)
library(dplyr)



#Merge by Date
Copper_index <- data_commodities[,1:2]
colnames(Copper_index) <- c("Date", "CopperIndex")
Copper_index <- Copper_index[complete.cases(Copper_index),]

Copper_price <- data_commodities[,3:4]
colnames(Copper_price) <- c("Date", "CopperPrice")
Copper_price <- Copper_price[complete.cases(Copper_price),]


Alu_price <- data_commodities[,5:6]
colnames(Alu_price) <- c("Date", "AluPrice")
Alu_price <- Alu_price[complete.cases(Alu_price),]

Alu_Index <- data_commodities[,7:8]
colnames(Alu_Index) <- c("Date", "AluIndex")
Alu_Index <- Alu_Index[complete.cases(Alu_Index),]

Nickel_Price <- data_commodities[,9:10]
colnames(Nickel_Price) <- c("Date", "NickelPrice")
Nickel_Price <- Nickel_Price[complete.cases(Nickel_Price),]

Nickel_Index <- data_commodities[,11:12]
colnames(Nickel_Index) <- c("Date", "NickelIndex")
Nickel_Index <- Nickel_Index[complete.cases(Nickel_Index),]

Lead_Price <- data_commodities[,13:14]
colnames(Lead_Price) <- c("Date", "LeadPrice")
Lead_Price <- Lead_Price[complete.cases(Lead_Price),]

Lead_Index <- data_commodities[,15:16]
colnames(Lead_Index) <- c("Date", "LeadIndex")
Lead_Index <- Lead_Index[complete.cases(Lead_Index),]

Tin_Price <- data_commodities[,17:18]
colnames(Tin_Price) <- c("Date", "TinPrice")
Tin_Price <- Tin_Price[complete.cases(Tin_Price),]

Tin_Index <- data_commodities[,19:20]
colnames(Tin_Index) <- c("Date", "TinIndex")
Tin_Index <- Tin_Index[complete.cases(Tin_Index),]

Zinc_Price <- data_commodities[,21:22]
colnames(Zinc_Price) <- c("Date", "ZincPrice")
Zinc_Index <- data_commodities[,23:24]
colnames(Zinc_Index) <- c("Date", "ZincIndex")
Zinc_Price <- Zinc_Price[complete.cases(Zinc_Price),]
Zinc_Index <- Zinc_Index[complete.cases(Zinc_Index),]

Steel_Price <- data_commodities[,25:26]
colnames(Steel_Price) <- c("Date", "SteelPrice")
Steel_Index <- data_commodities[,27:28]
colnames(Steel_Index) <- c("Date", "SteelIndex")
Steel_Price <- Steel_Price[complete.cases(Steel_Price),]
Steel_Index <- Steel_Index[complete.cases(Steel_Index),]

Iron_Price <- data_commodities[,29:30]
colnames(Iron_Price) <- c("Date", "IronPrice")
Iron_Index <- data_commodities[,31:32]
colnames(Iron_Index) <- c("Date", "IronIndex")
Iron_Index2 <- data_commodities[,33:34]
colnames(Iron_Index2) <- c("Date", "IronIndex2")

Iron_Price <- Iron_Price[complete.cases(Iron_Price),]
Iron_Index <- Iron_Index[complete.cases(Iron_Index),]
Iron_Index2 <- Iron_Index2[complete.cases(Iron_Index2),]

Gold_Price <- data_commodities[,35:36]
colnames(Gold_Price) <- c("Date", "GoldPrice")
Gold_Price <- Gold_Price[complete.cases(Gold_Price),]

Platinum_Price <- data_commodities[,37:38]
colnames(Platinum_Price) <- c("Date", "PlatinumPrice")
Platinum_Price <- Platinum_Price[complete.cases(Platinum_Price),]

Palladium_Price <- data_commodities[,39:40]
colnames(Palladium_Price) <- c("Date", "PalladiumPrice")
Palladium_Price <- Palladium_Price[complete.cases(Palladium_Price),]

Silver_Price <- data_commodities[,41:42]
colnames(Silver_Price) <- c("Date", "SilverPrice")
Silver_Price <- Silver_Price[complete.cases(Silver_Price),]

Oil_Price <- data_commodities[,43:44]
colnames(Oil_Price) <- c("Date", "OilPrice")
Oil_Price <- Oil_Price[complete.cases(Oil_Price),]
Oil2_Price <- data_commodities[,45:46]
colnames(Oil2_Price) <- c("Date", "Oil2Price")
Oil2_Price <- Oil2_Price[complete.cases(Oil2_Price),]
Gas_Price <- data_commodities[,47:48]
colnames(Gas_Price) <- c("Date", "GasPrice")
Gas_Price <- Gas_Price[complete.cases(Gas_Price),]

BB_index <- data_indices[,1:2]
colnames(BB_index) <- c("Date", "BBIndex")
BB_index <- BB_index[complete.cases(BB_index),]

Precious_metals_index <- data_indices[,3:4]
colnames(Precious_metals_index) <- c("Date", "Prec.met.Index")
Precious_metals_index <- Precious_metals_index[complete.cases(Precious_metals_index),]

BBIndustrial_index <- data_indices[,5:6]
colnames(BBIndustrial_index) <- c("Date", "BBIndustrialIndex")
BBIndustrial_index <- BBIndustrial_index[complete.cases(BBIndustrial_index),]

BBEnergy_index <- data_indices[,7:8]
colnames(BBEnergy_index) <- c("Date", "BBEnergyIndex")
BBEnergy_index <- BBEnergy_index[complete.cases(BBEnergy_index),]


SPIndustrial_index <- data_indices[,13:14]
colnames(SPIndustrial_index) <- c("Date", "SPIndustrialIndex")
SPIndustrial_index <- SPIndustrial_index[complete.cases(SPIndustrial_index),]

SP_index <- data_indices[,9:10]
colnames(SP_index) <- c("Date", "SPIndex")
SP_index <- SP_index[complete.cases(SP_index),]

SP.prec.met_index <- data_indices[,11:12]
colnames(SP.prec.met_index) <- c("Date", "SP.prec.metIndex")
SP.prec.met_index <- SP.prec.met_index[complete.cases(SP.prec.met_index),]

SP.crude.oil_index <- data_indices[,15:16]
colnames(SP.crude.oil_index) <- c("Date", "SP.crude.oilIndex")
SP.crude.oil_index <- SP.crude.oil_index[complete.cases(SP.crude.oil_index),]

SP.Nat.Gas_index <- data_indices[,17:18]
colnames(SP.Nat.Gas_index) <- c("Date", "SP.Nat.GasIndex")
SP.Nat.Gas_index <- SP.Nat.Gas_index[complete.cases(SP.Nat.Gas_index),]

SP.Agri_index <- data_indices[,19:20]
colnames(SP.Agri_index) <- c("Date", "SP.AgriIndex")
SP.Agri_index <- SP.Agri_index[complete.cases(SP.Agri_index),]

SP.Agri.Live_index <- data_indices[,21:22]
colnames(SP.Agri.Live_index) <- c("Date", "SP.Agri.LiveIndex")
SP.Agri.Live_index <- SP.Agri.Live_index[complete.cases(SP.Agri.Live_index),]

SP.Livestock_index <- data_indices[,23:24]
colnames(SP.Livestock_index) <- c("Date", "SP.LivestockIndex")
SP.Livestock_index <- SP.Livestock_index[complete.cases(SP.Livestock_index),]

SP.Energy_index <- data_indices[,25:26]
colnames(SP.Energy_index) <- c("Date", "SP.EnergyIndex")
SP.Energy_index <- SP.Energy_index[complete.cases(SP.Energy_index),]


SP.Copper_index <- data_indices[,27:28]
colnames(SP.Copper_index) <- c("Date", "SP.CopperIndex")
SP.Copper_index <- SP.Copper_index[complete.cases(SP.Copper_index),]

SP.Aluminium_index <- data_indices[,29:30]
colnames(SP.Aluminium_index) <- c("Date", "SP.AluminiumIndex")
SP.Aluminium_index <- SP.Aluminium_index[complete.cases(SP.Aluminium_index),]


SP.Gold_index <- data_indices[,31:32]
colnames(SP.Gold_index) <- c("Date", "SP.GoldIndex")
SP.Gold_index <- SP.Gold_index[complete.cases(SP.Gold_index),]


Brent3 <- data_oilfutures[,1:2]
colnames(Brent3) <- c("Date", "Brent3yr")
Brent3 <- Brent3[complete.cases(Brent3),]

Brent2 <- data_oilfutures[,3:4]
colnames(Brent2) <- c("Date", "Brent2yr")
Brent2 <- Brent2[complete.cases(Brent2),]

Brent1 <- data_oilfutures[,5:6]
colnames(Brent1) <- c("Date", "Brent1yr")
Brent1 <- Brent1[complete.cases(Brent1),]

Brent6 <- data_oilfutures[,7:8]
colnames(Brent6) <- c("Date", "Brent6m")
Brent6 <- Brent6[complete.cases(Brent6),]

WTI3 <- data_oilfutures[,9:10]
colnames(WTI3) <- c("Date", "WTI3yr")
WTI3 <- WTI3[complete.cases(WTI3),]

WTI2 <- data_oilfutures[,11:12]
colnames(WTI2) <- c("Date", "WTI2yr")
WTI2 <- WTI2[complete.cases(WTI2),]

WTI1 <- data_oilfutures[,13:14]
colnames(WTI1) <- c("Date", "WTI1yr")
WTI1 <- WTI1[complete.cases(WTI1),]

WTI6 <- data_oilfutures[,15:16]
colnames(WTI6) <- c("Date", "WTI6m")
WTI6 <- WTI6[complete.cases(WTI6),]


a<- merge(Alu_Index, Alu_price, by="Date", all= T)
a<- merge(a, BB_index, by="Date", all=T)
a<- merge(a, BBEnergy_index, by="Date", all=T)
a<- merge(a, BBIndustrial_index, by="Date", all=T)
a<- merge(a, Brent1, by="Date", all=T)
a<- merge(a, Brent2, by="Date", all=T)
a<- merge(a, Brent3, by="Date", all=T)
a<- merge(a, Brent6, by="Date", all=T)
a<- merge(a, Copper_index, by="Date", all=T)
a<- merge(a, Copper_price, by="Date", all=T)
a<- merge(a, Gas_Price, by="Date", all=T)
a<- merge(a, Gold_Price, by="Date", all=T)
a<- merge(a, Iron_Index, by="Date", all=T)
a<- merge(a, Iron_Index2, by="Date", all=T)
a<- merge(a, Iron_Price, by="Date", all=T)
a<- merge(a, Lead_Index, by="Date", all=T)
a<- merge(a, Lead_Price, by="Date", all=T)
a<- merge(a, Nickel_Index, by="Date", all=T)
a<- merge(a, Nickel_Price, by="Date", all=T)
a<- merge(a, Oil_Price, by="Date", all=T)
a<- merge(a, Oil2_Price, by="Date", all=T)
a<- merge(a, Palladium_Price, by="Date", all=T)
a<- merge(a, Platinum_Price, by="Date", all=T)
a<- merge(a, Precious_metals_index, by="Date", all=T)
a<- merge(a, Silver_Price, by="Date", all=T)
a<- merge(a, SP_index, by="Date", all=T)
a<- merge(a, SP.Agri_index, by="Date", all=T)
a<- merge(a, SP.Agri.Live_index, by="Date", all=T)
a<- merge(a, SP.Aluminium_index, by="Date", all=T)
a<- merge(a, SP.Copper_index, by="Date", all=T)
a<- merge(a, SP.crude.oil_index, by="Date", all=T)
a<- merge(a, SP.Energy_index, by="Date", all=T)
a<- merge(a, SP.Gold_index, by="Date", all=T)
a<- merge(a, SP.Livestock_index, by="Date", all=T)
a<- merge(a, SP.Nat.Gas_index, by="Date", all=T)

a<- merge(a, SP.Nat.Gas_index, by="Date", all=T)
a<- merge(a, SPIndustrial_index, by="Date", all=T)
a<- merge(a, Steel_Index, by="Date", all=T)
a<- merge(a, Steel_Price, by="Date", all=T)
a<- merge(a, Tin_Index, by="Date", all=T)
a<- merge(a, Tin_Price, by="Date", all=T)
a<- merge(a, WTI1, by="Date", all=T)
a<- merge(a, WTI2, by="Date", all=T)
a<- merge(a, WTI3, by="Date", all=T)
a<- merge(a, WTI6, by="Date", all=T)
a<- merge(a, Zinc_Index, by="Date", all=T)
a<- merge(a, Zinc_Price, by="Date", all=T)

a$Date <- dmy(a$Date)


setwd("/bc_commodities/data/commodities")
write_rds(a, "datCOM.RDS")

