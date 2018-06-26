# Einlesen Gesamtdaten Commodities & Indices und Quarterly data
data_comm <- readRDS("data/raw_data/comm_mean_qu.rds")

# Daten Subsetten auf Daten vorhanden ab 1970. 
# ab 70: Date, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, TinIndex, ZincIndex, 
# ab 75: Date, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex SP.prec.metIndex, TinIndex, ZincIndex, 
# ab 80: Date, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.CopperIndex, SP.GoldIndex, SPIndustrialIndex,SP.LivestockIndex, TinIndex, ZincIndex, 
# ab 85: Date, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, Oil2Price, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.CopperIndex, Sp.EnergyIndex, SP.GoldIndex,SP.LivestockIndex,SP.prec.metIndex,SPIndustrialIndex, TinIndex, ZincIndex, 


dat1970 <- subset(data_comm, select = c(
  Date,
  CopperIndex,
  GoldPrice,
  LeadIndex,
  SilverPrice,
  SPIndex,
  SP.AgriIndex,
  SP.Agri.LiveIndex,
  SP.LivestockIndex,
  TinIndex,
  ZincIndex))

dat1975 <- subset(data_comm, select = c(
  Date,
  CopperIndex,
  GoldPrice,
  LeadIndex,
  SilverPrice,
  SPIndex,
  SP.AgriIndex,
  SP.Agri.LiveIndex,
  SP.LivestockIndex,
  SP.prec.metIndex,
  TinIndex,
  ZincIndex))

dat1980 <- subset(data_comm, select = c(
  Date,
  AluIndex,
  CopperIndex,
  GoldPrice,
  LeadIndex,
  NickelIndex,
  SilverPrice,
  SPIndex,
  SP.AgriIndex,
  SP.Agri.LiveIndex,
  SP.GoldIndex,
  SPIndustrialIndex,
  SP.LivestockIndex,
  TinIndex,
  ZincIndex))

dat1985 <- subset(data_comm, select = c(
  Date,
  AluIndex,
  CopperIndex,
  GoldPrice,
  LeadIndex,
  NickelIndex,
  Oil2Price,
  SilverPrice,
  SPIndex,
  SP.AgriIndex,
  SP.Agri.LiveIndex,
  SP.EnergyIndex,
  SP.GoldIndex,
  SP.LivestockIndex,
  SP.prec.metIndex,
  SPIndustrialIndex,
  TinIndex,
  ZincIndex))

