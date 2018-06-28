# Einlesen Gesamtdaten Commodities & Indices und Quarterly data
data_comm <- readRDS("data/raw_data/comm_mean_qu.rds")

# Daten Subsetten auf Daten vorhanden ab 1970.
# ab 70: Date, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, TinIndex, ZincIndex,
# ab 75: Date, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex SP.prec.metIndex, TinIndex, ZincIndex,
# ab 80: Date, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.CopperIndex, SP.GoldIndex, SPIndustrialIndex,SP.LivestockIndex, TinIndex, ZincIndex,
# ab 85: Date, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, Oil2Price, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.CopperIndex, Sp.EnergyIndex, SP.GoldIndex,SP.LivestockIndex,SP.prec.metIndex,SPIndustrialIndex, TinIndex, ZincIndex,

for(i in seq(2, ncol(data_comm), 9)) {
  if(i + 8 < ncol(data_comm)) {
    x <- data_comm[c(1, seq(i, i + 8))]
  } else {
    x <- data_comm[c(1, seq(i, ncol(data_comm)))]
  }
  plot(ts(x))
}

# TinIndex & Brent3yr with gaps

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
  ZincIndex))

