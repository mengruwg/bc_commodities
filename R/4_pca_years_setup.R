#Einlesen Gesamtdaten Commodities & Indices und Quarterly data
datCOM <- readRDS("datCOM_qu.rds")

#Daten Subsetten auf Daten vorhanden ab 1970. 
#ab 70: Group.1, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, TinIndex, ZincIndex, 
#ab 75: Group.1, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex SP.prec.metIndex, TinIndex, ZincIndex, 
#ab 80: Group.1, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.CopperIndex, SP.GoldIndex, SPIndustrialIndex,SP.LivestockIndex, TinIndex, ZincIndex, 
#ab 85: Group.1, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, Oil2Price, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.CopperIndex, Sp.EnergyIndex, SP.GoldIndex,SP.LivestockIndex,SP.prec.metIndex,SPIndustrialIndex, TinIndex, ZincIndex, 

dat1970 <- subset(datCOM, select= c(Group.1, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, TinIndex, ZincIndex))
dat1975 <- subset(datCOM, select= c(Group.1, CopperIndex, GoldPrice, LeadIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.LivestockIndex, SP.prec.metIndex, TinIndex, ZincIndex))
dat1980 <- subset(datCOM, select= c(Group.1, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.GoldIndex, SPIndustrialIndex,SP.LivestockIndex, TinIndex, ZincIndex))
dat1985 <- subset(datCOM, select= c(Group.1, AluIndex, CopperIndex, GoldPrice, LeadIndex, NickelIndex, Oil2Price, SilverPrice, SPIndex, SP.AgriIndex, SP.Agri.LiveIndex, SP.EnergyIndex, SP.GoldIndex,SP.LivestockIndex,SP.prec.metIndex,SPIndustrialIndex, TinIndex, ZincIndex))                

