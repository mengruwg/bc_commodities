library(WDI)
library(wbstats)
library(OECD)
library(quantmod)

countries = read.table("countries.txt")
countries = levels(countries$V1)

WDIsearch("inflation")

gdp = WDI(indicator = "NY.GDP.PCAP.KD", 
          country = countries, 
          start = 1960, end = 2012)
schooling = WDI(indicator = c("UIS.EA.MEAN.1T6.AG25T99", "BAR.TER.SCHL.15UP", "BAR.SEC.SCHL.15UP", "BAR.PRM.SCHL.15UP", "BAR.SCHL.15UP"),
                country = countries, 
                start=1960, end=2012)


gdp_AUS = getSymbols("NAEXKP01AUQ661S", src = "FRED")
gdp_ZAF = getSymbols("NAEXKP01ZAQ661S", src = "FRED")
gdp_NOR = getSymbols("NAEXKP01NOQ661S", src = "FRED")
gdp_CHL = getSymbols("NAEXKP01CLQ661S", src = "FRED")
gdp_USA = getSymbols("NAEXKP01USQ661S", src = "FRED")
gdp_DEU = getSymbols("NAEXKP01DEQ661S", src = "FRED")

??OECD

search_dataset("quarterly")
get_dataset("QNA", filter = list(c("AUS", "CHL", "NOR", "ZAF")))

oecd_gdp <- read_delim("data/oecd_gdp.csv", 
                       ";", escape_double = FALSE, col_types = cols(Value = col_double()), 
                       locale = locale(decimal_mark = ",", grouping_mark = "."), 
                       trim_ws = TRUE)
oecd_gdp = oecd_gdp[which(oecd_gdp$MEASURE == "PC_CHGPP"), ]
oecd_gdp$TIME = as.yearqtr(oecd_gdp$TIME, format = "%Y-Q%q")

x = oecd_gdp[which(oecd_gdp$LOCATION %in% c("AUS")), ]
#x = oecd_gdp[which(oecd_gdp$TIME > "1990-01-01"), ]

ggplot(x, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line() +
  geom_hline(yintercept = 0)


