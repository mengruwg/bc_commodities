data <- readRDS("data/country_data.RDS")

plot_country <- function(x) {
  # remove some to circumvent 10 var limit
  x <- x[c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "i10y", "equity")[which(c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "i10y", "equity") %in% names(x))]]
  
  plot(ts(x))
}

# lapply(data, function(x) {
#   nrow(x[complete.cases(x), ])
#   summary(x) # exchange rate has 135 NAs
#   nrow(x[complete.cases(x[-8]), -8])
#   summary(x)
# })

# Chile misses:
#   i10y
# Australia misses:
#   equity
# All miss:
#   i3m, import, trade, exch_rate

# Chile & Norway use i3m for missing mp_rate
# Germany uses M2

plot_country(data$AUS)
data_aus <- data$AUS[c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "i10y")]
data_aus <- data_aus[complete.cases(data_aus), ]

plot_country(data$CHL)
data_chl <- data$CHL[c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "equity")]
data_chl <- data_chl[complete.cases(data_chl), ]

plot_country(data$NOR)
data_nor <- data$NOR[c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "i10y", "equity")]
data_nor <- data_nor[complete.cases(data_nor), ]

plot_country(data$ZAF)
data_zaf <- data$ZAF[c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "i10y", "equity")]
data_zaf <- data_zaf[complete.cases(data_zaf), ]

plot_country(data$DEU)
data_deu <- data$DEU[c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "i10y", "equity")]
data_deu <- data_deu[complete.cases(data_deu), ]

plot_country(data$USA)
data_usa <- data$USA[c("comm", "industr", "gdp", "infl", "export", "m3", "mp_rate", "i10y", "equity")]
data_usa <- data_usa[complete.cases(data_usa), ]

rm(data, plot_country)