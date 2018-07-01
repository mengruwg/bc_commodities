data <- readRDS("data/country_data.RDS")

plot_country <- function(x) {
  # remove some to circumvent 10 var limit
  x$import <- NULL
  x$i3m <- NULL
  x$TIME <- NULL
  plot(ts(x))
}

# lapply(data, function(x) {
#   nrow(x[complete.cases(x), ])
#   summary(x) # exchange rate has 135 NAs
#   nrow(x[complete.cases(x[-8]), -8])
#   summary(x)
# })

# Chile & Norway miss:
#   i10y
# Australia misses:
#   equity
# All miss:
#   i3m, import, trade, exch_rate

# Chile & Norway use i3m for missing mp_rate
# Germany uses M2

plot_country(data$AUS)
data_aus <- data$AUS[c("gdp", "infl", "i10y", "export", "m3", "mp_rate", "comm", "industr")]
data_aus <- data_aus[complete.cases(data_aus), ]

plot_country(data$CHL)
data_chl <- data$CHL[c("gdp", "infl", "export", "m3", "mp_rate", "comm", "industr", "equity")]
data_chl <- data_chl[complete.cases(data_chl), ]

plot_country(data$NOR)
data_nor <- data$NOR[c("gdp", "infl", "export", "m3", "mp_rate", "comm", "industr", "equity")]
data_nor <- data_nor[complete.cases(data_nor), ]

plot_country(data$ZAF)
data_zaf <- data$ZAF[c("gdp", "infl", "i10y", "export", "m3", "mp_rate", "comm", "industr", "equity")]
data_zaf <- data_zaf[complete.cases(data_zaf), ]

plot_country(data$DEU)
data_deu <- data$DEU[c("gdp", "infl", "i10y", "export", "m3", "mp_rate", "comm", "industr", "equity")]
data_deu <- data_deu[complete.cases(data_deu), ]

plot_country(data$USA)
data_usa <- data$USA[c("gdp", "infl", "i10y", "export", "m3", "mp_rate", "comm", "industr", "equity")]
data_usa <- data_usa[complete.cases(data_usa), ]

rm(data, plot_country)