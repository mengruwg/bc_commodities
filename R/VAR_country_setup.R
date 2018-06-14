data <- readRDS("data/country_data.RDS")

lapply(data, function(x) {
  nrow(x[complete.cases(x), ])
  summary(x) # exchange rate has 135 NAs
  nrow(x[complete.cases(x[-8]), -8])
  summary(x)
})

# Chile & Norway miss:
#   i3m, i10y, exch_rate
# Others miss:
#   i3m, exch_rate

# Chile & Norway use i3m for missing mp_rate
# Germany uses M2

data_aus <- data$AUS[c(-1, -5, -8)]
data_aus <- data_aus[complete.cases(data_aus), ]

data_chl <- data$CHL[c(-1, -4, -5, -8)]
data_chl <- data_chl[complete.cases(data_chl), ]

data_nor <- data$NOR[c(-1, -4, -5, -8)]
data_nor <- data_nor[complete.cases(data_nor), ]

data_zaf <- data$ZAF[c(-1, -5, -8)]
data_zaf <- data_zaf[complete.cases(data_zaf), ]

data_deu <- data$DEU[c(-1, -5, -8)]
data_deu <- data_deu[complete.cases(data_deu), ]

data_usa <- data$USA[c(-1, -5, -8)]
data_usa <- data_usa[complete.cases(data_usa), ]

rm(data)
