require(tseries)

data <- readRDS("data/country_data.rds")

apply(data$AUS[2:ncol(data$AUS)], 2, function(x) {
  adf.test(na.omit(x))$p.value > 0.05
})

acf(data$AUS$infl)
