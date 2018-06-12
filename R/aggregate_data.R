indices <- readRDS("data/indices_stationary.rds")
exch_rates <- readRDS("data/exchRates_q_mean.rds")

data <- list(gdp, inflation, i10y, trade_balance, m3)
names(data) <- c("gdp", "infl", "i10y", "trade", "m3")

aus <- lapply(data, function(x) x$AUS)
aus <- data.frame(TIME = data$gdp$TIME, aus)
aus <- merge(aus, spgsci, by = "TIME")
aus <- aus[complete.cases(aus), ]
