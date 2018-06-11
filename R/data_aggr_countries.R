data <- list(gdp, inflation, i10y, trade_balance, m3)
names(data) <- c("gdp", "infl", "i10y", "trade", "m3")

aus <- lapply(data, function(x) x$AUS)
aus <- data.frame(TIME = data$gdp$TIME, aus)
aus <- aus[complete.cases(aus), ]
