require(vars)

source("R/2_VAR_country_setup.R")

#data <- data_zaf[c(1, 5, 2)]

data <- data_usa[c("gdp", "trade", "spgsci", "infl", "m3", "mp_rate", "i10y", "equity")]
data <- data_deu[c("gdp", "trade", "spgsci", "infl", "m3", "mp_rate", "i10y", "equity")]
data <- data_zaf[c("gdp", "trade", "spgsci", "infl", "m3", "mp_rate", "i10y", "equity")]
data <- data_nor[c("gdp", "trade", "spgsci", "infl", "m3", "mp_rate", "equity")]
data <- data_aus[c("gdp", "trade", "spgsci", "infl", "m3", "mp_rate", "i10y")]
data <- data_nor[c("gdp", "trade", "spgsci", "infl", "m3", "mp_rate")]

data <- apply(data, 2, scale)


# vars --------------------------------------------------------------------

# maybe change IC
model <- VAR(y = data, lag.max = 8, ic = "AIC")

lag <- model$p

# restricted irf
plot(irf(model))
# irf under long-run restrictions
#plot(irf(BQ(model)))
