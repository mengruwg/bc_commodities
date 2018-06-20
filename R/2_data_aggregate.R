library(zoo)

# Aggregate all our data into a manageable format on per-country basis

# Dataframes of our commodity indices in a list
indices <- readRDS("data/indices_stationary.rds") # deprecated
# A dataframe of exchange rates
exch_rates <- readRDS("data/exch_rates_q_mean.rds")
# Dataframes of monetary policy rates in a list
mp_rates <- readRDS("data/raw_data/mp_rates.rds")
# Dataframes of OECD data in a list
oecd_data <- readRDS("data/oecd_logdiff.rds")
# Stock indices
stock_indices <- readRDS("data/stocks_logdiff.rds")


# Australia ---------------------------------------------------------------

AUS <- lapply(oecd_data, function(x) x$AUS)
# this works cause all of the dataframes here are the same length
AUS <- data.frame(TIME = oecd_data$gdp$TIME, AUS)

# add AUD exchange rate
AUS <- merge(AUS, exch_rates[c("TIME", "AUD")], by = "TIME", all.x = TRUE)
names(AUS)[which(names(AUS) == "AUD")] <- "exch_rate"

# add MP rate
AUS <- merge(AUS, mp_rates$aus_mp_rate, by = "TIME", all.x = TRUE)
names(AUS)[which(names(AUS) == "Value")] <- "mp_rate"
# carry over the last value if the rate wasn't adjusted
AUS$mp_rate <- na.locf(AUS$mp_rate, na.rm = FALSE)
# use i3m for missing values
i3m <- readRDS("data/raw_data/oecd_i3m_interbank.rds")
i3m <- i3m[i3m$LOCATION == "AUS", ]
i3m <- i3m[c("TIME", "Value")]
i3m$TIME <- as.yearqtr(i3m$TIME, format = "%Y-Q%q")
AUS <- merge(AUS, i3m, by = "TIME", all.x = TRUE)
AUS[is.na(AUS$mp_rate), ]$mp_rate <- AUS[is.na(AUS$mp_rate), ]$Value
AUS$Value <- NULL

# add the SPGSCI
AUS <- merge(AUS, indices$spgsci, by = "TIME", all.x = TRUE)
names(AUS)[which(names(AUS) == "Value")] <- "spgsci"

# add stock indices
AUS <- merge(AUS, stock_indices[c("TIME", "AUS")], by = "TIME", all.x = TRUE)
names(AUS)[which(names(AUS) == "AUS")] <- "equity"

#AUS <- AUS[complete.cases(AUS), ]


# Chile ---------------------------------------------------------------

CHL <- lapply(oecd_data, function(x) x$CHL)
# this works cause all of the dataframes here are the same length
CHL <- data.frame(TIME = oecd_data$gdp$TIME, CHL)

# add AUD exchange rate
CHL <- merge(CHL, exch_rates[c("TIME", "CLP")], by = "TIME", all.x = TRUE)
names(CHL)[which(names(CHL) == "CLP")] <- "exch_rate"

# add MP rate
CHL <- merge(CHL, mp_rates$chl_mp_rate, by = "TIME", all.x = TRUE)
names(CHL)[which(names(CHL) == "Value")] <- "mp_rate"
# carry over the last value if the rate wasn't adjusted
CHL$mp_rate <- na.locf(CHL$mp_rate, na.rm = FALSE)

# add the SPGSCI
CHL <- merge(CHL, indices$spgsci, by = "TIME", all.x = TRUE)
names(CHL)[which(names(CHL) == "Value")] <- "spgsci"

# add stock indices
CHL <- merge(CHL, stock_indices[c("TIME", "CHL")], by = "TIME", all.x = TRUE)
names(CHL)[which(names(CHL) == "CHL")] <- "equity"

#CHL <- CHL[complete.cases(CHL), ]


# Germany ---------------------------------------------------------------

DEU <- lapply(oecd_data, function(x) x$DEU)
# this works cause all of the dataframes here are the same length
DEU <- data.frame(TIME = oecd_data$gdp$TIME, DEU)

# add AUD exchange rate
DEU <- merge(DEU, exch_rates[c("TIME", "DEM")], by = "TIME", all.x = TRUE)
names(DEU)[which(names(DEU) == "DEM")] <- "exch_rate"

# add MP rate
DEU <- merge(DEU, mp_rates$deu_mp_rate, by = "TIME", all.x = TRUE)
names(DEU)[which(names(DEU) == "Value")] <- "mp_rate"
# carry over the last value if the rate wasn't adjusted
DEU$mp_rate <- na.locf(DEU$mp_rate, na.rm = FALSE)

# add the SPGSCI9
DEU <- merge(DEU, indices$spgsci, by = "TIME", all.x = TRUE)
names(DEU)[which(names(DEU) == "Value")] <- "spgsci"

# add stock indices
DEU <- merge(DEU, stock_indices[c("TIME", "DEU")], by = "TIME", all.x = TRUE)
names(DEU)[which(names(DEU) == "DEU")] <- "equity"

#DEU <- DEU[complete.cases(DEU), ]


# Norway ---------------------------------------------------------------

NOR <- lapply(oecd_data, function(x) x$NOR)
# this works cause all of the dataframes here are the same length
NOR <- data.frame(TIME = oecd_data$gdp$TIME, NOR)

# add AUD exchange rate
NOR <- merge(NOR, exch_rates[c("TIME", "NOK")], by = "TIME", all.x = TRUE)
names(NOR)[which(names(NOR) == "NOK")] <- "exch_rate"

# add MP rate
NOR <- merge(NOR, mp_rates$nor_mp_rate, by = "TIME", all.x = TRUE)
names(NOR)[which(names(NOR) == "Value")] <- "mp_rate"
# carry over the last value if the rate wasn't adjusted
NOR$mp_rate <- na.locf(NOR$mp_rate, na.rm = FALSE)
# use i3m for missing values
i3m <- readRDS("data/raw_data/oecd_i3m_interbank.rds")
i3m <- i3m[i3m$LOCATION == "NOR", ]
i3m <- i3m[c("TIME", "Value")]
i3m$TIME <- as.yearqtr(i3m$TIME, format = "%Y-Q%q")
NOR <- merge(NOR, i3m, by = "TIME", all.x = TRUE)
NOR[is.na(NOR$mp_rate), ]$mp_rate <- NOR[is.na(NOR$mp_rate), ]$Value
NOR$Value <- NULL

# add the SPGSCI
NOR <- merge(NOR, indices$spgsci, by = "TIME", all.x = TRUE)
names(NOR)[which(names(NOR) == "Value")] <- "spgsci"

# add stock indices
NOR <- merge(NOR, stock_indices[c("TIME", "NOR")], by = "TIME", all.x = TRUE)
names(NOR)[which(names(NOR) == "NOR")] <- "equity"

#NOR <- NOR[complete.cases(NOR), ]


# United States ---------------------------------------------------------------

USA <- lapply(oecd_data, function(x) x$USA)
# this works cause all of the dataframes here are the same length
USA <- data.frame(TIME = oecd_data$gdp$TIME, USA)

# add AUD exchange rate
USA <- merge(USA, exch_rates[c("TIME", "USD")], by = "TIME", all.x = TRUE)
names(USA)[which(names(USA) == "USD")] <- "exch_rate"

# add MP rate
USA <- merge(USA, mp_rates$usa_mp_rate, by = "TIME", all.x = TRUE)
names(USA)[which(names(USA) == "Value")] <- "mp_rate"
# carry over the last value if the rate wasn't adjusted
USA$mp_rate <- na.locf(USA$mp_rate, na.rm = FALSE)

# add the SPGSCI
USA <- merge(USA, indices$spgsci, by = "TIME", all.x = TRUE)
names(USA)[which(names(USA) == "Value")] <- "spgsci"

# add stock indices
USA <- merge(USA, stock_indices[c("TIME", "USA")], by = "TIME", all.x = TRUE)
names(USA)[which(names(USA) == "USA")] <- "equity"

#USA <- USA[complete.cases(USA), ]


# South Africa ---------------------------------------------------------------

ZAF <- lapply(oecd_data, function(x) x$ZAF)
# this works cause all of the dataframes here are the same length
ZAF <- data.frame(TIME = oecd_data$gdp$TIME, ZAF)

# add AUD exchange rate
ZAF <- merge(ZAF, exch_rates[c("TIME", "ZAR")], by = "TIME", all.x = TRUE)
names(ZAF)[which(names(ZAF) == "ZAR")] <- "exch_rate"

# add MP rate
ZAF <- merge(ZAF, mp_rates$zaf_mp_rate, by = "TIME", all.x = TRUE)
names(ZAF)[which(names(ZAF) == "Value")] <- "mp_rate"
# carry over the last value if the rate wasn't adjusted
ZAF$mp_rate <- na.locf(ZAF$mp_rate, na.rm = FALSE)

# add the SPGSCI
ZAF <- merge(ZAF, indices$spgsci, by = "TIME", all.x = TRUE)
names(ZAF)[which(names(ZAF) == "Value")] <- "spgsci"

# add stock indices
ZAF <- merge(ZAF, stock_indices[c("TIME", "ZAF")], by = "TIME", all.x = TRUE)
names(ZAF)[which(names(ZAF) == "ZAF")] <- "equity"


#ZAF <- ZAF[complete.cases(ZAF), ]



# All ---------------------------------------------------------------------

data <- list(AUS, CHL, DEU, NOR, USA, ZAF)
names(data) <- c("AUS", "CHL", "DEU", "NOR", "USA", "ZAF")

saveRDS(data, "data/country_data.rds")
