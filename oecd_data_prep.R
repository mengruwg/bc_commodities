library(readr)

data_gdp <- read_csv("data/oecd_gdp.csv")
unique(data_gdp$Subject)

data <- read_csv("data/oecd_data.csv")
unique(data$Subject)

gdp = data_gdp[data_gdp$MEASURE == "CPCARSA", ]
gdp = gdp[which(gdp$FREQUENCY == "Q"), ]
gdp = gdp[which(gdp$SUBJECT == "B1_GE"), ]



# subset with consumer price index all items, we drop harmonised (CPHPTT01) due to limited availability
data_infl <- data[data$SUBJECT == "CPALTT01", ]
# measured as index with 2010 = 100
data_infl <- data_infl[data_infl$MEASURE == "IXOB", ]
# quarterly data due to availability
data_infl <- data_infl[data_infl$FREQUENCY == "Q", ]

ggplot(data_infl, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_point()


# subset with M3 monetary aggregate, as M2 not available
data_m3 <- data[data$SUBJECT == "MABMM301", ]
# measured as index with 2010 = 100
data_m3 <- data_m3[data_m3$MEASURE == "IXOBSA", ]
# quarterly data due to availability
data_m3 <- data_m3[data_m3$FREQUENCY == "Q", ]

ggplot(data_m3, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_point()
# DEU only available annually


# subset with 3 month Interbank rates
data_i3m <- data[data$SUBJECT == "IR3TIB01", ]
# Quarterly, just because
data_i3m <- data_i3m[data_i3m$FREQUENCY == "Q", ]

ggplot(data_i3m, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_point()
# Chile very late

# subset with 10 year government bond yields
data_i10y <- data[data$SUBJECT == "IRLTLT01", ]
# Quarterly, just because
data_i10y <- data_i10y[data_i10y$FREQUENCY == "Q", ]

ggplot(data_i10y, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_point()
# Chile very late


# subset with imports in USD, current prices, PPP & seasonally adjusted
data_im <- gdp[gdp$SUBJECT == "P6", ]
data_im <- data_im[data_im$MEASURE == "CPCARSA", ]
# Quarterly, just because
data_im <- data_im[data_im$FREQUENCY == "Q", ]

ggplot(data_im, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_point()


# subset with imports in USD, current prices, PPP & seasonally adjusted
data_ex <- gdp[gdp$SUBJECT == "P7", ]
data_ex <- data_ex[data_ex$MEASURE == "CPCARSA", ]
# Quarterly, just because
data_ex <- data_ex[data_ex$FREQUENCY == "Q", ]

ggplot(data_ex, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_point()


saveRDS(data_m3, "oecd_m3.rds")
saveRDS(data_infl, "oecd_inflation.rds")
saveRDS(data_im, "oecd_imports.rds")
saveRDS(data_i3m, "oecd_i3m_interbank.rds")
saveRDS(data_i10y, "oecd_i10y_government.rds")
saveRDS(data_im, "oecd_imports.rds")
saveRDS(data_ex, "oecd_exports.rds")

