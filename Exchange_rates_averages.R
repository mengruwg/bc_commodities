library(dplyr)
library(lubridate)

exchange_rates <- readRDS("C:/Users/cenge/Desktop/Money Credit Finance/Paper/bc_commodities/data/exchange_rates.rds")
quarter(exchange_rates$Date)
year(exchange_rates$Date)
