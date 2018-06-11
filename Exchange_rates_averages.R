library(dplyr)
library(lubridate)
library(zoo)


exchange_rates <- readRDS("C:/Users/cenge/Desktop/Money Credit Finance/Paper/bc_commodities/data/exchange_rates.rds")

custommean<- function(x){mean(x, na.rm=T)}

means<- aggregate(exchange_rates[,2:7], list(exchange_rates$Date2),custommean)


