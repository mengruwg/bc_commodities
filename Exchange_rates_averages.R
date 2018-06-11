library(dplyr)
library(lubridate)
library(zoo)


exchange_rates <- readRDS("C:/Users/cenge/Desktop/Money Credit Finance/Paper/bc_commodities/data/exchange_rates.rds")

#Custom function to use na.rm, for nested function
custommean<- function(x){mean(x, na.rm=T)}

#Quarterly dates
exchange_rates$Date2<- as.yearqtr(exchange_rates$Date)
#monthly dates
exchange_rates$Date3<- as.yearmon(exchange_rates$Date)

#Means per quarter
means<- aggregate(exchange_rates[,2:7], list(exchange_rates$Date2),custommean)
#Means per month
means2<- aggregate(exchange_rates[,2:7], list(exchange_rates$Date3),custommean)

#saving the fooking Data
setwd("data/IMF")

saveRDS(means, file="exchange_rates_quarterly")
saveRDS(means2, file="exchange_rates_monthly")

