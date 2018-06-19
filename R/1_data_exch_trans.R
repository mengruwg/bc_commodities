library(dplyr)
library(lubridate)
library(zoo)

exchange_rates <- readRDS("data/exchange_rates.rds")

#Custom function to use na.rm, for nested function
custommean <- function(x) {
  mean(x, na.rm = TRUE)
}

#Quarterly dates
exchange_rates$Date_q <- as.yearqtr(exchange_rates$Date)
#monthly dates
exchange_rates$Date_m <- as.yearmon(exchange_rates$Date)

#Means per quarter
exch_rates_means_q <- aggregate(exchange_rates[,2:7], list(exchange_rates$Date_q), custommean)
#Means per month
exch_rates_means_m <- aggregate(exchange_rates[,2:7], list(exchange_rates$Date_m), custommean)

names(exch_rates_means_m)[1] <- names(exch_rates_means_q)[1] <- "TIME"

#saving the fooking Data
saveRDS(exch_means_q, file = "data/IMF/exch_rates_q_mean.rds")
saveRDS(exch_means_m, file = "data/IMF/exch_rates_m_mean.rds")
