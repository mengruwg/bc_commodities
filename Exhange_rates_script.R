rm(list = ls())

library(stringr)
library(readr)
#Data einlesen
IMF_Data <- read_delim("data/IMF/IMF_Data.csv",";", escape_double = FALSE, col_types = cols(`Euro   (EUR)` = col_double()),locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
columns <- c("Date", "AUD", "CLP", "DEM", "EUR", "NOK", "ZAR", "USD")
#Column names for easier handling of data
colnames(IMF_Data)<-columns
rm(columns)
#Euro Löschen
IMF_Data$EUR<- NULL


#Monat und Jahr Variable erstellen
IMF_Data$Year<-str_sub(IMF_Data$Date, -2)
Date<- str_split_fixed(IMF_Data$Date, "-", 2)
IMF_Data$Month<-str_sub(Date[,2], 1,3)
IMF_Data$Day<-Date[,1]
IMF_Data$Date<-paste(IMF_Data$Day,"-",IMF_Data$Month,"-",IMF_Data$Year)

IMF_Data$Month[IMF_Data$Month==1] <- "01"
IMF_Data$Month[IMF_Data$Month==2] <- "02"
IMF_Data$Month[IMF_Data$Month==3] <- "03"
IMF_Data$Month[IMF_Data$Month==4] <- "04"
IMF_Data$Month[IMF_Data$Month==5] <- "05"
IMF_Data$Month[IMF_Data$Month==6] <- "06"
IMF_Data$Month[IMF_Data$Month==7] <- "07"
IMF_Data$Month[IMF_Data$Month==8] <- "08"
IMF_Data$Month[IMF_Data$Month==9] <- "09"
IMF_Data$Month[IMF_Data$Month==10] <- "10"
IMF_Data$Month[IMF_Data$Month==11] <- "11"
IMF_Data$Month[IMF_Data$Month==12] <- "12"


IMF_Data$Date<-paste(IMF_Data$Day,"-",IMF_Data$Month,"-",IMF_Data$Year)

IMF_Data$Date2<- strptime(IMF_Data$Date, format="%e - %m - %y")

IMF_Data$Date<- IMF_Data$Date2

#Bereinigen Werte Deutschland
IMF_Data$DEM[IMF_Data$DEM==0] <- NA

saveRDS(IMF_Data[,1:7], "exchange_rates.rds")
