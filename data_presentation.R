rm(list = ls())

library(zoo)
library(readr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(pracma)
library(mFilter)

### Commodities

data_ind = read_delim("data/data_indices.csv", 
                          ";", 
                          escape_double = FALSE, 
                          locale = locale(decimal_mark = ",", 
                                          grouping_mark = "."), 
                          trim_ws = TRUE, 
                          skip = 1)

# data_com = read_delim("data/data_commodities.csv", 
#                               ";", 
#                               escape_double = FALSE, 
#                               locale = locale(decimal_mark = ",", 
#                                               grouping_mark = "."), 
#                               trim_ws = TRUE, 
#                               skip = 1)
# 
# data_oil = read_delim("data/data_oilfutures.csv", 
#                              ";", 
#                              escape_double = FALSE, 
#                              locale = locale(decimal_mark = ",", 
#                                              grouping_mark = "."), 
#                              trim_ws = TRUE, 
#                              skip = 1)

bloomberg_import = function(x) {
  out = vector("list", ncol(x) / 2)

  for (i in seq(1, ncol(x), 2)) {
    out[[(i + 1) / 2]] = na.omit(x[, i:(i + 1)])

    index_name = substr(names(x[, i + 1]), 1, 
                        regexpr(" ", names(x[, i + 1]))[1] - 1)
    names(out[[(i + 1) / 2]]) = c("Date", index_name)
    
    out[[(i + 1) / 2]]$Date = as.character(out[[(i + 1) / 2]]$Date)
    out[[(i + 1) / 2]]$Date = as.Date(out[[(i + 1) / 2]]$Date, "%d%m%Y")
    
    names(out)[(i + 1) / 2] = index_name
  }
  
  return(out)
}

data_ind = bloomberg_import(data_ind)
data_ind = Reduce(function(...) merge(..., all = TRUE), data_ind)

data_ind_re = data.frame(mapply(`/`, data_ind[, 2:ncol(data_ind)], 
                                as.numeric(data_ind[253, 2:ncol(data_ind)]))) * 100
data_ind_re = data.frame(data_ind$Date, data_ind_re)
names(data_ind_re)[1] = "Date"

# data_com = bloomberg_import(data_com)
# data_com = Reduce(function(...) merge(..., all = TRUE), data_com)
# 
# data_oil = bloomberg_import(data_oil)
# data_oil = Reduce(function(...) merge(..., all = TRUE), data_oil)

tail(data_ind_re)

df = melt(data_ind_re[, which(names(data_ind_re) %in% 
                             c("Date", "SPGSCI", "BCOM", "SPGSCL"))], 
          id = "Date")

ind_line = ggplot(df, aes(x = Date, y = value)) +
  geom_line(aes(colour = variable), size = 0.5) +
  ggtitle("Commodity Indices") +
  scale_y_continuous(breaks = c(0, 100, 200, 400, 600)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme_fivethirtyeight() +
  scale_color_gdocs(name = NULL)

### GDP

gdp = read_csv("data/oecd_gdp.csv")

gdp = gdp[which(gdp$MEASURE == "CPCARSA"), ]
gdp = gdp[which(gdp$FREQUENCY == "Q"), ]
nettrade = gdp[which(gdp$SUBJECT %in% c("P6", "P7")), ]
gdp = gdp[which(gdp$SUBJECT == "B1_GE"), ]
gdp = gdp[, c("LOCATION", "TIME", "Value")]

gdp$Date = as.yearqtr(gdp$TIME, format = "%Y-Q%q"); gdp$TIME = NULL
gdp$Value = log(gdp$Value)

ggplot(gdp, aes(x = Date, y = Value, colour = LOCATION)) +
  geom_line()

countries = unique(gdp$LOCATION)

gdps = vector("list", length(countries))
gdps_hp = vector("list", length(countries))
y = vector("list", length(countries))

idx = 1
for(i in countries){
  gdps[[idx]] = gdp[which(gdp$LOCATION == i), ]
  gdps_hp[[idx]] = hpfilter(gdps[[idx]]$Value, freq = 1600)
  y[[idx]] = gdps_hp[[idx]]$cycle
  idx = idx + 1
}; rm(idx, i)

names(gdps_hp) = countries
names(gdps) = countries

# Accumulate different detrending methods in a list unelegantly
{
AUS = gdps$AUS[, -1]
CHL = gdps$CHL[, -1]
NOR = gdps$NOR[, -1]
ZAF = gdps$ZAF[, -1]
DEU = gdps$DEU[, -1]
USA = gdps$USA[, -1]

AUS$hp_cycle = gdps_hp$AUS$cycle
CHL$hp_cycle = gdps_hp$CHL$cycle
NOR$hp_cycle = gdps_hp$NOR$cycle
ZAF$hp_cycle = gdps_hp$ZAF$cycle
DEU$hp_cycle = gdps_hp$DEU$cycle
USA$hp_cycle = gdps_hp$USA$cycle

AUS$diff = c(0, diff(AUS$Value))
CHL$diff = c(0, diff(CHL$Value))
NOR$diff = c(0, diff(NOR$Value))
ZAF$diff = c(0, diff(ZAF$Value))
DEU$diff = c(0, diff(DEU$Value))
USA$diff = c(0, diff(USA$Value))

AUS$lin = detrend(AUS$Value)
CHL$lin = detrend(CHL$Value)
NOR$lin = detrend(NOR$Value)
ZAF$lin = detrend(ZAF$Value)
DEU$lin = detrend(DEU$Value)
USA$lin = detrend(USA$Value)

gdps[[1]] = AUS
gdps[[2]] = CHL
gdps[[3]] = NOR
gdps[[4]] = ZAF
gdps[[5]] = DEU
gdps[[6]] = USA
rm(AUS, CHL, NOR, ZAF, DEU, USA)
}

charts = paste("Detrending", names(gdps))
detrend_line = vector("list", length(gdps))

i = 1
for(country in gdps) {
  df = melt(country, id = "Date")
  df$Date = as.Date(df$Date)
  
  detrend_line[[i]] = ggplot(df, aes(x = Date, y = value, colour = variable)) +
    geom_line(size = 0.5) +
    ggtitle(charts[i]) +
    scale_y_continuous(breaks = c(-0.3, -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 0.3), 
                       expand = c(0, 0)) +
    coord_cartesian(ylim = c(-0.3, 0.3)) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    theme_fivethirtyeight() +
    scale_color_gdocs(name = NULL)
  i = i + 1
}

detrend_line
