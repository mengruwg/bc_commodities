library(readr)
library(zoo)
library(ggplot2)

gdp = read_csv("data/oecd_gdp.csv")

gdp = gdp[, c("LOCATION", "TIME", "Value")]
gdp$LOCATION = as.factor(gdp$LOCATION)
gdp$TIME = as.yearqtr(gdp$TIME, format = "%Y-Q%q")

ggplot(gdp, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line()

countries = levels(gdp$LOCATION)

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

plot(gdps_hp[[1]])

# USA from 1955-Q1, Chile 1995-Q1, others 1960-Q1