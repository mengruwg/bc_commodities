library(readr)
library(zoo)
library(ggplot2)
library(reshape2)
library(tseries)

gdp = read_csv("data/oecd_gdp.csv")
gdp = gdp[which(gdp$MEASURE == "CPCARSA"), ]
gdp = gdp[which(gdp$FREQUENCY == "Q"), ]
gdp = gdp[which(gdp$SUBJECT == "B1_GE"), ]

gdp = gdp[, c("LOCATION", "TIME", "Value")]
gdp$LOCATION = as.factor(gdp$LOCATION)
gdp$TIME = as.yearqtr(gdp$TIME, format = "%Y-Q%q")
gdp$Value = log(gdp$Value)

ggplot(gdp, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line()# +
  scale_y_log10()

data <- dcast(gdp, TIME ~ LOCATION, value.var = "Value")
data[-1, 2:7] <- apply(data[2:7], 2, diff)
data <- data[-1, ]

ggplot(data, aes(x = TIME)) +
  geom_line(aes(y = USA))

df <- melt(data, id = "TIME")
names(df) <- c("TIME", "LOCATION", "Value")

ggplot(df[which(df$LOCATION != "USA" & df$LOCATION != "DEU"), ], aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line()
ggplot(df, aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line()

# Dickey Fuller
for(i in 2:ncol(data)) {
  print(colnames(data[i]))
  print(adf.test(data[!is.na(data[, i]), i], alternative = c("stationary")))
}
