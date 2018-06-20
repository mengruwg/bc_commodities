library(reshape2)
library(zoo)
library(readr)
library(ggplot2)

plot_data <- function(x) {
  ggplot(x, aes(x = TIME, y = Value, colour = LOCATION)) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = "Set1")
}

plot_stationary <- function(x) {
  x <- melt(x, id = "TIME")
  ggplot(x, aes(x = TIME, y = value, colour = variable)) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    geom_smooth(method = "lm")
}

unmelt_oecd <- function(x) {
  x <- dcast(x, TIME ~ LOCATION, value.var = "Value")
  x <- x[-1, ]
}

stationarise_oecd <- function(x) {
  x$Value <- log(x$Value)
  x <- dcast(x, TIME ~ LOCATION, value.var = "Value")
  x[-1, 2:ncol(x)] <- apply(x[2:ncol(x)], 2, diff)
  # drop first row
  x <- x[-1, ]
  
  return(x)
}

### gdp
gdp <- readRDS("data/raw_data/oecd_gdp.rds")
gdp <- gdp[c("TIME", "LOCATION", "Value")]
gdp$TIME <- as.yearqtr(gdp$TIME, format = "%Y-Q%q")
# check data
plot_data(gdp)
gdp <- stationarise_oecd(gdp)
plot_stationary(gdp)

### inflation
inflation <- readRDS("data/raw_data/oecd_inflation.rds")
inflation <- inflation[c("TIME", "LOCATION", "Value")]
inflation$TIME <- as.yearqtr(inflation$TIME, format = "%Y-Q%q")
# check data
plot_data(inflation)
inflation <- stationarise_oecd(inflation)
plot_stationary(inflation)

# looks pretty bad for CHL, but we will only look at 1990 onwards for it

### m3
m3 <- readRDS("data/raw_data/oecd_m3.rds")
m3 <- m3[c("TIME", "LOCATION", "Value")]
m3$TIME <- as.yearqtr(m3$TIME, format = "%Y-Q%q")
# lets add Datastreams M2 for Germany
ger_m2 <- read_delim("data/datastream/ger_m2.csv", 
                     ";", 
                     escape_double = FALSE, 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     col_types = cols(Original = col_skip()),
                     trim_ws = TRUE)
# We rebase with 2010 = 100
ger_m2$LOCATION <- "DEU"
ger_m2$TIME <- as.yearqtr(ger_m2$TIME, format = "Q%q %Y")
m3 <- rbind(ger_m2, m3)
# check data
plot_data(m3)
m3 <- stationarise_oecd(m3)
plot_stationary(m3)

# Germany has a bad jump at 1995, but I guess better than nothing

### imports
imports <- readRDS("data/raw_data/oecd_imports.rds")
imports <- imports[c("TIME", "LOCATION", "Value")]
imports$TIME <- as.yearqtr(imports$TIME, format = "%Y-Q%q")
# check data
plot_data(imports)
imports <- stationarise_oecd(imports)
plot_stationary(imports)

### exports
exports <- readRDS("data/raw_data/oecd_exports.rds")
exports <- exports[c("TIME", "LOCATION", "Value")]
exports$TIME <- as.yearqtr(exports$TIME, format = "%Y-Q%q")
# check data
plot_data(exports)
exports <- stationarise_oecd(exports)
plot_stationary(exports)

### trade balance
trade_balance <- imports[1]
trade_balance[2:7] <- exports[2:7] - imports[2:7]
names(trade_balance) <- names(imports)
# check out data
plot_stationary(trade_balance)



### i10y
i10y <- readRDS("data/raw_data/oecd_i10y_government.rds")
i10y <- i10y[c("TIME", "LOCATION", "Value")]
i10y$TIME <- as.yearqtr(i10y$TIME, format = "%Y-Q%q")
# check data
plot_data(i10y)
i10y <- unmelt_oecd(i10y)
#i10y <- stationarise_oecd(i10y)
#plot_stationary(i10y)

### i3m
i3m <- readRDS("data/raw_data/oecd_i3m_interbank.rds")
i3m <- i3m[c("TIME", "LOCATION", "Value")]
i3m$TIME <- as.yearqtr(i3m$TIME, format = "%Y-Q%q")
# check data
plot_data(i3m)
i3m <- unmelt_oecd(i3m)
#i3m <- stationarise_oecd(i3m)
#plot_stationary(i3m)

# We can't really calculate a yield spread from these two interest rates

oecd_data <- list(gdp, inflation, i10y, i3m, trade_balance, m3)
names(oecd_data) <- c("gdp", "infl", "i10y", "i3m", "trade", "m3")

saveRDS(oecd_data, "data/oecd_logdiff.rds")
