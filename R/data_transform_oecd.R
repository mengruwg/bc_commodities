library(reshape2)
library(zoo)
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

stationarise_oecd <- function(x) {
  x$Value <- log(x$Value)
  x <- dcast(x, TIME ~ LOCATION, value.var = "Value")
  x[-1, 2:ncol(x)] <- apply(x[2:ncol(x)], 2, diff)
  # drop first row
  x <- x[-1, ]
  
  return(x)
}

### gdp
gdp <- readRDS("data/oecd_gdp.rds")
gdp <- gdp[c("TIME", "LOCATION", "Value")]
gdp$TIME <- as.yearqtr(gdp$TIME, format = "%Y-Q%q")
# check data
plot_data(gdp)
gdp <- stationarise_oecd(gdp)
plot_stationary(gdp)

### inflation
inflation <- readRDS("data/oecd_inflation.rds")
inflation <- inflation[c("TIME", "LOCATION", "Value")]
inflation$TIME <- as.yearqtr(inflation$TIME, format = "%Y-Q%q")
# check data
plot_data(inflation)
inflation <- stationarise_oecd(inflation)
plot_stationary(inflation)

### m3
m3 <- readRDS("data/oecd_m3.rds")
m3 <- m3[c("TIME", "LOCATION", "Value")]
m3$TIME <- as.yearqtr(m3$TIME, format = "%Y-Q%q")
# check data
plot_data(m3)
m3 <- stationarise_oecd(m3)
plot_stationary(m3)
