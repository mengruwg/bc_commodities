library(ggplot2)
library(zoo)
library(reshape2)

plot_stocks <- function(x) {
  x <- melt(x, id = "TIME")
  ggplot(x, aes(x = TIME, y = value, colour = variable)) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    geom_smooth(method = "lm")
}



stock_indices <- readRDS("data/raw_data/stock_indices.rds")
stock_indices$TIME <- as.yearqtr(stock_indices$TIME, format = "Q%q %Y")

plot_stocks(stock_indices)

stock_indices[-1, 2:ncol(stock_indices)] <- apply(stock_indices[2:ncol(stock_indices)], 2, function(x) {
  x <- diff(log(x))
})
stock_indices <- stock_indices[-1, ]

plot_stocks(stock_indices)

saveRDS(stock_indices, "data/stocks_logdiff.rds")
