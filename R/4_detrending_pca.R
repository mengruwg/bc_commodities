library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(reshape2)
library(readr)

pca.1980 <- readRDS("data/commodities/pca.1980.rds")
plot_data <- function(x){
  ggplot(x, aes(x = TIME, y = Value)) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = "Set1")
}

# pca.comm prep.
pca.1980.comm <- cbind(pca.1980$Date, pca.1980$comm.)
colnames(pca.1980.comm) <- c("TIME", "Value")
pca.1980.comm <- as.data.frame(pca.1980.comm)

#pca. ind. met. prep
pca.1980.ind.met. <- cbind(pca.1980$Date, pca.1980$ind.met.)
colnames(pca.1980.ind.met.) <- c("TIME", "Value")
pca.1980.ind.met. <- as.data.frame(pca.1980.ind.met.)

#time series plots
plot_data(pca.1980.comm)
plot_data(pca.1980.ind.met.)



#Stationarity function
plot_stationary <- function(x) {
  x <- melt(x, id = "TIME")
  ggplot(x, aes(x = TIME, y = value, colour = variable)) +
    geom_point() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    geom_smooth(method = "lm")
}

#plot with line.
plot_stationary(pca.1980.comm)
plot_stationary(pca.1980.ind.met.)

stationarise_pca <- function(x) {
  #x$Value <- log(x$Value)
  x[-1, 2:ncol(x)] <- apply(x[2:ncol(x)], 2, diff)
  # drop first row
  x <- x[-1, ]
  
  return(x)
}

pca.1980.comm.s <- stationarise_pca(pca.1980.comm)
pca.1980.ind.met.s.<- stationarise_pca(pca.1980.ind.met.)
plot_stationary(pca.1980.ind.met.s.)
plot_stationary(pca.1980.comm.s)

#create a single dataset with stationary values.
pca.1980.stat. <- cbind(pca.1980.comm.s$TIME, pca.1980.comm.s$Value, pca.1980.ind.met.s.$Value)
colnames(pca.1980.stat.) <- c("TIME", "comm.value", "ind.met.value")

saveRDS(pca.1980.stat., "stat.pca.RDS")
