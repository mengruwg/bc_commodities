library(reshape2)

source("R/4_var_setup.R")
source("R/8_var_compute.R")
source("R/9_plot_functions.R")

data <- var(data_aus, 2)

irf_plot_1 <- plot_irf(data$IRF, 7, var_names = names(data_aus))
irf_plot_2 <- plot_irf(data$IRF, 8, var_names = names(data_aus))
heatmap_plot <- pip_heatmap(data_aus)
