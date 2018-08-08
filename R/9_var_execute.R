library(vars)

source("R/4_var_setup.R")
#source("R/4_var_single_setup.R")
source("R/8_var_compute.R")
source("R/8_plot_functions.R")

data_countries <- list(data_aus, data_chl, data_deu, data_nor, data_usa, data_zaf)
names(data_countries) <- c("AUS", "CHL", "DEU", "NOR", "USA", "ZAF")

lags <- vector("list", length(data_countries))
i = 1
for(country in data_countries) {
  lags[[i]] <- VARselect(country, lag.max = 1)$selection
  data <- var(country, lags[[i]][1], nburn = 5000, nsave = 10000)
  
  irf_plot_1 <- plot_irf(data$IRF,
                         which(names(country) == "comm"),
                         var_names = names(country))
  irf_plot_2 <- plot_irf(data$IRF,
                         which(names(country) == "industr"),
                         var_names = names(country))
  irf_all <- plot_irf_full(data$IRF, var_names = names(country))
  heatmap_plot <- pip_heatmap(country)

  ggsave(paste0("irf_short_", names(data_countries)[i], ".png"), plot_grid(irf_plot_1, irf_plot_2),
         path = "img", units = "cm", height = 20, width = 16)
  ggsave(paste0("irf_all_", names(data_countries)[i], ".png"), irf_all,
         path = "img", units = "cm", height = 32, width = 48)
  ggsave(paste0("pip_heatmap_", names(data_countries)[i], ".png"), heatmap_plot,
         path = "img", units = "cm", height = 20, width = 20)
  
  i <- i + 1
}
