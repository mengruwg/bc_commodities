library(ggplot2)
library(gtable)
library(grid)
library(reshape2)

source("R/4_var_setup.R")
source("var.R")

data <- var(data_aus, 2)

irf_lo <- apply(data$IRF, c(2, 3, 4), quantile, 0.84, na.rm = TRUE)
irf_md <- apply(data$IRF, c(2, 3, 4), median, na.rm = TRUE)
irf_hi <- apply(data$IRF, c(2, 3, 4), quantile, 0.16, na.rm = TRUE)

par(mfrow = c(5, 5))

for (ii in 1:5) {
  for (jj in 1:5) {
    ts.plot(
      cbind(irf_lo[ii, jj, ], irf_hi[ii, jj, ], irf_md[ii, jj, ]),
      ylab = colnames(data_aus)[[ii]],
      main = colnames(data_aus)[[jj]]
    )
    abline(h = 0, col = "red")
  }
}

plot_irf <- function(x, impulse) {
  
  df <- data.frame("id" = 1:dim(x)[3], t(x[, impulse,]))
  df <- melt(df, id = "id")
  
  df[df$variable == 1, ]

  plots <- vector("list", dim(x)[1])
  i = 1
  for(j in 1:dim(x)[1]) {
    plots[[j]] <- ggplot(df[df$variable == paste0("X", i), ], aes(x = id, y = value)) +
      geom_line()
    i = i + 1
  }
  
  ggplot(df, aes(x = id, y = value, group = variable, colour = variable)) +
    #geom_ribbon(aes(ymin = `5P`, ymax = `95P`), fill = "grey60") +
    #geom_ribbon(aes(ymin = `25P`, ymax = `75P`), fill = "grey30") +
    geom_line(col = "black", size = 1) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2, 20, 4)) +
    geom_hline(yintercept = 0) +
    theme_minimal()

}