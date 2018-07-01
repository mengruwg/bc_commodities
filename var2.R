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

