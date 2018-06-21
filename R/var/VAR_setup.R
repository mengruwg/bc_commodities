require(vars)
require(bvarsv)

source("R/2_VAR_country_setup.R")

#data <- data_zaf[c(1, 5, 2)]
# trade excl
data <- data_usa[c("spgsci", "gdp", "mp_rate")]
data <- data_deu[c("spgsci", "gdp", "mp_rate", "equity")]
data <- data_zaf[c("spgsci", "gdp", "mp_rate")]
data <- data_chl[c("spgsci", "gdp", "mp_rate")]
data <- data_aus[c("spgsci", "gdp", "m3", "mp_rate")]
data <- data_nor[c("spgsci", "gdp", "mp_rate")]

data <- data_usa[c("spgsci", "gdp", "mp_rate")]

data <- apply(data, 2, scale)

bvars <- vector("list", 6)

bvars[[1]]<- fit <- bvar.sv.tvp(Y = as.matrix(data), p = lag, nburn = 10000, nrep = 5000) # usa
bvars[[2]]<- fit <- bvar.sv.tvp(Y = as.matrix(data), p = lag, nburn = 10000, nrep = 5000) # deu
bvars[[3]]<- fit <- bvar.sv.tvp(Y = as.matrix(data), p = lag, nburn = 10000, nrep = 5000) # zaf
bvars[[4]]<- fit <- bvar.sv.tvp(Y = as.matrix(data), p = lag, nburn = 10000, nrep = 5000) # chl
bvars[[5]]<- fit <- bvar.sv.tvp(Y = as.matrix(data), p = lag, nburn = 10000, nrep = 5000) # aus
bvars[[6]]<- fit <- bvar.sv.tvp(Y = as.matrix(data), p = lag, nburn = 10000, nrep = 5000) # nor


# vars --------------------------------------------------------------------

# maybe change IC
fit.ols <- VAR(y = data, lag.max = 8, ic = "AIC")

lag <- fit.ols$p

# restricted irf
#plot(irf(fit.ols, ci = 0.9))
# irf under long-run restrictions
#plot(irf(BQ(fit.ols), ci = 0.9))

fit <- bvars[[1]]

matplot2 <- function(...) matplot(..., type = "l", lty = 1, lwd = 2,
                                  bty = "n", ylab = "")
stat.helper <- function(z) c(mean(z), quantile(z, c(0.16, 0.84)))[c(2, 1, 3)]
xax <- seq(1, 148, 1) # x axis

gp <- seq(1965, 2000, 5) # marks for vertical lines
# colors, taken from http://www.cookbook-r.com
cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7")
cols1 <- cols[c(2, 4, 2)]
# Residual variance from simpler benchmark model
sd.residuals.ols <- apply(residuals(fit.ols), 2, sd)
# Make plot
par(mfrow = c(3, 1))
# SD of inflation residual
# Get posterior draws
sd_inf <- parameter.draws(fit, type = "vcv", row = 1, col = 1)
x1 <- t(apply(sqrt(sd_inf), 2, stat.helper))
# Plot
matplot2(x = xax, y = x1, ylim = c(0, 0.8), col = cols1,
         main = "Inflation", xlab = "Time")
abline(h = c(0.2, 0.4, 0.6), v = gp, lty = 4, lwd = 0.3)
abline(h = sd.residuals.ols[1], col = cols[1], lwd = 1.4)
# SD of unemployment residual
# Get posterior draws
sd_une <- parameter.draws(fit, type = "vcv", row = 2, col = 2)
x2 <- t(apply(sqrt(sd_une), 2, stat.helper))
# Plot
matplot2(x = xax, y = x2, ylim = c(0, 1), col = cols1,
         main = "Unemployment", xlab = "Time")
abline(h = c(0.2, 0.4), v = gp, lty = 4, lwd = 0.3)
abline(h = sd.residuals.ols[2], col = cols[1], lwd = 1.4)
# SD of interest rate residual
# Get posterior draws
sd_tbi <- parameter.draws(fit, type = "vcv", row = 3, col = 3)
x3 <- t(apply(sqrt(sd_tbi), 2, stat.helper))
# Plot
matplot2(x = xax, y = x3, ylim = c(0, 5), col = cols1,
         main = "Interest rate", xlab = "Time")
abline(h = c(0.2, 0.4), v = gp, lty = 4, lwd = 0.3)
abline(h = sd.residuals.ols[3], col = cols[1], lwd = 1.4)

bvarsv_irf <- function(bvar, impulse = 1, response = 1, ols) {
  irf <- impulse.responses(bvar, impulse.variable = imp, response.variable = resp)
  if(!is.missing(ols)) {
    irf_ols <- irf(ols, n.ahead = 20)[[1]][[imp]][-1, resp]
    lines(x = 1:20, y = ira.ols, lwd = 3, lty = 5)
  }
  pdat <- t(apply(irf$irf, 2, function(z) quantile(z, c(0.05, 0.25, 0.5, 0.75, 0.95))))
  xax <- 1:20
  matplot(x = xax, y = pdat, type = "n", ylab = "", xlab = "Horizon", 
          bty = "n", xlim = c(1, 20))
  polygon(c(xax, rev(xax)), c(pdat[, 5], rev(pdat[, 4])), 
          col = "grey60", border = NA)
  polygon(c(xax, rev(xax)), c(pdat[, 4], rev(pdat[, 3])), 
          col = "grey30", border = NA)
  polygon(c(xax, rev(xax)), c(pdat[, 3], rev(pdat[, 2])), 
          col = "grey30", border = NA)
  polygon(c(xax, rev(xax)), c(pdat[, 2], rev(pdat[, 1])), 
          col = "grey60", border = NA)
  lines(x = xax, y = pdat[, 3], type = "l", col = 1, lwd = 2.5)
  abline(h = 0, lty = 2)
}

par(mfrow = c(1, 1))
ira <- impulse.responses(fit, impulse.variable = imp, response.variable = resp, scenario = 2)
ira.ols <- irf(fit.ols, n.ahead = 20)[[1]][[imp]][-1, resp]
lines(x = 1:20, y = ira.ols, lwd = 3, lty = 5)
