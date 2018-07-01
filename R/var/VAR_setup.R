require(vars)
require(bvarsv)

source("R/2_VAR_country_setup.R")

data <- data_aus[c("comm", "industr", "gdp", "export", "infl", "m3", "mp_rate", "i10y")]
data <- data_zaf[c("comm", "industr", "gdp", "export", "infl", "m3", "mp_rate", "i10y", "equity")]



# prep --------------------------------------------------------------------

names <- c("USA", "DEU", "ZAF", "CHL", "AUS", "NOR")

data <- vector("list", 6)

data[[1]] <- data_usa[c("spgsci", "gdp", "mp_rate")]
data[[2]] <- data_deu[c("spgsci", "gdp", "mp_rate", "equity")]
data[[3]] <- data_zaf[c("spgsci", "gdp", "mp_rate")]
data[[4]] <- data_chl[c("spgsci", "gdp", "mp_rate")]
data[[5]] <- data_aus[c("spgsci", "gdp", "m3", "mp_rate")]
data[[6]] <- data_nor[c("spgsci", "gdp", "mp_rate")]

#data <- apply(data, 2, scale)

bvars <- vector("list", 6)

bvars[[1]] <- readRDS("data/bvars/bvar_usa.rds")
bvars[[2]] <- readRDS("data/bvars/bvar_deu.rds")
bvars[[3]] <- readRDS("data/bvars/bvar_zaf.rds")
bvars[[4]] <- readRDS("data/bvars/bvar_chl.rds")
bvars[[5]] <- readRDS("data/bvars/bvar_aus.rds")
bvars[[6]] <- readRDS("data/bvars/bvar_nor.rds")

# vars --------------------------------------------------------------------

i <- 3
fit_ols <- VAR(y = data[[i]], lag = 3)
fit <- bvars[[i]]


# sd over time ------------------------------------------------------------

matplot2 <- function(...) {
  matplot(..., type = "l", lty = 1, lwd = 2, bty = "n", ylab = "residual sd")
}
stat_helper <- function(z) {
  c(mean(z), quantile(z, c(0.16, 0.84)))[c(2, 1, 3)]
}

# x-axis
xax <- seq(1984.5, 2018, 0.25)
# grid
grid <- seq(1985, 2015, 5)


# colors
col <- c("#E69F00", "#009E73", "#E69F00")
col2 <- "black"

# Residual variance from simpler benchmark model
sd_residuals_ols <- apply(residuals(fit_ols), 2, sd)

# Make plot
par(mfrow = c(3, 1))

# SD 1
# Get posterior draws
sd_1 <- parameter.draws(fit, type = "vcv", row = 1, col = 1)
x1 <- t(apply(sqrt(sd_1), 2, stat_helper))
# Plot
matplot2(x = xax, y = x1, ylim = c(0, 5), col = col,
         main = "SPGSCI", xlab = "")
abline(h = sd_residuals_ols[1], col = col2, lwd = 1.4, lty = 1)
abline(h = c(0, 2, 4), v = gp, lty = 3, lwd = 0.3, col = "gray")

# SD 2
# Get posterior draws
sd_2 <- parameter.draws(fit, type = "vcv", row = 2, col = 2)
x2 <- t(apply(sqrt(sd_2), 2, stat_helper))
# Plot
matplot2(x = xax, y = x2, ylim = c(0, 0.1), col = col,
         main = "GDP", xlab = "")
abline(h = sd_residuals_ols[2], col = col2, lwd = 1.4, lty = 1)
abline(h = c(0, 0.04, 0.08), v = gp, lty = 3, lwd = 0.3, col = "gray")

# SD 3
# Get posterior draws
sd_3 <- parameter.draws(fit, type = "vcv", row = 3, col = 3)
x3 <- t(apply(sqrt(sd_3), 2, stat_helper))
# Plot
matplot2(x = xax, y = x3, ylim = c(0, 4), col = col,
         main = "Monetary Policy Rate", xlab = "time")
abline(h = sd_residuals_ols[3], col = col2, lwd = 1.4, lty = 1)
abline(h = c(0, 2, 4), v = gp, lty = 3, lwd = 0.3, col = "gray")



# irf ---------------------------------------------------------------------



bvarsv_irf <- function(bvar, impulse = 1, response = 1, title = "", ols) {
  irf <- impulse.responses(bvar, scenario = 2,
                           impulse.variable = impulse, 
                           response.variable = response)
  
  pdat <- t(apply(irf$irf, 2, function(z) quantile(z, c(0.05, 0.25, 0.5, 0.75, 0.95))))
  pdat <- data.frame("id" = 1:nrow(pdat), pdat)
  names(pdat) <- c("id", "5P" ,"25P", "50P", "75P", "95P")
  
  ggplot(pdat, aes(x = id)) +
    geom_ribbon(aes(ymin = `5P`, ymax = `95P`), fill = "grey60") +
    geom_ribbon(aes(ymin = `25P`, ymax = `75P`), fill = "grey30") +
    geom_line(aes(y = `50P`), col = "black", size = 1.5) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(2, 20, 4)) +
    theme_light() +
    xlab("horizon") +
    ylab("") +
    ggtitle(title)
  # xax <- 1:20
  # matplot(x = xax, y = pdat, type = "n", ylab = "", xlab = "Horizon", 
  #         bty = "n", xlim = c(1, 20), main = title)
  # polygon(c(xax, rev(xax)), c(pdat[, 5], rev(pdat[, 4])),
  #         col = "grey60", border = NA)
  # polygon(c(xax, rev(xax)), c(pdat[, 4], rev(pdat[, 3])),
  #         col = "grey30", border = NA)
  # polygon(c(xax, rev(xax)), c(pdat[, 3], rev(pdat[, 2])),
  #         col = "grey30", border = NA)
  # polygon(c(xax, rev(xax)), c(pdat[, 2], rev(pdat[, 1])),
  #         col = "grey60", border = NA)
  # lines(x = xax, y = pdat[, 3], type = "l", col = 1, lwd = 2.5)
  # abline(h = 0, lty = 2)
  # 
  # if(!missing(ols)) {
  #   irf_ols <- irf(ols, n.ahead = 20)[[1]][[imp]][-1, resp]
  #   lines(x = 1:20, y = ira.ols, lwd = 2, lty = 2, col = "gray")
  # }
}
data <- readRDS("data/bvars/ols_all.rds")
bvars[[1]] <- readRDS("data/bvars/bvar_usa.rds")
bvars[[2]] <- readRDS("data/bvars/bvar_deu.rds")
bvars[[3]] <- readRDS("data/bvars/bvar_zaf.rds")
bvars[[4]] <- readRDS("data/bvars/bvar_chl.rds")
bvars[[5]] <- readRDS("data/bvars/bvar_aus.rds")
bvars[[6]] <- readRDS("data/bvars/bvar_nor.rds")

i = 3

bvarsv_irf(bvars[[i]], 1, 1, names[i])
bvarsv_irf(bvars[[i]], 1, 2, names[i])
bvarsv_irf(bvars[[i]], 1, 3, names[i])
