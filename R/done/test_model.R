gdp = gdp[which(gdp$LOCATION == "AUS"), ]
data = data[which(data$LOCATION == "AUS"), ]

m1 = data[which(data$Subject == "Monetary aggregates and their components > Narrow money and components > M1 and components > M1"), ]
m1 = m1[which(m1$Frequency == "Quarterly"), ]
m1 = m1[which(m1$MEASURE == "IXOBSA"), ]

employ = data[which(data$Subject == "Leading Indicators OECD > Component series > BTS - Employment > Original series"), ]

cpi = data[which(data$Subject == "Consumer Price Index > All items > Total > Total"), ]
cpi = cpi[which(cpi$MEASURE == "IXOB"), ]


# int_10y = data[which(data$Subject == "Interest Rates > Long-term government bond yields > 10-year > Main (including benchmark)"), ]
# int_10y = int_10y[which(int_10y$Frequency == "Quarterly"), ]
# int_3m = data[which(data$Subject == "Interest Rates > 3-month or 90-day rates and yields > Interbank rates > Total"), ]
# int_3m = int_3m[which(int_3m$Frequency == "Quarterly"), ]

gdp = gdp[c("TIME", "Value")]
m1 = m1[c("TIME", "Value")]
employ = employ[c("TIME", "Value")]
cpi = cpi[c("TIME", "Value")]
# int_10y = int_10y[c("TIME", "Value")]
# int_3m = int_3m[c("TIME", "Value")]

mdl = merge(gdp, cpi, by = "TIME")
names(mdl) = c("TIME", "GDP", "CPI")
mdl = merge(mdl, employ, by = "TIME")
names(mdl) = c("TIME", "GDP", "CPI", "EMPLOY")
# mdl = merge(mdl, int_10y, by = "TIME")
# names(mdl) = c("TIME", "LOCATION", "GDP", "M1", "EMPLOY", "INT10")
# mdl = merge(mdl, int_3m, by = "TIME")
# names(mdl) = c("TIME", "LOCATION", "GDP", "M1", "EMPLOY", "INT10", "INT3")
# 
# mdl$SPREAD = mdl$INT10 - mdl$INT3

mdl$TIME = as.yearqtr(mdl$TIME, "%Y-Q%q")

plot(mdl)

comm = index_re[c("DATE", "GCSI")]
names(comm) = c("TIME", "GCSI")

comm = comm[seq(1, nrow(comm), 3), ]

comm$TIME = as.yearqtr(comm$TIME)

ols = merge(mdl, comm, by = "TIME")

hp_GDP = hpfilter(ols$GDP, freq = 1600)
hp_CPI = hpfilter(ols$CPI, freq = 1600)
hp_GCSI = hpfilter(ols$GCSI, freq = 1600)

x = lm(hp_GDP$cycle ~ hp_CPI$cycle + EMPLOY + hp_GCSI$cycle, data = ols)
summary(x)

df = data.frame(ols$TIME, hp_GDP$cycle, ols$EMPLOY, hp_GCSI$cycle)
names(df) = c("TIME", "GDP", "EMPLOY", "GCSI")

ggplot(df, aes(x = TIME)) +
  geom_line(aes(y = GDP), colour = "red") +
  geom_line(aes(y = GCSI))

ols_diff = data.frame(diff(ols$GDP), diff(ols$EMPLOY), diff(ols$GCSI))

analyse = ts(ols_diff, freq = 4, start = c(1970, 1))

par(mfcol = c(3, 1))
plot(analyse[, 1], main = "GDP, cyclical")
plot(analyse[, 2], main = "Employment")
plot(analyse[, 3], main = "GCSI, cyclical")

acf(analyse, lag.max = 25)
acf(analyse, type = "partial", lag.max = 25)

analyse_sel = VARselect(analyse, lag.max = 12, type = "const")
analyse_sel$selection

analyse_var = VAR(analyse, p = 1, type = "const")
summary(analyse_var)

plot(irf(analyse_var))
