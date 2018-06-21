require(tseries)

cointegration<-function(x,y)
{
  vals<-data.frame(x,y)
  beta<-coef(lm(vals[,2]~vals[,1]+0,data=vals))[1]
  (adf.test(vals[,2]-beta*vals[,1], alternative="stationary", k=0))$p.value
}

cointegration(data_nor[6], data_nor[5])
