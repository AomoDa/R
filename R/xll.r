

# loading data
x <- read.table('EuroIndices.txt',header = T)
summary(x)

# plot ts
plot(stock_ts)

# for dax

dax_ts <- stock_ts[,4]

par(mfrow=c(1,2))
acf(dax_ts,lag.max = 36,main='ACF of DAX')
pacf(dax_ts,lag.max = 36,main='PACF of DAX')
par(mfrow=c(1,1))

library(forecast)

# fit best AR(p)
ar1 <- auto.arima(dax_ts,d = 0,max.p = 5,max.P = 0,ic='bic')
# fit best ARMA(p,q)
arma1 <- auto.arima(dax_ts,d = 0,max.p = 5,max.P = 5,ic='bic')
# fit best ARiMA(p,d=2,q)
arima1 <- auto.arima(dax_ts,d = 2,max.p = 5,max.P = 5,ic='bic')

#AIC and BIC
AIC(ar1);AIC(arma1);AIC(arima1)
BIC(ar1);BIC(arma1);BIC(arima1)

# residuals Shapiro-Wilk normality test
shapiro.test(ar1$residuals)
shapiro.test(arma1$residuals)
shapiro.test(arima1$residuals)

# Box-Ljung test
Box.test(ar1$residuals,type = 'Ljung')
Box.test(arma1$residuals,type = 'Ljung')
Box.test(arima1$residuals,type = 'Ljung')

# 
pred <- forecast.Arima(arima1,h = 10,level = 0.95)
plot(pred)
