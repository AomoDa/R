

#load necessary library
library(tseries)
library(TSA)
#read data
df <- read.csv("C://Users//mali//Documents//DSPI.csv",header = T,stringsAsFactors = F)


##build time series data
dts <- ts(df$DSPI)
##plot ts data
par(mfrow=c(1,2))
plot.ts(dts, main = "Original Data",
       xlab="Year", ylab="DSPI")
plot.ts(log(dts), main = "Log-Transformed Data",
       xlab="Year", ylab="DSPI")
par(mfrow=c(1,1))


## Spectrum Analysis
par(mfrow=c(1,2))
periodogram(df$DSPI,main='periodogram of DSPI')
spec.pgram(df$DSPI)
par(mfrow=c(1,1))


## Stationary Analysis

#first order defference
plot.ts(diff(log(dts),1),ylab='',main='first order defference of log-DSPI',type='b')
abline(h=0,col='red',lty=2,lwd=2)

###Augmented Dickey-Fuller Test
adf.test(log(dts));
adf.test(diff(log(dts)))


#ACF,PACF
par(mfrow=c(1,2))
acf(diff(log(dts),1))
pacf(diff(log(dts),1))
par(mfrow=c(1,1))


#arima model

eacf(diff(log(dts)))
m1 <- arima(log(dts),order = c(0,1,1))
m2 <- arima(log(dts),order = c(0,1,5))
m3 <- arima(log(dts),order = c(1,1,2))

#aic
m1$aic;m2$aic;m3$aic


## Whitenoise Variance
mean(residuals(m1));mean(residuals(m2));mean(residuals(m3))

#Normality1
shapiro.test(residuals(m1));shapiro.test(residuals(m2));shapiro.test(residuals(m3))

#Ljung-Box Test
Box.test(residuals(m1),type = 'Ljung')
Box.test(residuals(m2),type = 'Ljung')
Box.test(residuals(m3),type = 'Ljung')
