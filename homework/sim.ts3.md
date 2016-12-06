---
title: "Untitled"
author: "Your Nmae"
date: "2016-12-06"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##EDA time series

- From the time sequence diagram, the original series seems to be a stationary trend.
- Useing Augmented Dickey-Fuller Test,I find $Dickey-Fuller = -6.8977, Lag order = 7, p-value = 0.01<0.05$,which indicated the original series is a stationary series.
- Next I will use $d=0$  to fit my ARIMA model.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
# load data
library(TSA)
library(forecast)
x <- scan('C://Users//mali//Documents//Question4.txt')
# plot ts
par(mfrow=c(1,2))
plot.ts(x,main='original data')
plot.ts(diff(x),main='first difference data')
par(mfrow=c(1,1))
#adf test
adf.test(x)
```

##ACF and PACF

The acf and PACF tell me that $p=1$ and $q=0$ maybe a good choice for my ARIMA model.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
#acf and pacf
par(mfrow=c(1,2))
acf(x)
pacf(x)
par(mfrow=c(1,1))

```

##Fit  ARIMA Model

I fit my ARIMA(p=1,d=0,q=0) and plot the fitted value and real vales.I also find  that the ARIMA(p=1,d=0,q=0) model  fits the original data very good.

-----

Item|ar1|10.0644
----|-----|------
Coefficients|0.6005|10.0644
s.e.|0.0357 |0.0711

-----

```{r, echo=FALSE, message=FALSE, warning=FALSE}
auto.arima(x)
m1 <- arima(x,order = c(1,0,0))
summary(m1)
plot.ts(fitted.Arima(m1),col='red',main='ARIMA(1,0,0)')
points(x,type='l')
legend('topleft',col=c('red','black'),legend=c('fitted values','real vales'),lty=1)
```


##Predict

I drew a forecast of the most recent time.The forecast of the next 10 steps is a relatively stable trend.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
plot(forecast.Arima(m1,
                    h = 10,
                    level = 0.95),
     xlim=c(400,510))
```

##Appendix R Code

```{r, eval=FALSE, include=T}
# load data
library(TSA)
library(forecast)
x <- scan('Question4.txt')
# plot ts
par(mfrow=c(1,2))
plot.ts(x,main='original data')
plot.ts(diff(x),main='first difference data')
par(mfrow=c(1,1))
#adf test
adf.test(x)
#acf and pacf
par(mfrow=c(2,2))
acf(x)
pacf(x)
acf(diff(x))
pacf(diff(x))
par(mfrow=c(1,1))
#fit model
auto.arima(x)
m1 <- arima(x,order = c(1,0,0))
plot.ts(fitted.Arima(m1),col='red',main='ARIMA(1,0,0)')
points(x,type='l')
legend('topleft',col=c('red','black'),legend=c('fitted values','read vales'),lty=1)
#residuals
tsdiag(m1)
```

