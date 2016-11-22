---
title: "Time Series Analysis of Disposable Personal Income of the United States"
author: "Your Nmae"
date: "2016-11-22"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#1.Introduction

From the standpoint of an individual, disposable income is the income that you have left over after you have paid all taxes and other mandatory expenses such as home loan payments, consumer loan payments, and child support obligations. You can think of mandatory expenses as expenses required to be paid by law or a legally binding agreement.

#2.Data




#3.Analysis

Load data and packages that we would use to analysis.

```{r, message=FALSE, warning=FALSE}
#load necessary library
library(tseries)
library(TSA)
#read data
df <- read.csv("C://Users//mali//Documents//DSPI.csv",header = T,stringsAsFactors = F)
```


##3.1 Tend Analysis

Consider first the monthly disposable personal income (DSPI) of the United States from January 1959 to October 2016 for 693 observations. I would observe the trend of original data and log transformed data.

```{r}
##build time series data
dts <- ts(df$DSPI)
##plot ts data
par(mfrow=c(1,2))
plot.ts(dts, main = "Original Data",
       xlab="Year", ylab="DSPI")
plot.ts(log(dts), main = "Log-Transformed Data",
       xlab="Year", ylab="DSPI")
par(mfrow=c(1,1))
```


##3.2 Spectrum Analysis

According to the graph based on spectral analysis,We find that there seems not periodic.

```{r}
## Spectrum Analysis
par(mfrow=c(1,2))
periodogram(df$DSPI,main='periodogram of DSPI')
spec.pgram(df$DSPI)
par(mfrow=c(1,1))
```

##3.3 Stationary Analysis

Augmented Dickey-Fuller Test:

- Null hypothesis: not-stationary
- Alternative hypothesis:stationary

-----

Item|pvalue of ADF test|Conclusion
-----|------------------|----------
Log DSPI|0.99|Fail to reject Null hypothesis
First Difference of Log DSPI|0.01| reject Null hypothesis

-----

```{r}
#first order defference
plot.ts(diff(log(dts),1),ylab='',main='first order defference of log-DSPI',type='b')
abline(h=0,col='red',lty=2,lwd=2)
###Augmented Dickey-Fuller Test
adf.test(log(dts));
adf.test(diff(log(dts)))
```


##3.4 ACF and PACF


```{r}
par(mfrow=c(1,2))
acf(diff(log(dts),1))
pacf(diff(log(dts),1))
par(mfrow=c(1,1))
```


#4.Fit Model

##4.1 My Model

According to acf and pacf,I perfer model arima(0,1,1),arima(0,1,5) and arima(1,1,2)

```{r}
eacf(diff(log(dts)))
m1 <- arima(log(dts),order = c(0,1,1))
m2 <- arima(log(dts),order = c(0,1,5))
m3 <- arima(log(dts),order = c(1,1,2))
```


##4.2 analysis of residuals

-----

Item|arima(0,1,1)|arima(0,1,5)|arima(1,1,2)
-----|-------------|------------|------------
AIC|-4588.816|-4695.426|-4903.936
Whitenoise Variance|0.0045|0.0031|0.0001
Diagnostics Normality|P=0|P=0|P=0
Ljung-Box Test3|P=0|P=0.0003|P=0.4438

-----


```{r}
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
```

#5.Discussion

- residuals of three model is normality,because of the pvalue of Shapiro-Wilk Normality Test is less than 0.05.
- arima(0,1,1) and arima(0,1,5)  do not pass Box-Ljung test,but arima(1,1,2) do.
- AIC of arima(1,1,2) is the smallest .

#6.Conclusion


arima(1,1,2)  is the best model to predict DSPI.


#7.Reference

- US. Bureau of Economic Analysis, Disposable Personal Income [DSPI], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DSPI, November 20, 2016.


#8.Appendix R-Code

```{r, eval=FALSE, message=FALSE, warning=FALSE, include=T}


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
```

