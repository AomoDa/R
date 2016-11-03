#Stat 153 Final Project
#Huayang Xie
#Appendix for R codes
library(fGarch)
library(forecast)
library(MASS)
library(TSA)
library(tseries)
library(car)

file=read.csv("report.csv",head=F)
v.weekly=ts(as.numeric(as.character(file[c(106:157,159:418,420:627),2])),
            start=2006,frequency=52)
plot(v.weekly,ylab='Search Interest',xlab='Year',type='o')
v.biweekly=c()
for(i in seq(1,520,2)){
  v.biweekly=c(v.biweekly,(v.weekly[i]+v.weekly[i+1]))
}
v=ts(v.biweekly,start=2006,frequency=26)
tsoutliers(v)
v[68]=113.9680
v[69]=113.3837
v[70]=116.4976
v[71]=119.5753
v[72]=117.9243
v[179]=135.7529
v[260]=139.7614
v.org=v
par(mfrow=c(1,2))
plot(v,ylab='Search Interest',xlab='Year',
     main='Bi-weekly Search Interest for R',type='o')
#The time series is observed an upward trend but with no obvious seasonality.
plot(y=v,x=zlag(v),ylab='Search Interest',
     xlab='Search Interest of Previous Bi-weekly Point')
#There is strong positive relationship between this bi-week’s interest 
#and previous bi-week’s interest.
acf(as.numeric(v),250)
pacf(as.numeric(v),250)
eacf(as.numeric(v))
#From the plot of ACF and PACF, I can't identify any pure AR or MA pattern.
#So ARMA model is assumed.
v.stl=stl(v,s.window='periodic')
plot(v.stl)
BoxCox.ar(v)
#With 95% confidence interval for λ contains the value of λ = 0 
#quite near its center. It strongly suggests a logarithmic transformation.
v=log(v)
plot(v,ylab='Search Interest',xlab='Year',type='o')
#De-trend through first differencing
v.diff=diff(v)
plot(v.diff,ylab='Differenced and Log Search Interest',xlab='Year',
     main='1st Diff and Log Transformation of Search Interest',type='o')
lin.m=lm(v.diff~time(v.diff))
abline(lin.m,col='red')
summary(lin.m)
#After differencing, the time series looks quite stationary now.
par(mfrow=c(1,2))
acf(as.numeric(v.diff),250,
    main='ACF for 1st Diff and Log of Search Interest')
pacf(as.numeric(v.diff),250,
     main='PACF for 1st Diff and Log of Search Interest')
eacf(as.numeric(v.diff))
#The plot of ACF shows some kind of seasonality with a period of 26 
#which equals to a year length.

#Let's first start with a deterministic model - seasonal means model.
weeks=season(v.diff)
m.d=lm(v.diff~weeks-1)
summary(m.d)
plot(ts(fitted(m.d),start=c(2006,2),frequency=26),
     ylab='1st Diff and Log of Search Interest',xlab='Year',type='l',
     ylim=range(c(fitted(m.d),v.diff)),
     col=rgb(red=1,green=0.1,blue=0.1,alpha=0.9),
     main='Predicted Data in Red and Observed Data in Black')
points(v.diff,cex=0.2,type='o',col=rgb(red=0.1,green=0.1,blue=0.1,alpha=0.7))
#ylim ensures that the y axis range fits the raw data and the fitted values.
#It looks like already fitting well.
par(mfrow=c(1,2))
plot(y=rstandard(m.d),x=as.vector(time(v.diff)),xlab='Year',type='o',
  ylab='Standardized Residuals',main='Standardized Residuals Plot',cex=0.5)
abline(h=0,col='red')
AIC(m.d) #-1186.887
LB.test(m.d,lag=250) #p-value = 0.005537
qqnorm(rstandard(m.d))
qqline(rstandard(m.d))
shapiro.test(rstandard(m.d)) #p-value = 5.385e-05
runs(rstandard(m.d)) #p-value = 0.0572
acf(rstandard(m.d),lag.max=250)
pacf(rstandard(m.d),lag.max=250)
periodogram(rstandard(m.d),main='Periodogram of Residuals')
spec(rstandard(m.d),spans=16)

#Add a harmonic model on residuals
res.period.d=periodogram(residuals(m.d),main="Periodogram of Residuals") 
freq=res.period.d$freq[order(res.period.d$spec,decreasing=T)][1:5]
#cosine and sine pairs 
c1<-cos(2*pi*freq[1]*time(rstandard(m.d))) 
s1<-sin(2*pi*freq[1]*time(rstandard(m.d))) 
c2<-cos(2*pi*freq[2]*time(rstandard(m.d))) 
s2<-sin(2*pi*freq[2]*time(rstandard(m.d))) 
c3<-cos(2*pi*freq[3]*time(rstandard(m.d))) 
s3<-sin(2*pi*freq[3]*time(rstandard(m.d))) 
c4<-cos(2*pi*freq[4]*time(rstandard(m.d))) 
s4<-sin(2*pi*freq[4]*time(rstandard(m.d))) 
c5<-cos(2*pi*freq[5]*time(rstandard(m.d))) 
s5<-sin(2*pi*freq[5]*time(rstandard(m.d))) 
freq.model= lm(residuals(m.d)~c1+s1+c2+s2+c3+s3+c4+s4+c5+s5)
step(freq.model)
freq.model= lm(residuals(m.d)~s1+c2+c3+s3+c4+s5)
summary(freq.model)
par(mfrow=c(1,2))
plot(y=rstandard(freq.model),x=as.vector(time(v.diff)),
     xlab='Year',ylab='Standardized Residuals',
     main='Standardized Residuals Plot',type='o',cex=0.5)
abline(h=0,col='red')
AIC(freq.model) # aic=-1274.417
LB.test(freq.model,lag=250) #p-value = 0.2677
hist(rstandard(freq.model),breaks=50,
  xlab='Standardized Residuals',main='Histogram of Standardized Residuals')
qqPlot(rstandard(freq.model),ylab='Sample Quantiles',
  xlab='Theoretical Quantiles',main='Normal Q-Q Plot with 95% Confidence Interval')
shapiro.test(rstandard(freq.model)) #p-value = 8.897e-05
runs(rstandard(freq.model)) #p-value = 0.0234
acf(residuals(freq.model),lag.max=250,main='Residuals from Harmonic Model')
pacf(residuals(freq.model),lag.max=250,main='Residuals from Harmonic Model')
periodogram(residuals(freq.model))
spec(residuals(freq.model),spans=16)
ArchTest(residuals(freq.model)) #p-value = 0.323 no ARCH effects

#Let's continue with another model - seasonal arima.
#By first taking a seasonal difference
par(mfrow=c(1,2))
plot(diff(v.diff,lag=26),xlab='Years',ylab='1st and Seasonal Diff of Search Interest',
     main='Plot of 1st and Seasonal Diff of Search Interest',type='o')
acf(as.numeric(diff(v.diff,lag=26)),ci.type='ma',250,
    main='ACF of 1st and Seasonal Diff of Search Interest')
pacf(as.numeric(diff(v.diff,lag=26)),250)
eacf(as.numeric(diff(v.diff,lag=26)))
auto.arima(v.diff)

#Fit the ARIMA model
final.aic=Inf
final.order=c(0,0,0)
final.order2=c(0,0,0)
for(p in 0:10){ 
  for(q in 0:10){
    for(P in 0:10){
      for(Q in 0:10){
        arimaFit=tryCatch(arima(v.diff, order=c(p, 0, q),
                          seasonal=list(order=c(P,1,Q),period=26)),
                          error=function(err) FALSE,
                          warning=function(err) FALSE)
        if(!is.logical(arimaFit)){
          current.aic=AIC(arimaFit)
          if(current.aic<final.aic){
            final.aic=current.aic
            final.order=c(p,0,q)
            final.order2=c(P,1,Q)
            final.arima=arima(v.diff,order=final.order,
                              seasonal=list(order=final.order2,period=26))
          }
        } 
        else{
          next
        }
      }
    }
  }
}
print(final.aic)
print(final.order)
print(final.order2)
arima(v.diff,order=c(1,0,0),seasonal=list(order=c(0,1,1),period=26))
#Coefficients:
#  ar1     sma1
#-0.2411  -1.0000
#s.e.   0.0640   0.1707
#sigma^2 estimated as 0.0005093:  log likelihood = 522.83,  aic = -1041.66
m.arima=arima(v.diff,order=c(1,0,0),seasonal=list(order=c(0,1,1),period=26))
#intercept should be dropped because its s.e is greater than its estimate.
detectAO(m.arima)
#"No AO detected"
detectIO(m.arima)
#ind     40.000000 67.000000
#lambda1 -4.565638  4.005071
m1.arima=arimax(v.diff,order=c(1,0,0),
        seasonal=list(order=c(0,1,1),period=26),io=c(40,67)) #aic=-1055.45
plot(ts(fitted(m1.arima),start=c(2006,2),frequency=26),
     ylab='1st Differenced and log of Search Interest',xlab='Year',type='l',
     ylim=range(c(fitted(m1.arima),v.diff)),col=rgb(red=1,green=0.1,blue=0.1,alpha=0.9),
     main='Predicted Time Series in Red and Observed Time Series in Black')
points(v.diff,cex=0.2,type='o',col=rgb(red=0.1,green=0.1,blue=0.1,alpha=0.7))

#residual analysis
par(mfrow=c(1,2))
#check for randomness
plot(rstandard(m1.arima),ylab='Standardized Residuals',xlab='Year',
     main='Standardized Residuals Plot',type='o')
abline(h=0,col='red')
tsdiag(m1.arima)
LB.test(m1.arima,lag=250)
#check for normality
hist(rstandard(m1.arima),xlab='Standardized Residuals',breaks=10)
#looks very normal
qqnorm(rstandard(m1.arima))
qqline(rstandard(m1.arima))
#looks pretty fit
shapiro.test(rstandard(m1.arima))
jarque.bera.test(rstandard(m1.arima))
#check for independency
runs(rstandard(m1.arima))
#check for correlations
acf(rstandard(m1.arima),lag.max=250, main='ACF of Standardized Residuals')
pacf(rstandard(m1.arima),lag.max=250,main='PACF of Standardized Residuals')
#check for frequency
periodogram(rstandard(m1.arima),main='Periodogram of Residuals')
spec(rstandard(m1.arima),spans=16)
#overfitting check
arimax(v.diff,order=c(1,0,0),seasonal=list(order=c(0,1,2),period=26))
#sma2 should be dropped because its s.e is greater than its estimate. 
#The model is worse as aic increase.
arimax(v.diff,order=c(1,0,0),seasonal=list(order=c(1,1,1),period=26))
#sar1 should be dropped because its s.e is greater than its estimate. 
#The model is worse as aic increase.
arimax(v.diff,order=c(1,0,1),seasonal=list(order=c(0,1,1),period=26))
#ma1 should be dropped because its s.e is greater than its estimate. 
#The model is worse as aic is larger.
arimax(v.diff,order=c(2,0,0),seasonal=list(order=c(0,1,1),period=26))
#ar2 should be dropped because its s.e is greater than its estimate. 
#The model is worse as aic is larger.

#Hybrid model - seasonal ARIMA + GARCH model.
#Testing for garch
McLeod.Li.test(m1.arima,250)
#Some p-values are significant. ARCH/GARCH is needed for the residuals.
par(mfrow=c(1,2))
plot(rstandard(m1.arima),ylab='Standardized Residuals',xlab='Year',type='o')
#Residual plot displays some cluster of volatility.
plot(rstandard(m1.arima)^2,ylab='Standardized Residuals',xlab='Year',type='o')
#Squared residual plot displays some cluster of volatility.
acf(as.numeric(rstandard(m1.arima))^2,250)
pacf(as.numeric(rstandard(m1.arima))^2,250)
#ACF & PACF of squared residuals will confirm that the residuals 
#(noise term) are not independent and can be predicted.
#Followings are found on the plots of squared residuals:
#1.Squared residuals plot shows cluster of volatility at some points in time
#2.ACF seems to die down
#3.PACF cuts off even though some remaining lags are significant
#The residuals therefore show some patterns that might be modeled.
#Determine the Order of GARCH ModelL:
table=NULL
Q=6
row=NULL
for(q in 1:Q){ 
  g0q=garch(residuals(m1.arima),order=c(0,q),grad="analytical") 
  row=c(row,AIC(g0q))
}
table=cbind(table,row)
P=6
for(q in 1:Q){
  row=NULL
  for(p in 1:P){ 
    gpq=garch(residuals(m1.arima),order=c(p,q),grad="analytical") 
    row=c(row,AIC(gpq))
  } 
  table=cbind(table,row) 
}
View(table)
#GARCH(1,1) has the smallest AIC value.
m.g=garch(residuals(m1.arima),order=c(1,1),grad="analytical")

#Residual Analysis for GARCH(1,1)
par(mfrow=c(1,3))
plot(residuals(m.g),ylab='Standardized Residuals',
     main='Standardized Residuals from GARCH Model',type='o')
abline(h=0,col='red')
LB.test(m.g,lag=250)#p-value = 0.4286
hist(residuals(m.g),xlab='Standardized Residuals',
     main='Histogram of Standardized Residuals')
#looks very normal
qqnorm(residuals(m.g))
qqline(residuals(m.g))
#looks pretty fit
shapiro.test(residuals(m.g))
acf(residuals(m.g),na.action=na.omit,lag.max=250,main="ACF of Residuals")
pacf(residuals(m.g),na.action=na.omit,lag.max=250,main='PACF of Residuals')
#check for frequency
periodogram(residuals(m.g)[-1],main='Plot of Periodogram')
spec(residuals(m.g)[-1],spans=16)

#Goodness of Fit
ht.garch=m.g$fit[,1]^2 
plot(ht.garch,ylab='Conditional Variance',xlab='Year',
     main='Plot of Conditional Variances')
fit=fitted.values(m1.arima) 
low=fit-1.96*sqrt(ht.garch) 
high=fit+1.96*sqrt(ht.garch) 
plot(v.diff,ylab='1st Diff and Log of Search Interest',xlab='Year',type='l',
  ylim=range(c(fitted(m1.arima),v.diff)),col=rgb(red=0,green=0,blue=0,alpha=1),
  main='95% Confidence Interval of Predicted Data in Red, Observed Data in Black')
lines(low,col=rgb(red=1,green=0,blue=0,alpha=0.8),lty=1)
lines(high,col=rgb(red=1,green=0,blue=0,alpha=0.8),lty=1)

#Forecasting
forecast.arima=plot(m.arima,n.ahead=52,type="l")
predict.v=forecast.arima$pred[1:52]
y=c()
t=tail(v,n=1)
for(i in 1:52){
  t=t+predict.v[i]
  y=c(y,t)
}
sim=garch.sim(alpha=c(6.033372e-05,1.486725e-01),beta=7.382327e-01,n=52)
low.y=y-1.96*abs(sim)
high.y=y+1.96*abs(sim)
pred.v=c(rep(NA,259),tail(v.org,n=1),exp(y))
pred.low=c(rep(NA,259),tail(v.org,n=1),exp(low.y))
pred.high=c(rep(NA,259),tail(v.org,n=1),exp(high.y))
plot(ts(c(v.org,rep(NA,52)),start=2006,frequency=26),
     ylab='Bi-weekly Search Interest',xlab='Year',type='l',
     ylim=range(c(90,160)),col=rgb(red=0,green=0,blue=0,alpha=0.9),
     main='95% Confidence Interval in Red Dotted Line, Predicated Data in Black Dotted line')
points(ts(pred.v,start=2006,frequency=26),type='l',lty=2)
points(ts(pred.low,start=2006,frequency=26),type='l',
       col=rgb(red=1,green=0,blue=0,alpha=1),lty=2)
points(ts(pred.high,start=2006,frequency=26),type='l',
       col=rgb(red=1,green=0,blue=0,alpha=1),lty=2)
