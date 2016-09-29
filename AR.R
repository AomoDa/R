#clear everything
rm(list = ls())
seted(I:/CUEB/Teaching/ISEM_ECMT_TAMU)

# installs the needed libraries
library(fBasics)
library(dynlm) 
library(graphics)

data = read.csv("I:/CUEB/Teaching/ISEM_ECMT_TAMU/GDP_t3_t12.csv", header=T)
#data<-data[complete.cases(data), ]

gdp = ts(c(data[,1]),start=c(1947,2), frequency=4) 
plot(gdp)

newdata = ts.intersect(gdp, gdpl = lag(gdp,-1),  gdpl2 = lag(gdp,-2), dframe=TRUE)
reg = lm(gdp~gdpl+gdpl2, data=newdata)  ##ar(2) model
summary(reg)
plot(gdp£©
lines(reg$fitted, col="red")

## AC function, ACF or PACF###

autocorr1 = acf(gdp, lag.max = 24)
autocorr1$acf
dev.new()
autocorr2 = acf(gdp, lag.max = 36)
autocorr2$acf

dev.new()
partialautocorr2 = pacf(gdp,lag.max = 24)

Box.test(gdp,lag=4,type='Ljung')

########################################################

library(forecast)

basicStats(gdp)

bsgdp = basicStats(gdp)
bsgdp[16,]

par(mfrow=c(2,1))
plot(gdp)
plot(lag(gdp,-1))

####### 5 Different Methods######

pc1 = lm(gdp~lag(gdp,-1),na.action = NULL)
summary(pc1)

newdata = ts.intersect(gdp, gdpl = lag(gdp,-1),  gdpl2 = lag(gdp,-2), dframe=TRUE)
pc3 = lm(gdp~gdpl,data=newdata,na.action = NULL)
summary(pc3)

pc4 = dynlm(gdp~lag(gdp,-1))
summary(pc4)

ar1 = ar(gdp,aic = 'false',order = 4)
ar1

ar1aic = ar(gdp, order =12)
ar1aic

#ar2 = ar(gdp,aic = 'false',order = 1, demean=F, intercept=T)
#ar2

#ar2ols = ar(gdp,aic = 'false',order = 1, demean=F, intercept=T, method='ols')
#ar2ols

ar4 = arima(gdp,c(4,0,0))
ar4

T = length(gdp)
ar4aic = auto.arima(gdp, max.p = T^0.25, max.q = 0, seasonal = FALSE, ic = 'aic')
ar4aic

plot(gdp)
lines(fitted(ar4),col=2)

###### Make forecasts############
oos = forecast(ar4, h=10, level=c(95), fan=FALSE)
oos

oospred = oos$mean
print(oospred)

########## Recusive estimation################

ns = 40
#h=10
fcast = matrix(NA,nrow=ns,ncol=1)
for(i in ns:1)
{
  ar5 = arima(gdp[1:(T-i)],c(4,0,0))
  fcast[i,] = forecast(ar5,h=1)$mean
}

plot(fcast)
