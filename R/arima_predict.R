
rm(list=ls())
install.packages("fBasics")
install.packages("psych")
install.packages('forecast')

library(psych)
library(fBasics)
library(forecast)
library(stats)

# Bring in Data
Tech.Firms.Data <- read.csv("Tech Firms Data.csv",na.strings="NA")
data<-Tech.Firms.Data
dim(data)
data[seq(1,5,1),]

# Log Returns
data<-na.omit(data)
date<-data[1:(nrow(data)-1),1,drop=FALSE]
dim(date)
lret<-100*((log(data[2:nrow(data),2:6])-log(data[1:nrow(data)-1,2:6])))
dim(data)
ldata<-cbind(date,lret)
ldata[seq(500,503,1),]
ldata1<-subset(ldata,select=2:ncol(data))

# Table of Summary Statistics
summary=describe(ldata1)
dim(summary)
fBtable=t(basicStats(ldata1,ci=0.95))
dim(fBtable)
n=nrow(ldata1)

# Test Skewness
# H0: skewness=0
s1=summary[,11]
t1=s1/sqrt(6/n)
pvalue.skew=2*(1-pnorm(abs(t1)))
decision.skew=as.logical(1-(pvalue.skew<=0.05))
decision.skew

# Test Excess Kurtosis
# H0: excess kurtosis=0
k1=summary[,12]
t2=k1/sqrt(24/n)
pvalue.kurt=2*(1-pnorm(abs(t2)))
decision.kurt=as.logical(1-(pvalue.kurt<=0.05))  
decision.kurt

##############################  ARIMA  ################################
ARMA0<-matrix(c(1:300),nrow = 60,ncol = 5) # 建立一个60x5的matrix，一共是300个值
for (i in 1:5) {
  ARMA0[,i]<-predict((arima(ldata1[,i],order = c(0,0,0))), n.ahead = 60)$pred[1:60] # 填充这个matrix
}
colnames(ARMA0)<-colnames(ldata1) # 用ldata1的列名替换ARMA0的列名
ARMA0<-data.frame(ARMA0) # 转化成数据集模式

# Convert into Price
ARMAT=ARMA0/100
lretap<-111.49
plretap<-lretap*exp(ARMAT[,1:5])



price4cast = function(s){
  newP <- data.frame(a=numeric(),b=numeric(),c=numeric(),d=numeric(),e=numeric())
  S <- matrix(c(111.49,805.48,59.8,34.52,152.79),nrow = 1,ncol = 5)
  for (i in 1:60){
      newP[i,] <- as.numeric(((exp( (s[i,])/100 ))*S ))
      S <- newP[i,]
    }
    return(newP)
  }

pARMA0<-price4cast(ARMA0)#forecasted next 60 days price based on ARMA(0,0)
pARMA1<-price4cast(ARMA1)#forecasted next 60 days price based on ARMA(1,1)
pARMAm<-price4cast(ARMAm)#forecasted next 60 days price based on ARMA(p,q)




row.names(ldata1) <- 1:503
row.names(ARMA0) <- 504:563	
ARMA00<-rbind(ldata1,ARMA0)



######################### Plot Picture ##################################
#attach(ARMA0)
ARMA00<-rbind(ldata1,ARMA0) # 顺序乱了
xdata<-seq(1,nrow(ARMA00),1) 
par(mfrow=c(3,2))
plot(xdata,ARMA00[,1],type="l",main = "AAPL ARMA(0,0)",xlab = "Days", ylab = "Log Returns")
plot(xdata,ARMA00[,2],type="l",main = "GOOGL ARMA(0,0)",xlab = "Days", ylab = "Log Returns")
plot(xdata,ARMA00[,3],type="l",main = "MSFT ARMA(0,0)",xlab = "Days", ylab = "Log Returns")
plot(xdata,ARMA00[,4],type="l",main = "INTC ARMA(0,0)",xlab = "Days", ylab = "Log Returns")
plot(xdata,ARMA00[,5],type="l",main = "IBM ARMA(0,0)",xlab = "Days", ylab = "Log Returns")

######################### Forecast Accuracy ################################

orders<-matrix(c(0,0,0,0,0,0,1,0,0,0),ncol = 2,nrow = 5)
half<-(length(ldata1[,1]))/2
half1<-ldata1[1:half,]
half2<-ldata1[(half+1):(2*half),]
f4cast0<-c()
f4cast1<-c()
dm.pvalue<-c()
enc.pvalue<-c()
for (i in 1:5) {
  for (m in 1:half) {
    f4cast0[m]<-predict((arima(ldata1[m:(half+m-1),i],order=c(orders[i,1],0,orders[i,2]),method = "ML")),1)$pred[1]
    f4cast1[m]<-predict((arima(ldata1[m:(half+m-1),i],order=c(1,0,1),method = "ML")),1)$pred[1]
  }
  error0<-f4cast0-half2[,i]
  error1<-f4cast1-half2[,i]
  dm.pvalue[i]<-dm.test(error0,error1)$p.value #Diebold-Mariano Statistic Method
  enc.value<-(sum((error1*(error1-error0)/half)))/(var(error1*(error1-error0))) #Encompassing statistic value
  enc.pvalue[i]<-2*(1-pnorm(abs(enc.value),0,1)) #Encompassing p-value under the N(0,1)
}

############################ Forecast daily return ##############################
plot.ts(ldata1[,1])
fit1=arima(ldata1[,1], order = c(0,0,0), method = "ML",include.mean = TRUE)
plot(forecast.Arima(fit1,h=60),xlim=c(0,563),main="Arima Forecast (0,0,0)")
fitted.values(fit1) 
points(fitted.values(fit1),col='red',type='l')

# Forecasting accuracy test
m1=arima(ldata1[1:251,1],order = c(0,0,0), method = "ML",include.mean = TRUE)
names(m1)
pm1=predict(arima(ldata1[,1],order=c(0,0,0)),n.ahead=252)
names(pm1)
pm1
pred1=pm1$pred
e1=(ldata1[252:n,1]-pred1)^2

m2=arima(ldata1[1:251,1],order = c(1,0,1), method = "ML",include.mean = TRUE)
pm2=predict(arima(ldata1[,1],order=c(1,0,1)),n.ahead=252)
pred2=pm2$pred
e2=(ldata1[252:n,1]-pred2)^2

dm.test(e1, e2,h=252, power=2)


