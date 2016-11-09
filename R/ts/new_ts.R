
library(stringr)
library(mice)
library(ggplot2)
library(car)
library(plyr)
library(TSA)
library(lubridate)


#------------------------------------------------------------------------------
#Part 1 Multivariate Imputation by Chained Equations
#------------------------------------------------------------------------------

#import data

prescription <- read.csv('prescription.csv',header = T,stringsAsFactors = F)
supply <- read.csv('supply.csv',header = T,stringsAsFactors = F)
prescription$Time <- as.Date(prescription$Time)
supply$Time <- as.Date(supply$Time)

#-------------------------------------------------------------------------------------
## get full data
get_full_data <- function(x) {
 min_date <-min(x$Time)
 max_date <-max(x$Time)
 ind <- nrow(x)+1
 while(min_date <= max_date){
   if(nrow(x[x$Time==min_date,])==0){
      x[ind,1] <- min_date
      x[ind,2] <- NA
      ind <- ind +1
      print(paste('Insert NA of date=', min_date,sep='') )
    }
    min_date <- min_date +1
 }
 x <- x[order(x$Time),]
 return(x) 
}


## Multivariate Imputation by Chained Equations (MICE)
mice_impute_pmm <- function(x) {
  require(mice)
  impute_data <- data.frame(ind=1:nrow(x),Amount=x$Amount)
  fit1 <- mice(impute_data)
  x$impute_amount <-as.vector(complete(fit1)[,2])
  return(x)
}

#supply
full_supply <- get_full_data(supply)
impute_supply <- mice_impute_pmm(full_supply)

#prescription
full_prescription <- get_full_data(prescription)
impute_prescription <- mice_impute_pmm(full_prescription)

#------------------------------------------------------------------------------
#Part 2 EDA
#------------------------------------------------------------------------------


with(impute_supply,
     plot(Time,impute_amount,
         type='l',lty=1,col='red',
         main='amount plots'))


##hist and qqplot
par(mfrow=c(1,2))
hist(impute_supply$impute_amount,xlim=c(0,1e7),
     breaks = 5e2,freq = F,
     main='Histogram of \n impute_amount')
lines(density(impute_supply$impute_amount),col='red')
qqPlot(impute_supply$impute_amount,main='qqplot of \n impute_amount ')
par(mfrow=c(1,1))


##boxplot
par(mfrow=c(1,2))
boxplot(impute_supply$impute_amount,
        ylim=c(-1e5,1e7),
        horizontal = T,
        main='boxplot of \n impute_amount')
with(impute_supply,
    boxplot(Amount~month(impute_supply$Time),ylim=c(0,1e7),
        main='boxplot of \n monthly data',col=2:13))
par(mfrow=c(1,1))

##acf 
par(mfrow=c(1,2))
acf(impute_supply$impute_amount,main='acf of impute_amount')
pacf(impute_supply$impute_amount,main='pacf of impute_amount')
par(mfrow=c(1,1))


##periodogram
### orgain
par(mfrow=c(1,2))
with(impute_supply,spec.pgram(impute_amount))
abline(v=1/30,col='blue',lwd=1,lty=2)
text(x=1/15,y=1e11,labels = 'x=1/30',col='red')

periodogram(impute_supply$impute_amount,main='periodogram of \n impute_amount')
##abline(v=1/30,col='red',lwd=2)
text(x=1/15,y=4e15,labels = 'x=1/30',col='BLUE')
par(mfrow=c(1,1))

###log

par(mfrow=c(1,2))
with(impute_supply,spec.pgram(log(impute_amount)))
abline(v=1/30,col='blue',lwd=1,lty=2)
abline(v=1/7,col='blue',lwd=1,lty=2)
text(x=1/15,y=1e-1,labels = 'x=1/30',col='red')
text(x=1/5,y=1e-2,labels = 'x=1/7',col='red')

periodogram( log(impute_supply$impute_amount),main='periodogram of \n log(impute_amount)')
#abline(v=1/30,col='blue',lwd=2)
#abline(v=1/7,col='blue',lwd=1,lty=2)
text(x=1/15,y=100,labels = 'x=1/30',col='red')
text(x=1/5,y=150,labels = 'x=1/7',col='red')
par(mfrow=c(1,1))




supply_ts <- ts(log(impute_supply$impute_amount),frequency = 30)
plot(decompose(supply_ts,type = 'additive'))



#------------------------------------------------------------------------------
#Part 3 TIME SERIES MODEL
#------------------------------------------------------------------------------

## data
supply_ts_train <- ts(log(impute_supply$impute_amount[1:616]),
                     frequency = 30,start=c(1,1))
supply_ts_test <- ts(log(impute_supply$impute_amount[617:676]),
                    frequency = 30,start=c(21,17))


#------------------------------------------------------------------------------

##HoltWinters
hw_model <- HoltWinters(supply_ts_train)

###residuals
par(mfrow=c(1,3))
plot.ts(residuals(hw_model),xlab='Time',ylab='residuals',
     main='residuals of \n HoltWinters model')
abline(h=0,lty=2,col='red')
hist(residuals(hw_model),breaks = 20,xlim=c(-4,4),
     main='Histogram of \n HoltWinters model residuals')
qqPlot(residuals(hw_model),
     main='qqplot of \n HoltWinters model residuals')
par(mfrow=c(1,1))


### shapiro.test
shapiro.test(residuals(hw_model))

### predict

pred_hw  <- forecast.HoltWinters(hw_model,h=60,level = 0.95)

plot(pred_hw)
points(supply_ts_test,type='l')
points(hw_model$fitted[,1],type='l',col='red')
legend('topleft',legend=c('real valu','fitted value','predict value'),
        col=c('black','red','blue'),lty=1,cex=0.5)
abline(v=21.5667,lty=2)


#------------------------------------------------------------------------------

## Seasonal ARIMA Model

sarima <- auto.arima(supply_ts_train,trace = T,ic='aic')

###residuals
par(mfrow=c(1,3))
plot.ts(residuals(sarima),xlab='Time',ylab='residuals',
     main='residuals of \n HoltWinters model')
abline(h=0,lty=2,col='red')
hist(residuals(sarima),breaks = 20,xlim=c(-2,2),
     main='Histogram of \n HoltWinters model residuals')
qqPlot(residuals(sarima),
     main='qqplot of \n HoltWinters model residuals')
par(mfrow=c(1,1))


### shapiro.test
shapiro.test(residuals(sarima))

### predict
pred_sarima <- forecast.Arima(sarima,h=60,level = 0.95)

plot(pred_sarima,ylim=c(11,18))
points(supply_ts_test,type='l')
points(fitted.Arima(pred_sarima),type='l',col='red')
legend('topleft',legend=c('real valu','fitted value','predict value'),
        col=c('black','red','blue'),lty=1,cex=0.5)
abline(v=21.5667,lty=2)


#------------------------------------------------------------------------------
#Part 4 
#------------------------------------------------------------------------------

##merge data

get_data_merge <- function(){
 a <- impute_supply[,c(1,3)]
 b <- impute_prescription[,c(1,3)]
 names(a)[2] <- 'supply'
 names(b)[2] <- 'prescription'
 data_ok <- merge(a,b)
 return(data_ok)
}

data_ok <- get_data_merge()

lm0 <- lm(supply~prescription,data=data_ok)
lm1 <- lm(log(supply)~log(prescription),data=data_ok)
