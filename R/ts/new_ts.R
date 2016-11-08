
library(stringr)
library(mice)
library(ggplot2)
library(fBasics)
library(plyr)


## PART 1
#import data

prescription <- read.csv('input_prescription.csv',header = T,stringsAsFactors = F)
supply <- read.csv('output_supply.csv',header = T,stringsAsFactors = F)
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

#-------------------------------------------------------------------------------------

full_supply <- get_full_data(supply)
impute_supply <- mice_impute_pmm(full_supply)


## PART 2 


with(impute_supply,
     plot(Time,impute_amount,
         type='l',lty=1,col='red',
         main='amount plots'))



par(mfrow=c(2,2))
hist(impute_supply$impute_amount,xlim=c(0,1e7),
     breaks = 5e2,freq = F,
     main='Histogram of impute_amount')
lines(density(impute_supply$impute_amount),col='red')
qqnormPlot(impute_supply$impute_amount)
boxplot(impute_supply$impute_amount,
        ylim=c(-1e5,1e7),
        horizontal = T,
        main='boxplot of impute_amount')
par(mfrow=c(1,1))


par(mfrow=c(1,2))
acf(impute_supply$impute_amount,main='acf of impute_amount')
pacf(impute_supply$impute_amount,main='pacf of impute_amount')
par(mfrow=c(1,1))


