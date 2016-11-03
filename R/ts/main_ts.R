

## PART 1 
library(stringr)
library(mice)
library(ggplot2)
library(fBasics)
library(plyr)

source('myfun.R')

supply <- read.csv('supply.csv',stringsAsFactors = F)
prescription <- read.csv('Prescription.csv',stringsAsFactors = F)

## get full data
supply_full <- get_full_data(x = supply)
prescription_full <- get_full_data(x = prescription)


## Multivariate Imputation by Chained Equations (MICE)
impute_supply <- mice_impute_pmm(x = supply_full)
impute_prescription <- mice_impute_pmm(x = prescription_full)



## PART 2 

hour_mean <- aggregate(complete_data~hour,data=impute_supply,mean)

ggplot(data = impute_supply,aes(date,complete_data))+geom_path()

par(mfrow=c(2,2))
hist(impute_supply$complete_data,xlim=c(-1e4,4e6),
     breaks = 1e2,freq = F)
lines(density(impute_supply$complete_data),col='red')
qqnormPlot(impute_supply$complete_data)
boxplot(impute_supply$complete_data,ylim=c(-1e5,1e6),
        horizontal = T,main='boxplot')
boxplot(complete_data~hour,data = impute_supply,
        ylim=c(0,1e6),main='boxplot by hour')
par(mfrow=c(1,1))


par(mfrow=c(1,2))
acf(impute_supply$complete_data)
pacf(impute_supply$complete_data)
par(mfrow=c(1,1))
