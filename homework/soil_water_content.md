---
title: "hw"
author: "Your Nmae"
date: "2017-01-09"
output: word_document
---


##Data Prepare

- import the dataset into R
- library packages
- transformation hourly data to daily data
- descriptive statistics
- Histograms tell me that the data of volumetric soil Water content are not from normal distribution.


```{r, message=FALSE, warning=FALSE}
#----------------------------------------------
#Hourly soil water content/water tension readings
#Date
#	day/month/year hour:minute
#Julian day
#	Julian date (jan 1=1, dec 31=365)
#Hour
#	Hours of the day, from 0 to 23
#Volumetric Soil Water Content
#	Volumetric Soil Water Content[m3/m3]
#Pore water tension
#	0 probe below water table level & 1 probe above water table level
#-------------------------------------------------

library(plyr)
library(fractal)
library(ggplot2)

# loading data into R
x <- read.csv('C://Users//mali//Documents//data.csv',
              header = T,
              stringsAsFactors = F)
x <- na.omit(x)
x$Date <- as.Date(x$Date)

# daily data
mydata<- ddply(.data = x,.variables = .(Date),.fun = summarise,
	V_A_0.3=mean(A.group.probes..0.3.m.depth),
	V_A_0.6=mean(A.group.probes..0.6.m.depth),
	V_A_0.9=mean(A.group.probes..0.9.m.depth),
	V_A_1.5=mean(A.group.probes..1.5.m.depth),
	V_C_0.6=mean(C.group.probes..0.6.m.depth),
	V_C_0.9=mean(C.group.probes..0.9.m.depth),
	V_C_1.5=mean(C.group.probes..1.5.m.depth),
    P_A_0.3=max(A.group.probes..0.3.m.depth.1),
    P_C_0.3=max(C.group.probes..0.3.m.depth)
	)
# display the structure of my dataset 
str(mydata)
# data summaries
summary(mydata)
#Histograms
hist(mydata$V_A_0.3,probability = T,main='A-group probes, 0.3 m depth \n Daily Data',xlab='Volumetric Soil Water Content')
lines(density(mydata$V_A_0.3),col='red')
hist(mydata$V_A_0.6,probability = T,main='A-group probes, 0.6 m depth \n Daily Data',xlab='Volumetric Soil Water Content')
lines(density(mydata$V_A_0.6),col='red')
hist(mydata$V_A_0.9,probability = T,main='A-group probes, 0.9 m depth \n Daily Data',xlab='Volumetric Soil Water Content')
lines(density(mydata$V_A_0.9),col='red')
hist(mydata$V_A_1.5,probability = T,main='A-group probes, 1.5 m depth \n Daily Data',xlab='Volumetric Soil Water Content')
lines(density(mydata$V_A_1.5),col='red')
hist(mydata$V_C_0.6,probability = T,main='C-group probes, 0.6 m depth \n Daily Data',xlab='Volumetric Soil Water Content')
lines(density(mydata$V_C_0.6),col='red')
hist(mydata$V_C_0.9,probability = T,main='C-group probes, 0.9 m depth \n Daily Data',xlab='Volumetric Soil Water Content')
lines(density(mydata$V_C_0.6),col='red')
hist(mydata$V_C_1.5,probability = T,main='C-group probes, 1.5 m depth \n Daily Data',xlab='Volumetric Soil Water Content')
lines(density(mydata$V_C_1.5),col='red')
```


##part 1

Testing for stationarity in a time series

$$H_0:stationarity$$
$$H_1:non-stationarity$$

The p-value of all time series is less than 0.05 which means that all time series are non-stationarity.


```{r}
#------------------------
#part 1
#------------------------
#Testing for stationarity 
stationarity(mydata$V_A_0.3)
stationarity(mydata$V_A_0.6)
stationarity(mydata$V_A_0.9)
stationarity(mydata$V_A_1.5)
stationarity(mydata$V_C_0.6)
stationarity(mydata$V_C_0.9)
stationarity(mydata$V_C_1.5)

# simple plot vs time
plot(mydata$Date,mydata$V_A_0.3,type='l',main='A-group probes, 0.3 m depth \n Daily Data',xlab='DATE',ylab='')
plot(mydata$Date,mydata$V_A_0.6,type='l',main='A-group probes, 0.6 m depth \n Daily Data',xlab='DATE',ylab='')
plot(mydata$Date,mydata$V_A_0.9,type='l',main='A-group probes, 0.9 m depth \n Daily Data',xlab='DATE',ylab='')
plot(mydata$Date,mydata$V_A_1.5,type='l',main='A-group probes, 1.5 m depth \n Daily Data',xlab='DATE',ylab='')
plot(mydata$Date,mydata$V_C_0.6,type='l',main='A-group probes, 0.6 m depth \n Daily Data',xlab='DATE',ylab='')
plot(mydata$Date,mydata$V_C_0.9,type='l',main='A-group probes, 0.9 m depth \n Daily Data',xlab='DATE',ylab='')
plot(mydata$Date,mydata$V_C_1.5,type='l',main='A-group probes, 1.5 m depth \n Daily Data',xlab='DATE',ylab='')

```

##part 2

- when months 6,7 and 8  comes,the probe above water table level is 1.
- Pore water tension is greatly effected by the seasons.

```{r}
mydata$mon <- as.numeric(substr(mydata$Date,6,7))

# barplot
ggplot(data=mydata,aes(x=as.factor(mon),fill=as.factor(P_A_0.3)))+
    geom_bar()+labs(x='month',y='',title='Pore water tension \n A-group probes, 0.3 m depth')

ggplot(data=mydata,aes(x=as.factor(mon),fill=as.factor(P_C_0.3)))+
    geom_bar()+labs(x='month',y='',title='Pore water tension \n C-group probes, 0.3 m depth')

# mosaicplot
mosaicplot(table(mydata$mon,mydata$P_A_0.3),shade = T)
mosaicplot(table(mydata$mon,mydata$P_C_0.3),shade = T)
```


##part 3

### DEPTH

- boxplot of different depth 
- t test

#### GROUP A

- The boxplot of different depth in group A tells me that the mean of volumetric soil water with 0.9 m depth is maximum.
- The boxplot of different depth in group A tells me that the mean of volumetric soil water with 0.3 m depth is minimum.
- $V_{0.3}<V_{0.6}<V_{1.5}<V_{0.9}$


```{r}
par(las=2)
boxplot(mydata[,2:5],col=2:5,main='Volumetric Soil Water Content')
par(las=1)

t.test(mydata$V_A_0.3,mydata$V_A_0.6,alternative = 'greater')
t.test(mydata$V_A_0.6,mydata$V_A_0.9,alternative = 'greater')
t.test(mydata$V_A_0.9,mydata$V_A_1.5,alternative = 'less')

```


#### GROUP B

- The boxplot of different depth in group B tells me that the mean of volumetric soil water with 0.3 m depth is maximum.
- The boxplot of different depth in group B tells me that the mean of volumetric soil water with 0.9 m depth is minimum.
- $V_{0.9}<V_{1.5}<V_{0.6}$

```{r}
par(las=2)
boxplot(mydata[,6:8],col=2:4,main='Volumetric Soil Water Content')
par(las=1)

t.test(mydata$V_C_0.6,mydata$V_C_0.9,alternative = 'less')
t.test(mydata$V_C_0.9,mydata$V_C_1.5,alternative = 'greater')

```


###SEASON

Volumetric soil water is greatly effected by the seasons.

```{r}
# boxplot vs season
ggplot(data=mydata,aes(x=as.factor(mon),y=V_A_0.3,fill=as.factor(mon)))+
   geom_boxplot(show.legend = F)+labs(x='month',title='A-group probes, 0.3 m depth \n Daily Data')

ggplot(data=mydata,aes(x=as.factor(mon),y=V_A_0.6,fill=as.factor(mon)))+
   geom_boxplot(show.legend = F)+labs(x='month',title='A-group probes, 0.6 m depth \n Daily Data')

ggplot(data=mydata,aes(x=as.factor(mon),y=V_A_0.9,fill=as.factor(mon)))+
   geom_boxplot(show.legend = F)+labs(x='month',title='A-group probes, 0.9 m depth \n Daily Data')

ggplot(data=mydata,aes(x=as.factor(mon),y=V_A_1.5,fill=as.factor(mon)))+
   geom_boxplot(show.legend = F)+labs(x='month',title='A-group probes, 1.5 m depth \n Daily Data')

ggplot(data=mydata,aes(x=as.factor(mon),y=V_C_0.6,fill=as.factor(mon)))+
   geom_boxplot(show.legend = F)+labs(x='month',title='C-group probes, 0.6 m depth \n Daily Data')

ggplot(data=mydata,aes(x=as.factor(mon),y=V_C_0.9,fill=as.factor(mon)))+
   geom_boxplot(show.legend = F)+labs(x='month',title='C-group probes, 0.9 m depth \n Daily Data')

ggplot(data=mydata,aes(x=as.factor(mon),y=V_C_1.5,fill=as.factor(mon)))+
   geom_boxplot(show.legend = F)+labs(x='month',title='C-group probes, 1.5 m depth \n Daily Data')
```

