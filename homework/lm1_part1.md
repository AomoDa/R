---
title: "part1"
author: "Your Name"
date: "2017-01-09"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Data Prepare

Merge data

- Date
- Rainfall
- Potential.Evapotranspiration
- Surface.Runoff
- Solar.Energy
- Air.Temperature
- Ground.Temperature
- Atmospheric.Pressure


```{r, message=FALSE, warning=FALSE}
library(plyr)
library(psych)
library(ggplot2)
library(MASS)
library(car)
library(effects)
a <- read.csv('C://Users//AomoDa//Documents//part1_01.csv',
              header = T,
              stringsAsFactors = F)
b <- read.csv('C://Users//AomoDa//Documents//part1_02.csv',
              header = T,
              stringsAsFactors = F)
b <- na.omit(b)
a$Date <- as.Date(a$Date)
b$Date <- as.Date(b$Date)
# Solar radiation being negative should not be due to a calibration (this will be a systematic bias) 
# but it may be due to a processing error. As this occurs at night, data should be transformed
b$Solar.Energy <- ifelse(b$Solar.Energy<0,0,b$Solar.Energy)
b_01 <- ddply(.data = b,.variables = .(Date),.fun = summarise,Solar.Energy=sum(Solar.Energy),
	                                                          Air.Temperature=mean(Air.Temperature),
	                                                          Ground.Temperature=mean(Ground.Temperature),
	                                                          Atmospheric.Pressure=mean(Atmospheric.Pressure)
	        )

# merge data
mydata_part1 <- na.omit(join(a[,-2],b_01))
summary(mydata_part1)
```

##Normality 

The data of potential evapotranspiration is from normal distribution.

```{r}
par(mfrow=c(1,2))
hist(mydata_part1$Potential.Evapotranspiration,breaks = 30,probability = T)
lines(density(mydata_part1$Potential.Evapotranspiration),col='red')
qqPlot(mydata_part1$Potential.Evapotranspiration,main='QQplot of \n Potential.Evapotranspiration')
par(mfrow=c(1,1))
```


##Correlation Analysis

The correlation test tell me that

- The correlation coefficient of potential evapotranspiration with Rainfall is  -0.1847717  and the p value is 0.0009139 , which is statistically significant.
- The correlation coefficient of potential evapotranspiration with Surface.Runoff is  -0.5888321  and the p value is 0 , which is statistically significant.
- The correlation coefficient of potential evapotranspiration with Solar.Energy is  0.8421991  and the p value is 0 , which is statistically significant.
- The correlation coefficient of potential evapotranspiration with Air.Temperature is  0.7707586  and the p value is 0 , which is statistically significant.
- The correlation coefficient of potential evapotranspiration with Ground.Temperature is  0.6954752  and the p value is 0 , which is statistically significant.
- The correlation coefficient of potential evapotranspiration with Atmospheric.Pressure is  0.6954752  and the p value is 0.5754 , which is not  statistically significant.


```{r}
pairs.panels(mydata_part1[,-1],method = 'spearman')
cor.test(mydata_part1$Potential.Evapotranspiration,mydata_part1$Rainfall,method = 'spearman')
cor.test(mydata_part1$Potential.Evapotranspiration,mydata_part1$Surface.Runoff,method = 'spearman')
cor.test(mydata_part1$Potential.Evapotranspiration,mydata_part1$Solar.Energy,method = 'spearman')
cor.test(mydata_part1$Potential.Evapotranspiration,mydata_part1$Air.Temperature,method = 'spearman')
cor.test(mydata_part1$Potential.Evapotranspiration,mydata_part1$Ground.Temperature,method = 'spearman')
cor.test(mydata_part1$Potential.Evapotranspiration,mydata_part1$Atmospheric.Pressure,method = 'spearman')
```

##Scatter Plot

```{r}
ggplot(data=mydata_part1,aes(x=Rainfall,y=Potential.Evapotranspiration))+
    geom_point()+geom_smooth(method = 'lm')+labs(title='Rainfall')

ggplot(data=mydata_part1,aes(x=Surface.Runoff,y=Potential.Evapotranspiration))+
    geom_point()+geom_smooth(method = 'lm')+labs(title='Surface.Runoff')

ggplot(data=mydata_part1,aes(x=Solar.Energy,y=Potential.Evapotranspiration))+
     geom_point()+geom_smooth(method = 'lm')+labs(title='Solar.Energy')

ggplot(data=mydata_part1,aes(x=Air.Temperature,y=Potential.Evapotranspiration))+
     geom_point()+geom_smooth(method = 'lm')+labs(title='Air.Temperature')

ggplot(data=mydata_part1,aes(x=Ground.Temperature,y=Potential.Evapotranspiration))+
     geom_point()+geom_smooth(method = 'lm')+labs(title='Ground.Temperature')

ggplot(data=mydata_part1,aes(x=Atmospheric.Pressure,y=Potential.Evapotranspiration))+
    geom_point()+geom_smooth(method = 'lm')+labs(title='Atmospheric.Pressure')
```

## fit model


$$Potential.Evapotranspiration=-0.17+0.02*Rainfall-0.06*Surface.Runoff+0.0003*Solar.Energy +0.04*Air.Temperature+0.03*Ground.Temperature$$



```{r}
lm1 <- lm(Potential.Evapotranspiration~.,data=mydata_part1[,-1])

summary(lm1)
lm2 <- lm(Potential.Evapotranspiration~.-Atmospheric.Pressure,data=mydata_part1[,-1])
summary(lm2)
confint(lm2)
anova(lm1,lm2)
# Linear regression diagnosis
par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))
# DW TEST
durbinWatsonTest(lm2)
#Effects analysis
plot(allEffects(lm2))
# fitted values vs real value
plot(lm2$fitted.values,col='red',pch=3,main='fitted values vs real value')
points(mydata_part1$Potential.Evapotranspiration)
legend('topleft',col=c('black','red'),pch=c(16,3),legend = c('fitted value','real value'))
```

