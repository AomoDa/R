---
title: "hw3"
author: "Your Nmae"
date: "2016年11月23日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q1

```{r, message=FALSE, warning=FALSE}
library(stringr)
library(tidyr)
library(dplyr)
x <- read.csv('C://Users//mali//Documents//data_sample.csv')
xx <- gather(x,Country.Name,na.rm = T)
names(xx) <- c('Country.Name','time','value')
xx$year <- as.numeric(str_extract(xx$time,pattern = '[0-9]{4}'))
xx$quarter <-str_extract(xx$time,pattern = 'Q[1-4]')
xx$mon_num <- as.numeric(str_sub(str_extract(xx$time,pattern = 'M[0-9]{1,2}'),start = 2))

##  data output
year.data <- xx[ is.na(xx$quarter) & is.na(xx$mon_num) ,c(1,4,3) ]
head(year.data)
quarter.data <- xx[!is.na(xx$quarter) ,c(1,4,5,3) ]
head(quarter.data)
month.data <- xx[!is.na(xx$mon_num) ,c(1,4,6,3) ]
month.data$month <-factor(month.data$mon_num,levels = 1:12,labels = month.name)
month.data <-month.data[,-3]
head(month.data )

## write.csv
write.csv(year.data,'C://Users//mali//Documents//year.csv')
write.csv(quarter.data,'C://Users//mali//Documents//quarter.csv')
write.csv(month.data,'C://Users//mali//Documents//month.csv')
```

#Q2

```{r}
#chisq
set.seed(100)
x1 <- rchisq(10,df = 3)
x2 <- rchisq(20,df = 3)
x3 <- rchisq(40,df = 3)
x4 <- rchisq(100,df = 3)

par(mfrow=c(2,2))
plot(density(x1),main='chisq n=10,df=3')
plot(density(x2),main='chisq n=20,df=3')
plot(density(x3),main='chisq n=40,df=3')
plot(density(x4),main='chisq n=100,df=3')
par(mfrow=c(1,1))


##CTI
set.seed(100)
x1 <- replicate(100,mean(rchisq(10,df = 3)))
x2 <- replicate(100,mean(rchisq(10,df = 3)))
x3 <- replicate(100,mean(rchisq(10,df = 3)))
x4 <- replicate(100,mean(rchisq(10,df = 3)))

par(mfrow=c(2,2))
plot(density(x1),main='CTL with chisq n=10,df=3')
plot(density(x2),main='CTL with chisq n=20,df=3')
plot(density(x3),main='CTL with chisq n=40,df=3')
plot(density(x4),main='CTL with chisq n=100,df=3')
par(mfrow=c(1,1))
```

#Q3

```{r}
##1sd
pnorm(q=1,mean=0,sd=1)-pnorm(q=0,mean=0,sd=1)
pnorm(q=0,mean=0,sd=1)-pnorm(q=-1,mean=0,sd=1)

##2sd
pnorm(q=2,mean=0,sd=1)-pnorm(q=1,mean=0,sd=1)
pnorm(q=-1,mean=0,sd=1)-pnorm(q=-2,mean=0,sd=1)

##3sd

pnorm(q=3,mean=0,sd=1)-pnorm(q=2,mean=0,sd=1)
pnorm(q=-2,mean=0,sd=1)-pnorm(q=-3,mean=0,sd=1)

## other
1-pnorm(q=3,mean=0,sd=1)
pnorm(q=-3,mean=0,sd=1)

```

#Q4

##lm

```{r}
library(ggplot2)
lm(mpg~hp,data=mtcars)
```

##nls

```{r}
nls(mpg~a+b*hp,data=mtcars,start=list(a=1,b=0))
```

##mle

```{r, message=FALSE, warning=FALSE}
library(bbmle)
LL <- function(a, b, mu, sigma){
   R = mtcars$mpg - mtcars$hp *b - a 
   -sum(dnorm(R, mu, sigma, log = TRUE))

}
mle2(LL, start = list(a = 1, b = 1, mu = 1, sigma = 1), fixed = list(mu = 0))
```

