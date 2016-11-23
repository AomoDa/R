---
title: "hw1"
author: "Your Nmae"
date: "2016年11月23日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Q1

```{r, message=FALSE, warning=FALSE}
library(reshape2)
mydata <- data.frame(id=c(1,1,2,2),time=c(1,2,1,2),x1=c(5,3,6,2),x2=c(6,5,1,4))
md <- melt(mydata,id=c('id','time'))
md
dcast(md,id~variable+time)


library(tidyr)
unite(mydata,x1_time,x1,time) 
unite(mydata,x2_time,x2,time)
```


#Q2

```{r}
x <- read.csv('C://Users//mali//Documents//companies.csv')
```

##2.1

```{r}
x$margin <- round(x$profit/x$revenue,2)
```

##2.2

```{r}
aggregate(revenue~company,data=x,sum)
```

##2.3

```{r}
aggregate(profit~company,data=x,sum)
```


##2.4


```{r, message=FALSE, warning=FALSE}
x <- x[order(x$fy),]
aggregate(margin~company,data=x,diff)
```


##2.5

```{r}
library(plyr)
ddply(x,.variables = .(company),.fun = summarise,
       revenue=round(sd(revenue),0),
       profit=round(sd(profit),0),
       margin=round(sd(margin),0))
```


##2.6

```{r}
with(x[x$company=='Apple',],plot(fy,margin,type='b',lwd=2,lty=4,col='red',pch=15,ylim=c(0,0.5)))
with(x[x$company=='Google',],points(fy,margin,type='b',lwd=2,lty=2,col='green',pch=16))
with(x[x$company=='Microsoft',],points(fy,margin,type='b',lwd=2,lty=1,col='blue',pch=17))
legend('topleft',col=c('red','green','blue'),lwd=2,pch=15:17,
    legend=c('Apple','Google','Microsoft'),lty=c(4,2,1))
```


#Q3

```{r, message=FALSE, warning=FALSE}
library(tidyr)
library(dplyr)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)
```

