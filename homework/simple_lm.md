---
title: "Untitled"
author: "Your Nmae"
date: "2016-11-08"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##import data

```{r, message=FALSE, warning=FALSE}
library(psych)
library(forecast)
x <- read.csv('C://Users//mali//Documents//Data for Car Accidents.csv')
```

## EDA

### Data summary


```{r}
str(x)
summary(x)
```

###boxplot

每个变量的分布都相对比较均匀，并没有出现离群值。数据分布良好。
```{r}
par(mfrow=c(2,3))
for (i in 2:7) {
 boxplot(x[,i],main=names(x)[i],col=i)
 }
par(mfrow=c(1,1))
```


###Trend plot 

- Licensed.Drivers随时间的增加而增加。
- 其他变量随时间的增加而减少。
- 响应变量有个明显的趋势，暗示我们在拟合线性回归方程的时候可能需要考虑到时间趋势。

```{r}
par(mfrow=c(2,3))
for (i in 2:7) {
 plot(x[,1],x[,i],main=names(x)[i],
      col=i,lty=i,type='l',
      xlab='Year',ylab='')
 }
par(mfrow=c(1,1))
```


###pairs plot

多变量初步分析结果如下：
- 响应变量与所有因变量之间高度相关。
- 响应变量与 Licensed.Drivers成负相关。
- 响应变量与其他变量成正相关。
- 自变量之间相关性比较强，因此拟合的线性回归方程可能存在多重共线性问题。

```{r}
pairs.panels(x)
```


##MODEL

- 选取2013年的数据作为训练样本。
- 选取2013-2014年的数据作为测试样本。
- 将除时间变量之外的所有变量带入回归方程，使用OLS方法进行参数估计。

```{r}
data_train <- x[x$Year<2013,-1]
data_test <- x[x$Year>=2013,-1]

lm0 <-lm(Total.Car.Accidents.in.Illinois~.,data=data_train)

```


###Real values VS Fitted values

从拟合值和实际值的对比情况中，我们发现，线性回归方程拟合比较良好。

```{r}
plot(x[x$Year<2013,1],data_train[,1],type='boxplot',col='blue',
     xlab='',ylab='Total.Car.Accidents.in.Illinois',
     main='Real values VS Fitted values')
points(x[x$Year<2013,1],lm0$fitted.values,type='b',col='red')
legend('topright',col=c('blue','red'),lty=1,
    legend=c('Real values','Fitted values'))
```



### model diagnosis

从线性回归方程诊断图中的，残差部分和响应变量之间可能存在某种相关，但整体影响不大。

```{r}
par(mfrow=c(2,2))
plot(lm0)
par(mfrow=c(1,1))
```


##Predict


进行预测，从预测结果图中可以看出，实际值落在预测区间内，说明预测良好。

```{r}
pred <- forecast.lm(object = lm0,newdata = data_test,level = 0.95)
plot(x[x$Year>=2013,1],pred$mean,type='b',
     ylim=c(100000,420000),xlab='Year',
     main='Real values VS Predicted values')
lines(x[x$Year>=2013,1],x[x$Year>=2013,2],type='b',col='red')
lines(x[x$Year>=2013,1],pred$upper,type='b',lty=2,col='blue')
lines(x[x$Year>=2013,1],pred$lower,type='b',lty=2,col='blue')
legend('bottomleft',col=c('black','red','blue','blue'),ncol=2,
       lty=c(1,1,2,2),legend=c('Predicted values','Real values','upper','lower'))
```

##Conclusion

线性回归方程的系数和截距如下：

```{r}
round(lm0$coefficients,2)
```

