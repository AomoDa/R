---
title: "Untitled"
author: "Your Nmae"
date: "2016-11-10"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##1.Abstract

- Write your own
- Write your own
- Write your own
- Write your own


##2.Introduction and Background

- Write your own
- Write your own
- Write your own
- Write your own

##3.Data preprocessing

###3.1.Load data into R

- Import some packages,included "psych" and "forecast".
- Load data ,which will analysis with.

##3.2.Descriptive analysis

According to the following table, we can konw  the general distribution of the data is good.Because the mean and the median are close.

-----

item|X|Y
----|--|--
Min|10.51|-1923.9  
1st Qu|34.29| -823.9  
Median|53.42 | -632.4  
Mean|58.06| -589.8  
3rd Qu|90.36| -333.7  
Max|98.79|  751.5  

-----

```{r, message=FALSE, warning=FALSE}
library(psych)
library(forecast)
## part1
mid_data <- read.csv('C://Users//mali//Documents//MidtermData9 (LUCILLE).csv')
head(mid_data)
str(mid_data)
summary(mid_data)
```


##4.EDA


### 4.1.normal test

- For predictor variable $x$,**the pvalue of Shapiro-Wilk normality test is 0.001727**,which is  very statistically significant at 0.05  level. So We can reject $H_0$ and distribution of predictor variable $x$ is not normal.According to  histogram and Q-Q plot, we can also draw the same conclusion.

- For response  variable $y$,**the pvalue of Shapiro-Wilk normality test is 0.2047**,which is  not statistically significant at 0.05  level. So We can not  reject $H_0$ and distribution of response variable $y$ is  normal.According to  histogram and Q-Q plot, we can also draw the same conclusion.


```{r}
par(mfrow=c(2,2))
hist(mid_data$x)
qqnorm(mid_data$x)
qqline(mid_data$x)
hist(mid_data$y)
qqnorm(mid_data$y)
qqline(mid_data$y)
par(mfrow=c(1,1))

shapiro.test(mid_data$x)
shapiro.test(mid_data$y)
```

###4.2.pairs and corr test

the pearson correlation coefficient of response  variable $y$ and predictor variable $x$ is -0.74,which indicated $x$ and $y$ is negatively correlated and the more $x$ and the less $y$.

corr.test results show that correlation analysis is correct ,because of the p value is zero.

```{r}
pairs.panels(mid_data)
corr.test(mid_data)
```


### 4.3.plot 



```{r}
with(mid_data,plot(x,y,main='Scatter Plots with  Linear Model'))
abline(lm(y~x,data=mid_data),col='red',lwd=1,lty=1)
abline(lm(y~x-1,data=mid_data),col='blue',lwd=2,lty=3)
legend('topright',col=c('red','blue'),
       lwd=1:2,lty=c(1,3),cex=0.6,
       legend=c('Linear Model with Intercept',
       	        'Linear Model without Intercept'))
```

##5.MODEL

### 5.1.train data and test data

接下来进行模型分析，抽取原始数据的80%作为 train data,剩下的20%作为test data.抽样过程为随机不重复抽样。


```{r}
set.seed(100)
ind <- sample(x=1:2,size=nrow(mid_data),replace = T,prob = c(0.8,0.2))
mid_data.train <-mid_data[ind==1,]
mid_data.test  <-mid_data[ind==2,]
```

### 5.2fit models

结合前面的分析，我们分别拟合2个模型，

- fit1 :Linear Model with the Intercept.
- fit2 :Linear Model without the  Intercept.




**fit1**

- $F-statistic: 42.71$ and $DF1=1,DF2=37$,$p-value=0$ .,which shows that The linear regression equation was significant.
- $R_{adj}^2=52.32%$,which show that linear regression equations to be combined is not good.

-----

item|Estimate| Std. Error| t value|Pr(>|t|)
----|--------|-----------|--------|--------
(Intercept) | 136.981  |  117.616  |  1.165 |0.252    
x            |-12.490   |   1.911 |  -6.535 | 0

-----


**fit2**

- $F-statistic: 148.3$ and $DF1=1,DF2=38$,$p-value=0$,which shows that The linear regression equation was significant.
- $R_{adj}^2=79.07%$,which show that linear regression equations to be combined is  good.

-----
item|Estimate| Std. Error| t value|Pr(>|t|)
----|--------|-----------|--------|--------
x| -10.5011| 0.8623|-12.18|0

-----


```{r}
fit1 <- lm(y~x,data=mid_data.train)
summary(fit1)
fit2 <- lm(y~x-1,data=mid_data.train)
summary(fit2)

```


## 6.MODEL diagnostics

### 6.1.anova test

Compute analysis of variance (or deviance) tables for one or more fitted model objects.

the p value of Analysis of Variance Table is 0.2516,which is  not statistically significant at 0.05 level.So,fit2 is better.

-----

model|Res.Df|RSS| Df| Sum of Sq|F| Pr(>F)
-----|------|---|---|----------|--|-------
fit1|37| 4025662|                           
fit2|38| 4173241| -1|   -147579| 1.3564| 0.2516

-----


```{r}
anova(fit1,fit2)
```


###6.2.residuals analysis


- residuals均匀的分布在 "0刻度线"上下，但是有个别异常值。
- the histogram of residuals and qqplot of residuals show that the distribution of residuals seems have normal distribution.
- The residual  is consistent with the linear regression hypothesis。Our model is very  good.



```{r}
par(mfrow=c(1,3))
plot(residuals(fit2),main='residuals plot')
abline(h=0,col='red',lty=2)
hist(residuals(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2),col='red',lty=2)
par(mfrow=c(1,1))
```

### 6.3.fitted analysis

从下图可以看出，我们的fit2模型拟合的非常好，fitted values 和 real values都非常接近。

```{r}
with(mid_data,plot(x,y,col='red',type='p',pch=1))
points(mid_data.train$x,as.numeric(fitted.values(fit2)),col='blue',pch=2)
legend('topright',col=c('red','blue'),pch=1:2,
      legend=c('real values','fitted values'))
```



## 7.MODEL Predct

使用train model 去预测我们抽取出来的20%的数据，预测结果如下:红色点代表 test data 中的真是值，蓝色点代表 test data的预测点。结果发现两者非常接近，表明我们的模型能够很好的预测。因此我们的模型是非常完美的。

```{r}
pred <- forecast.lm(object = fit2,newdata = mid_data.test,level = 0.95)

plot(pred)
points(mid_data.test,col='red',pch=16)
legend('topright',col=c('white','blue','red'),pch=c(1,16,16),cex=0.8,
      legend=c('train real values','pred test values','real test values'))
```



##8.discuss

- Write your own
- Write your own
- Write your own
- Write your own

##9.conclusion

Linear regression equation
根据结果，分析得出结论为:$$Y=-10.5011 X + \epsilon$$,其中$\epsilon$为残差项.

##10.Reference

- Write your own
- Write your own
- Write your own
- Write your own

##11.appendix : R code

```{r, eval=FALSE, include=T}
## lm1

library(car)
library(psych)
library(forecast)
## part1
mid_data <- read.csv('MidtermData9 (LUCILLE).csv')
head(mid_data)
str(mid_data)
summary(mid_data)

##EDA
## normal test
par(mfrow=c(2,2))
hist(mid_data$x)
qqnorm(mid_data$x)
qqline(mid_data$x)
hist(mid_data$y)
qqnorm(mid_data$y)
qqline(mid_data$y)
par(mfrow=c(1,1))

shapiro.test(mid_data$x)
shapiro.test(mid_data$y)

##pairs
pairs.panels(mid_data)
corr.test(mid_data)

## plot 

with(mid_data,plot(x,y,main='Scatter Plots with  Linear Model'))
abline(lm(y~x,data=mid_data),col='red',lwd=1,lty=1)
abline(lm(y~x-1,data=mid_data),col='blue',lwd=2,lty=3)
legend('topright',col=c('red','blue'),
       lwd=1:2,lty=c(1,3),cex=0.6,
       legend=c('Linear Model with Intercept',
       	        'Linear Model without Intercept'))
# MODEL
## train data and test data

set.seed(100)
ind <- sample(x=1:2,size=nrow(mid_data),replace = T,prob = c(0.8,0.2))
mid_data.train <-mid_data[ind==1,]
mid_data.test  <-mid_data[ind==2,]


# fit models
fit1 <- lm(y~x,data=mid_data.train)
summary(fit1)
fit2 <- lm(y~x-1,data=mid_data.train)
summary(fit2)

# MODEL diagnostics

##Compute analysis of variance (or deviance) tables for one or more fitted model objects.
anova(fit1,fit2)

##residuals analysis
par(mfrow=c(1,3))
plot(residuals(fit2),main='residuals plot')
abline(h=0,col='red',lty=2)
hist(residuals(fit2))
qqnorm(residuals(fit2))
qqline(residuals(fit2),col='red',lty=2)
par(mfrow=c(1,1))

## fitted analysis

with(mid_data,plot(x,y,col='red',type='p',pch=1))
points(mid_data.train$x,as.numeric(fitted.values(fit2)),col='blue',pch=2)
legend('topright',col=c('red','blue'),pch=1:2,
      legend=c('real values','fitted values'))


# MODEL Predct

pred <- forecast.lm(object = fit2,newdata = mid_data.test,level = 0.95)

plot(pred)
points(mid_data.test,col='red',pch=16)
legend('topright',col=c('white','blue','red'),pch=c(1,16,16),cex=0.8,
      legend=c('train real values','pred test values','real test values'))


```

