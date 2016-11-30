---
title: "Untitled"
author: "Your Nmae"
date: "2016-11-30"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Descriptive analysis

- The distribution of `Turnover2011` is relatively discreted,because of the cv is maximum ,which is 0.61.
- original `Income2012` is far greater than `Turnover2011` and Their orders of magnitude are not at a level,which indicated  `Income2012` maybe need a log-transformed.
- Distribution trend of `log Income2012`  is very concentrated and  looks more friendly.


-----

item| Turnover2011 |Income2012 |log_Income2012 
-----|-------------|--------|---------------
Min|0.0200  |  65605    |11.09  
1st Qu|0.7175  | 262074  |12.48  
Median |1.8100   | 349142   |12.76  
Mean   |1.7627    | 375132    |12.69  
3rd Qu|2.6075  | 434439  |12.98  
Max   |3.7700      |1082635   |13.89  
SD|1.08|205717.71|0.56
Coefficient of Variation|0.61|0.55|0.04

-----


```{r, echo=FALSE}
library(ggplot2)
x <- read.table('C://Users//mali//Documents//StaffTurnover.txt',header = T)
x$log_Income2012  <- log(x$Income2012)
# Histogram of original data
```

```{r, echo=FALSE}
ggplot(data = x,aes(x=Income2012))+
   geom_histogram(bins = 20,col=I('white'))+
   labs(x=expression(Income2012),
      	title='Histogram of original data')
```


#Correlation analysis


Use correlation test  of  `Turnover2011` with `Income2012` and `log Income2012` to test andfind  which variable is more suitable for my model. The p value of correlation test of `Turnover2011` with `log Income2012` is statistically significant as 0.05 level,but `Income2012` not.


-----

$COR \ Turnover2011$|correlation \n coefficient|t value|df|p value|conclusion
-----|----------------------|-------|-----|--------|-----
Income2012|0.24|-1.78|54|0.08|fail to reject $H_0$
log Income2012|-0.39|3.13|54|0.00|reject $H_0$

-----

```{r, include=FALSE}
#correlation test
cor.test(~x$Turnover2011+x$Income2012)
cor.test(~x$Turnover2011+x$log_Income2012)
```

```{r, echo=FALSE}
# original data
ggplot(data=x,aes(x=Turnover2011,y=Income2012)) + 
      geom_point()+geom_smooth(method = 'lm')+
      labs(y=expression(Income2012),
      	   title='original data VS turnover2011')
# log transformed data
ggplot(data=x,aes(x=Turnover2011,y=log(Income2012) )) + 
      geom_point()+geom_smooth(method = 'lm',col=I('orange'))+
      labs(y=expression(log(Income2012)),
      	   title='log transformed data VS turnover2011')

```


#Fitting Linear Models


Fitted two models $lm1$  useing `original Income2012` and $lm2$ useing `log transformed Income2012`.

- Estimated coefficient of $lm2$ is  significant at 0.05 level,but $lm1$ not .
- F test of $lm2$ is very   significant at 0.05 level ,but $lm1$ not .
- $lm2$ passed residual normality test,but $lm1$ not .
- $lm2$ and $lm1$ passed DW test.



-----

Item |lm1 \n (original Income2012)|lm2 \n (log transformed Income2012)
-----|---------------------|-------------------
Intercept|454297|13.0
Intercept p value|0|0
(log)Turnover2011|44912|0.20
(log)Turnover2011 \n p value|0.08|0
F test value|0.08|0
Adjusted R-squared|0.04|0.13
residuals mean|0|0
residuals shapiro.test \n p value |0|0.307
DW test \n p value |0.044|0.012

----




```{r, message=FALSE, warning=FALSE, include=FALSE}
library(car)
lm1 <-lm(Income2012~Turnover2011,data=x)
lm2 <-lm(log(Income2012)~Turnover2011,data=x)

# residuals mean
mean(lm1$residuals)
mean(lm2$residuals)

# residuals shapiro.test
shapiro.test(lm1$residuals)
shapiro.test(lm2$residuals)

#Durbin-Watson Test for Autocorrelated Errors
durbinWatsonTest(lm1)
durbinWatsonTest(lm2)
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
curve(expr =exp(13.0530-0.2044*x),from = 0,to = 4,
	ylab='Income2012',xlab = 'Turnover2011',main='real vs fitted',
	ylim=c(6e4,1e6),col='red',lty=2,lwd=2)
points(x$Turnover2011,x$Income2012,pch=19,col='orange')
abline(lm1,lty=2,lwd=2,col='blue')
legend('topright',col=c('red','orange','blue'),
	pch=c(NA,19,NA),
	legend=c('log lm','read','lm'),
	lty=c(2,NA,2),ncol=1,cex=0.7)
```

#Conclusion

Our model is $$Income2012 = e^{13.05 - 0.2 \cdot Turnover2011 }$$



#Appendix R Code

```{r, eval=FALSE, message=FALSE, warning=FALSE, include=T}
library(ggplot2)
x <- read.table('StaffTurnover.txt',header = T)
x$log_Income2012  <- log(x$Income2012)
summary(x)
# Histogram of original data
ggplot(data = x,aes(x=Income2012))+
   geom_histogram(bins = 20,col=I('white'))+
   labs(x=expression(Income2012),
      	title='Histogram of original data')
#correlation test
cor.test(~x$Turnover2011+x$Income2012)
cor.test(~x$Turnover2011+x$log_Income2012)
# original data
ggplot(data=x,aes(x=Turnover2011,y=Income2012)) + 
      geom_point()+geom_smooth(method = 'lm')+
      labs(y=expression(Income2012),
      	   title='original data VS turnover2011')
# log transformed data
ggplot(data=x,aes(x=Turnover2011,y=log(Income2012) )) + 
      geom_point()+geom_smooth(method = 'lm',col=I('orange'))+
      labs(y=expression(log(Income2012)),
      	   title='log transformed data VS turnover2011')
#Fitting Linear Models
lm1 <-lm(Income2012~Turnover2011,data=x)
lm2 <-lm(log(Income2012)~Turnover2011,data=x)
# residuals mean
mean(lm1$residuals)
mean(lm2$residuals)
# residuals shapiro.test
shapiro.test(lm1$residuals)
shapiro.test(lm2$residuals)
#Durbin-Watson Test for Autocorrelated Errors
durbinWatsonTest(lm1)
durbinWatsonTest(lm2)

curve(expr =exp(13.0530-0.2044*x),from = 0,to = 4,
	ylab='Income2012',xlab = 'Turnover2011',main='real vs fitted',
	ylim=c(6e4,1e6),col='red',lty=2,lwd=2)
points(x$Turnover2011,x$Income2012,pch=19,col='orange')
abline(lm1,lty=2,lwd=2,col='blue')
legend('topright',col=c('red','orange','blue'),
	pch=c(NA,19,NA),
	legend=c('log lm','read','lm'),
	lty=c(2,NA,2),ncol=1,cex=0.7)

```

