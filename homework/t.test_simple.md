---
title: "HW2"
author: "Your Nmae"
date: '2017-03-29'
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#QUESTION 1

##i

The original data in 2012 and 2015 are both normally distributed.Consider the nature of the data,I will use **one sided paired sample T test**.

```{r, message=FALSE, warning=FALSE, include=FALSE}
x <- read.csv('C://Users//mali//Documents//q1.csv')
shapiro.test(x$X2012)
shapiro.test(x$X2015)
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
boxplot(x[,-1],main='F.1 BoxPlot of breeding Blue Swimmer Crabs \n in   2012 VS 2015')
```

## ii

$$H_0:u_{2012}<u_{2015}$$
$$H_1:u_{2012} \geq u_{2015}$$

## iii

The $t-stat=-1.9792$,$df=31$ and $p-value = 0.9716 >0.05$,which is not statistically significant at 0.05 level and means that we cann't reject $H_0$.The conclusion is the number of breeding Blue Swimmer Crabs has changed from 2012 to 2015,and  the number of 2015 is greater than 2012.

##iv

95 percent confidence interval is $[-9841.759,-\infty]$ and the mean of the differences is -5300.719 ,which means that we cann't reject $H_0$.And  the conclusion is correct.

#QUESTION 2

##i

The original data  are not normally distributed.But after using **natural logarithm algorithm transformation** ,data will be normally distributed.So I will use transformation data to conduct **two sided Two Sample t-test**.

```{r, include=FALSE}
x <- read.csv('C://Users//mali//Documents//q2.csv',header = T,na.strings='null')

x$group <- ifelse(x$Year >=1945 & x$Year<=1960, 'Y1945-1960', ifelse(x$Year >=2000 & x$Year<=2015, 'Y2000-2015', NA))
x$winter_rainfall <- x$Jun+x$Jul+x$Aug
mydata <- na.omit(x[,c('Year','group','winter_rainfall')])
```

```{r, echo=FALSE}
boxplot(log(winter_rainfall)~group,data=mydata,main='F.2 BoxPlot of log-transformed winter rainfall')
```

##ii

$$H_0:u_{1945\_1960}=u_{2000\_2015}$$
$$H_1:u_{1945\_1960} \neq u_{2000\_2015}$$


##iii

The $t-stat=4.5021$,$df=29.985$ and $p-value = 0 <0.05$,which is very statistically significant at 0.05 level and means that we should reject $H_0$.The conclusion is the mean winter rainfall (total June, July, August rainfall) for the period 1945-1960 is different to the mean winter rainfall for the period 2000-2015 .

###iv

95 percent confidence interval is $[0.1771,0.4711]$ and the mean of the differences is $5.8563-6.1804=-0.3241$ ,which means that we should reject $H_0$.And  the conclusion is correct.
