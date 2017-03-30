---
title: "Quiz 2"
author: "Your Nmae"
date: "2017年3月30日"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# QUESTION 1




$$H_0: u_{Temp}=30$$
$$H_1: u_{Temp} \neq 30$$

- The $t-stat=-2.719$,$df=29$ and $p-value = 0.011 <0.05$.The p value is statistically significant at 0.05 level,which indicates that we should reject $H_0$ and the mean of temperature  is not equal to $30$.
- The 95 percent confidence interval is $[26.262,29.471]$,which does not include $30$ and means the mean of temperature  is not equal to $30$.


```{r}
x <-read.csv('C://Users//mali//Documents//temp.csv')
t.test(x$Temp,mu = 30,alternative = 'two.sided')
```

#QUESTION 2

- The standard deviation is $4.297$.
- The standard error of the mean is $4.297/\sqrt{30}=0.785$.

```{r}
# the standard deviation
round(sd(x$Temp),3)
#the standard error of the mean.
round(sd(x$Temp)/sqrt(length(x$Temp)),3)
```


#QUESTION 3

**Independent sample T test with two sided** should be used to do this question.

$$H_0: u_{Jan}=u_{Dec}$$
$$H_0: u_{Jan} \neq u_{Dec}$$

#QUESTION 4


- The $t-stat=-0.381$,$df=118.01$ and $p-value = 0.704 >0.05$.The p value is not  statistically significant at 0.05 level,which indicates that we cann't reject $H_0$ and there is not a difference between the rainfall in January and December.
- The 95 percent confidence interval is $[-6.587,4.460]$,which include $0$ and means there is not a difference between the rainfall in January and December.


```{r}
rain <-read.csv('C://Users//mali//Documents//rain.csv')
t.test(rain$Jan,rain$Dec,alternative = 'two.sided')
```

#QUESTION 5

**Independent sample T test with two sided** should be used to do this question.

$$H_0: u_{May}=u_{July}$$
$$H_0: u_{May} \neq u_{July}$$
- The $t-stat=3.2048$,$df=58.679$ and $p-value = 0.00219 <0.05$.The p value is very  statistically significant at 0.05 level,which indicates that we should reject $H_0$ and the wind spread measurements for May and July is different.
- The 95 percent confidence interval is $[-1.00675,4.35453]$,which include $0$ and means the wind spread measurements for May and July is different.


```{r}
weather <-read.csv('C://Users//mali//Documents//weatherdata.csv')
weather.test.data <- weather[weather$Month %in%c(5,7),]
t.test(Wind~Month,data=weather.test.data)
```

