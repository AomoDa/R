---
title: "Untitled"
author: "Your Nmae"
date: '2017-02-07'
output:
  word_document:
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Q4

###a

Suppose that the true relationship between X and Y is linear ,then $RSS_{cubic}$ in train data set would be lower than $RSS_{linear}$.

###b

Suppose that the true relationship between X and Y is linear ,then  $RSS_{linear}$ in test data set would be lower than $RSS_{cubic}$.

###c

Suppose that the true relationship between X and Y is not linear,then $RSS_{cubic}$ in train data set would be lower than $RSS_{linear}$.


###d

We don’t know how far it is from linear.So there is not
enough information to tell which one would be lower than another in test data set .

##Q9

###c


-----

variable|Estimate| Std. Error| t value| P value
--------|--------|-----------|--------|--------
(Intercept) | -17.218435 |  4.644294|  -3.707 | 0.00024
cylinders  |   -0.493376 |  0.323282 | -1.526 | 0.12780
displacement |  0.019896  | 0.007515 |  2.647 | 0.00844
horsepower    |-0.016951  | 0.013787 | -1.230 | 0.21963
weight      |  -0.006474  | 0.000652 | -9.929  |0
acceleration |  0.080576  | 0.098845 |  0.815 | 0.41548
year     |      0.750773 |  0.050973 | 14.729 | 0
origin   |      1.426141 |  0.278136 |  5.127 |0

-----
```{r}
library(ISLR)
data("Auto")
lm1 = lm(mpg~.-name, data=Auto)
summary(lm1)
```

###e


- lm2: add interaction `cylinders * horsepower`.The p value of this interaction is very statistically significant at 0.05 level.
- lm3: add interaction `displacement * acceleration,`.The p value of this interaction is very statistically significant at 0.05 level.
- lm4: add interaction `cylinders * weight`.The p value of this interaction is very statistically significant at 0.05 level.
- lm5: add interaction `horsepower * acceleration`.The p value of this interaction is very statistically significant at 0.05 level.

```{r}
lm2 <- lm(mpg~.-name+cylinders*horsepower, data=Auto)
summary(lm2)
lm3 <- lm(mpg~.-name+displacement*acceleration, data=Auto)
summary(lm3)
lm4 <- lm(mpg~.-name+cylinders*weight, data=Auto)
summary(lm4)
lm5 <- lm(mpg~.-name+horsepower*acceleration, data=Auto)
summary(lm5)
```



###f

- lm6:`log(cylinders)`,`log(horsepower)` and `log(acceleration)`.Finally all predictors are very statistically significant at 0.05 level.
- lm7:`sqrt(cylinders)`,`sqrt(horsepower)` and `sqrt(acceleration)`.Finally all predictors expect `sqrt(horsepower)` and `Intercept` are very statistically significant at 0.05 level
- lm8:`I(cylinders^2)`,`I(horsepower^2)` and `I(acceleration^2)`.Finally all predictors expect `I(cylinders^2)` and `displacement` are very statistically significant at 0.05 level.
- lm9:`log(cylinders)`,`sqrt(horsepower)` and `log(acceleration)`.Finally all predictors expect `log(acceleration` and `Intercept` are very statistically significant at 0.05 level.



```{r}
lm6 <- lm(mpg ~ log(cylinders) + displacement + log(horsepower) + weight + log(acceleration) + year + origin ,data=Auto)
summary(lm6)
lm7 <- lm(mpg ~ sqrt(cylinders) + displacement + sqrt(horsepower) + weight + sqrt(acceleration) + year + origin ,data=Auto)
summary(lm7)
lm8 <- lm(mpg ~ I(cylinders^2) + displacement + I(horsepower^2) + weight + I(acceleration^2) + year + origin ,data=Auto)
summary(lm8)
lm9 <- lm(mpg ~ log(cylinders) + displacement + sqrt(horsepower) + weight + log(acceleration) + year + origin ,data=Auto)
summary(lm9)

```


##Q14


###a

The function of $y$ with $x_1$ and $x_2$ is $$y=2+2x_1+0.3x_2+\epsilon$$,where $\beta_0=2$,$\beta_1=2$ and $\beta_2=0.3$


```{r}
set.seed (1)
x1 <- runif (100)
x2 <- 0.5* x1+rnorm (100) /10
y <- 2+2* x1 +0.3* x2+rnorm (100)
```

###b


The correlation between $x_1$ and $x_2$ is 0.84.

```{r}
cor(x1,x2)
plot(x1,x2)
```

###c

- $\hat \beta_0=2.1305$ and the p value is 0,which means I should reject the null hypothesis $H_0:\beta_0=0$ at 0.05 level.
- $\hat \beta_1=1.4396$ and the p value is 0.0487,which means I should reject the null hypothesis $H_0:\beta_1=0$ at 0.05 level .
- $\hat \beta_0=1.0097$ and the p value is 0.3754,which means I can't reject the null hypothesis $H_0:\beta_2=0$ at 0.05 level .

-----

variable|Estimate| Std. Error| t value| P value
--------|--------|-----------|--------|--------
(Intercept) |  2.1305  |   0.2319 |  9.188| 0
x1       |     1.4396  |   0.7212  | 1.996  | 0.0487  
x2       |     1.0097  |   1.1337 |  0.891  | 0.3754   

-----

```{r}
lm1 <- lm(y~x1+x2)
summary(lm1)
```

###d

- $\hat \beta_0=2.1124$ and the p value is 0,which means I should reject the null hypothesis $H_0:\beta_0=0$ at 0.05 level.
- $\hat \beta_1=1.9759$ and the p value is 0,which means I should reject the null hypothesis $H_0:\beta_1=0$ at 0.05 level .


-----

variable|Estimate| Std. Error| t value| P value
--------|--------|-----------|--------|--------
(Intercept) |  2.1124  |   0.2307  | 9.155 |0
x1       |     1.9759   |  0.3963  | 4.986| 0 

-----

```{r}
lm2 <- lm(y~x1)
summary(lm2)
```

###e

- $\hat \beta_0=2.3899$ and the p value is 0,which means I should reject the null hypothesis $H_0:\beta_0=0$ at 0.05 level.
- $\hat \beta_1=2.8996$ and the p value is 0,which means I should reject the null hypothesis $H_0:\beta_1=0$ at 0.05 level .

-----

variable|Estimate| Std. Error| t value| P value
--------|--------|-----------|--------|--------
(Intercept)|   2.3899 |    0.1949  | 12.26 | 0
x2       |     2.8996 |    0.6330  |  4.58 | 0

-----

```{r}
lm3 <- lm(y~x2)
summary(lm3)
```

###f

The results obtained in (c)–(e) don't contradict each other.Because the correlation coefficients of $x_1$ and $x_2$ is 0.84,which means that there are some linear relation between $x_1$ and $x_2$.

###g

- The point in model ` y ~ x1 + x2` are  a outlier point and a high-leverage point .
- The point in model ` y ~ x1 ` is  a outlier point.
- The point in model ` y ~ x2 ` is   a high-leverage point.


```{r, message=FALSE, warning=FALSE}
x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)
library(car)
# x1 and x2
lm4 <- lm(y~x1+x2)
summary(lm4)
influencePlot(lm4)
```


```{r, message=FALSE, warning=FALSE}
# only x1
lm5 <- lm(y~x1)
summary(lm5)
influencePlot(lm5)
```

```{r, message=FALSE, warning=FALSE}
# only x2
lm6 <- lm(y~x2)
summary(lm6)
influencePlot(lm6)
```


