---
title: "Assignment 5"
author: "Your Nmae"
date: '2017-05-16'
output:
  word_document:
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# SELECTING FUNCTIONAL FORM

```{r, include=FALSE}
x=c(3.34,1.04,3.17,3.11,4.25,2.04,
    3.90,4.62,4.80,1.29,4.02,2.14,
    1.40,6.82,2.66,3.82,4.88,3.34,
    4.85,4.05,3.86,4.99,3.03,2.96,
    5.60,4.32,2.93,5.37,3.05,3.12,
    3.27,1.96,4.51,3.62,2.93,7.88,
    2.84,3.49,2.55,2.03,4.75,1.97,
    3.26,1.72,4.62,1.34,8.60,4.57,
    3.89,3.26)
y=c(25.77,1.63,7.18,7.47,39.15,10.11,
    30.73,41.61,36.17,1.98,25.09,6.65,
    3.32,57.85,12.83,11.19,44.63,23.59,
    62.48,41.98,31.30,90.31,15.67,21.93,
    25.14,48.83,16.32,38.52,18.03,17.64,
    17.05,9.79,55.12,20.76,10.91,60.62,
    18.86,31.18,13.23,1.46,56.90,5.52,
    18.52,7.62,40.04,2.68,37.05,54.55,
    29.73,27.41)
```

##a

I fit a simple linear regression model with $y=\beta_0 + \beta_1x$.And the regression equation is $$y=9.720x-8.665$$

- But I find that the p value of intercept is 0.07 (Table 1),which is greater than 0.05 and means that I can't reject the null hypothesis of $\beta_0=0$.
- The Multiple R-squared is 0.5837 ad Adjusted R-squared is 0.575,which is very small and means this model is **not adequate**.

```{r, include=FALSE}
lm1 <- lm(y~x)
summary(lm1)
```

```{r, echo=FALSE}
library(knitr)
kable(summary(lm1)$coefficients,
      format = 'pandoc',
      digits = 4,
      caption = 'Table 1 . Coefficients of Linear Regression Model')
```


##b

Ramsey Reset test

- First, estimate a simple model:$y=\beta_0 + beta_1 x$.
- Second, generate fitted or predicted values: $\hat y$.
- Third, include a quadratic power of $\hat y$ in model
and test if the coeficient $\beta_2$ on this additional variable is
significant $y=\beta_0 + beta_1 x + \beta_2 (\hat y)^2$. 
- The p value of $beta_2$ is 0.0016(Table 2),which is statistically significant at 0.05 level and means that I have a
problem with this functional form of $y=\beta_0 + beta_1 x$.


```{r, message=FALSE, warning=FALSE, include=FALSE}
fit2 <-as.vector(lm1$fitted.values)^2 
t2 <- round(summary(lm(y~x+fit2))$coefficients,4)

# do RESET Test use resettest function in lmtest package
library(lmtest)
resettest(lm1,power = 2,type = 'fitted')
```

```{r, echo=FALSE}
library(knitr)
kable(t2,format = 'pandoc',
      digits = 4,
      row.names = T,
      caption = 'Table 2. The Result of Ramsey Reset Test')
```



##c

- y~log(x) : `x` and `intercept` are both statistically significant at 0.05 level and the $R_{adj}=0.6039$.The p value of ramsey reset test is $0.1808>0.05$,which means that the functional form is good.
- log(y)~x : `x` and `intercept` are both statistically significant at 0.05 level and the $R_{adj}=0.6073$.The p value of ramsey reset test is $0 < 0.05$,which means that  the functional form is incorrect.
- log(y)~log(x) : `x` and `intercept` are both statistically significant at 0.05 level and the $R_{adj}=0.779$.The p value of ramsey reset test is $0.0162 <0.05$,which means that the functional form is incorrect.
- Finally ,I think  **y~log(x)** is my best model and the result model is follow in table 3 .




```{r, include=FALSE}
lm2 <- lm(y~log(x))
lm3 <- lm(log(y)~x)
lm4 <- lm(log(y)~log(x))
library(lmtest)
resettest(lm2,power = 2,type = 'fitted')
resettest(lm3,power = 2,type = 'fitted')
resettest(lm4,power = 2,type = 'fitted')
```

```{r, echo=FALSE}
kable(summary(lm2)$coefficients,
      format = 'pandoc',
      digits = 4,
      caption = 'Table 4. The result of My Best Model')
```


# 2 FITTING A REGRESSION LINE

##a

```{r, message=FALSE, warning=FALSE, include=FALSE}
x <- read.csv('C://Users//mali//Documents//meatweight.csv')
t4 <- apply(x[,c('age','weight')],MARGIN = 2,summary)
```

```{r, echo=FALSE}
kable(t4,format = 'pandoc',digits = 4,
      caption = 'Table 4 .Summarise and Describe Data')
```



##b

The regression equation is $$weight=429.81 + 38.48\cdot age$$.

- The intercept is 429.81,which means  when age of animal is zero,slaughter weight per animal in pounds would be  429.81.
- The slope is 38.48,which means that when age of animal increase 1 month,slaughter weight per animal in pounds would increase  38.48.

```{r, include=FALSE}
lm1 <- lm(weight~age,data=x)

```
```{r, echo=FALSE}
kable(summary(lm1)$coefficients,
      format = 'pandoc',
      digits = 4,
      caption = 'Table 5 .The Result of Linear Model ')

with(x,plot(age,weight,col=gray(0.3),
     main='Figure 1 . weight Vs age '))
abline(lm1,col='red',lwd=2)
legend('topleft',legend = 'y~ax+b',col='red',lwd=2)
```

##c

- The intercept is 424,which means when all variable are  zero,slaughter weight per animal in pounds would be  424.
- The coefficients of age is 38.03,which means that when age of animal increase 1 month,slaughter weight per animal in pounds would increase  38.03.
- The coefficients of male is 63.79,which means that when the animal is male,slaughter weight per animal in pounds would increase 63.79.
- The coefficients of shire1 is 21.12,which means that when the animal is shire1,slaughter weight per animal in pounds would increase 21.12.The coefficients of shire2 is 52.15,which means that when the animal is shire2,slaughter weight per animal in pounds would increase 52.15.The coefficients of shire3 is 76.14,which means that when the animal is shire3,slaughter weight per animal in pounds would increase 76.14.

```{r, include=FALSE}
mydf <- x[,c('age','male','breed1','shire1','shire2','shire3','weight')]
lm2 <- lm(weight~.,data=mydf)

```

```{r, echo=FALSE}
kable(summary(lm2)$coefficients,
      format = 'pandoc',
      digits = 4,
      caption = 'Table 6 .The Result of Multiple Linear Model ')
```

##d

The predicted weight at slaughter for a male animal, aged 21 months, of breed1 from shire 1 is 1195.7 and the 95% confidence interval is [1145.0,1246.5].

$$weight = 424.00 + 38.03 * 21 + 63.79 -111.82 + 21.12=1195.7 $$

```{r, include=FALSE}
predict(lm2,
	newdata = data.frame(age=21,male=1,breed1=1,shire1=1,shire2=0,shire3=0),
	interval = "prediction",
	level=0.95)
```


##e

The p value of $beta_{fit2}$ is 0.00317 (Table 7),which is statistically significant at 0.05 level and means that I need a better functional form.But it does not mean your model specification is incorrect.

```{r, include=FALSE}
mydf1 <- cbind(mydf,fit2=lm2$fitted.values^2)
summary(lm(weight~.,data=mydf1))
```

```{r, echo=FALSE}
kable(summary(lm2)$coefficients,
      format = 'pandoc',
      digits = 4,
      caption = 'Table 7 .The Result of Ramsey Reset Test')
```

