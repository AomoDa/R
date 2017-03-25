---
title: "HW7"
author: "Your Name"
date: '2017-03-25'
output:
  word_document:
    toc: yes
  html_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


#Q2

##a


**iii is correct**

The lasso, relative to least squares:

Less flexible and hence will give improved prediction accuracy when its increase in bias is less than its decrease in variance.

##b


**iii is correct**

The ridge regression, relative to least squares:

Less flexible and hence will give improved prediction accuracy when its increase in bias is less than its decrease in variance.

#Q4

##a

**iii is correct:  Steadily increase**

As we increase $\lambda$ from 0 to sufficiently large, all $\beta$ decrease from their least square estimate values to **zero**. Training error for OLS $\beta$  is the minimum and it steadily increases as $\beta$ are reduced to **zero**.

##b

**ii is correct :  Decrease initially, and then eventually start increasing in a U shape.**

- When $\lambda=0$, all $\beta$  are the sames as  the OLS estimate values and the   test RSS would be very high. 
- As we increase $\lambda$ to someone appropriate value,some $\beta$ are start reducing to zero and some of the overfitting is reduced,which means that the test RSS would be the minimum.
- When we increase $\lambda$ to  sufficiently large all  ,$\beta$ are approach zero and  the test RSS would be the maximum.

##c

**iv is correct:  Steadily decreases**

- When $\lambda=0$, all $\beta$  are the sames as  the OLS estimate values and  actual estimates heavily depend on the training data and hence variance is high.
- As we increase $\lambda$ to someone appropriate value ,some $\beta$ are start reducing to zero and some of the overfitting is reduced and model becomes simpler. The variance would be reduced.
- When we increase $\lambda$ to  sufficiently large all  ,$\beta$ are approach zero and has no variance.

#Q8

I set $\beta_0=5$, $\beta_1=-7$ ,$\beta_2=-3.5$ and  $\beta_3=-5$.

```{r}
set.seed(100)
X <- rnorm(100)
eps <- rnorm(100)
beta0 = 5
beta1 = -7
beta2 = 3.5
beta3 = -5
Y <- beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps
mydata <- data.frame(y = Y, x = X)  
```
```{r}

```


##e

Use 10 folds cross-validation to select the optimal value of $\lambda=0.1841$.And the coefficient estimates is $\beta_0=5.10$,$\beta_1=-7.17$,$\beta_2=3.40$ and $\beta_3=-4.91$. And the equation is 

$$Y= 5.10+ -7.17X+3.40X^2-4.91X^3$$.

the actual equation is $Y= 5+ -7X+3.5X^2-5X^3$. The lasso estimated equation is very close to the actual equation I think the model is very good.

```{r, message=FALSE, warning=FALSE}
library(glmnet)
q8_e <- model.matrix(y ~ poly(x, 10, raw = T), data = mydata)[, -1]
lasso1 <- cv.glmnet(q8_e, Y, alpha = 1)
# bets lambda
lasso1$lambda.min
#Create plots of the cross-validation error as a function of lambda
plot(lasso1)
#Report the resulting coefficient estimates
coef.glmnet(lasso1,s=lasso1$lambda.min)
```


##f

I set $\beta_0=5$ and $\beta_7=10$ .The actual equation is $Y=5+10X^7$.

```{r, message=FALSE, warning=FALSE}
library(leaps)
beta0 = 5
beta7 = 10
Y = beta0 + beta7 * X^7 + eps
mydata_f <- data.frame(y = Y, x = X)
```

###perform best subset selection

best subset selection:

- cp and BIC choose $id=2$ model.And the  estimated equation is $Y=4.99-0.1X^3+10X^7$.
- $R^2_{adj}$choose $id=7$ model.And the  estimated equation is $Y=4.89+1.28X^2-0.1X^3-1.88X^4+0.83X^6+10X^7-0.14X^8+0.01X^{10}$.

the actual equation is $Y= 5+ 10X^7$. The  best subset selection estimated equation with cp  and BIC is very close to the actual equation I think the model is very good.

```{r}
#perform best subset selection
m1 <- regsubsets(y ~ poly(x, 10, raw = T), 
                 data = mydata_f,
                 nvmax = 10)
par(mfrow=c(1,3))
plot(summary(m1)$cp,
     type='b',
     ylab='cp',
     main=paste0('CP choose model \n id=',which.min(summary(m1)$cp)),
     col='red')
plot(summary(m1)$bic,
     type='b',
     ylab='bic',
     main=paste0('BIC choose model \n id=',which.min(summary(m1)$bic)),
     col='blue')
plot(summary(m1)$adjr2,
     type='b',
     ylab='adjr2',
     main=paste0('R^2 choose model\n id=',which.max(summary(m1)$adjr2)),
     col='orange')
par(mfrow=c(1,1))
#the resulting coefficient estimates
coefficients(m1, id = 2)
coefficients(m1, id = 7)
```

###perform the lasso

Use 10 folds cross-validation to select the optimal value of $\lambda=34.51$.And the coefficient estimates is $\beta_0=8.26$ and $\beta_7=9.68$ And the equation is $Y=8.26+9.68X^7$.the actual equation is $Y= 5+ 10X^7$. The lasso estimated equation is  close to the actual equation I think the model is OK.

```{r}
q8_f <- model.matrix(y ~ poly(x, 10, raw = T), data = mydata_f)[, -1]
lasso_f <- cv.glmnet(q8_f, Y, alpha = 1)
plot(lasso_f)
# the resulting coefficient estimates
coef.glmnet(lasso_f,s=lasso_f$lambda.min)
```



#MNIST and Lasso

```{r}
load("C://Users//AomoDa//Documents//mnist68.RData")
images_df <- mnist68
myv <-  rep(NA,784)
for (j in 1:784){myv[j] <- var(images_df[,j])}
myfeatures <-  (1:784)[myv > quantile(myv, .9)]
mydf <-  images_df[,c(myfeatures,785)]
mydf$labels <- as.numeric(mydf$label==8)
```

##a

I use a linear model to predict the labels.

- The minimum of prediction is 0.34600 , maximum is 1.483 and  mean  is 0.5.
- I think when prediction is less than 0.5 I would get the negative class ,when prediction is greater  than 0.5 I would get the positive class .
- The accuracy rate in train model is 0.9606 and  the kappa in train dataset is 0.9211.


```{r, message=FALSE, warning=FALSE}
x <- model.matrix(labels~.,data=mydf)[,-1]
y <- as.numeric(mydf$labels)
m1 <- lm(y~x)
# fitted values
lm1.pred <- as.numeric(m1$fitted.values)
# summary
summary(lm1.pred )
hist(lm1.pred)
# accuracy rate
library(caret)
cf_table <- table(mydf$labels,ifelse(lm1.pred>0.5,1,0))
confusionMatrix(cf_table)
```

##b

Consider the coefficient plot in left-hand panel When $\lambda=0$, then the lasso simply gives the least squares fit, and when $\lambda$ becomes sufficiently large, the lasso gives the null model in which all coefficient estimates equal zero.The right-hand panel of coefficient plot displays the same lasso coefficient estimates as the left-hand panel, but instead of displaying $\lambda$ on the x-axis now display $L^1$ norm.The latter quantity ranges from 1 to 0.

```{r}
lass01 <- glmnet(x,y,alpha = 1)

par(mfrow=c(1,2))
plot(lass01)
plot(lass01,xvar = 'norm')
par(mfrow=c(1,1))
```

##c


- When $\lambda=0.1288$,I get  the last five features that remain in the model,which are `V430`,`V656`,`V657`,`V658`and `V659`.
- The correlation matrix of the last five features told me that there maybe be not independent in `V656`,`V657`,`V658`and `V659` ,because of the correlation coefficient of the four features is large.



```{r, message=FALSE, warning=FALSE}
library(psych)
for (i in 1:length(lass01$lambda)) {
a <- sum(round(as.numeric(coef.glmnet(lass01,s=lass01$lambda[i]))[-1],4)!=0)
if(a==5){
	print(lass01$lambda[i]) 
	break}
}
round(coef.glmnet(lass01,s=0.1288432),4)
cor(mydf[,c('V430','V656','V657','V658','V659')])
pairs.panels(mydf[,c('V430','V656','V657','V658','V659')])
library(psych)
```

