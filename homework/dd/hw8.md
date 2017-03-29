---
title: "HW8"
author: "Your Name"
date: "2017年3月29日"
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

$$\hat g=g(x)$$

##b

$$\hat g=ax^2 \cdot g(x)$$

##c
$$\hat g=ax^3 \cdot g(x)$$

#Q6

##b


I performed 10 folds cross validationto choose the optimal number of cuts, and the cv error plot told me the best cut is **8** .

```{r, message=FALSE, warning=FALSE}

library(ISLR)
library(ggplot2)
library(boot)
data('Wage')

#b
cv <- numeric(9)
for (i in 2:10) {
  Wage$age_cut <- cut(Wage$age, i)
  lm1 <- glm(wage~age_cut, data=Wage)
  cv[i-1] = cv.glm(Wage, lm1, K=10)$delta[2]
}

plot(2:10, cv, xlab='Cuts', ylab='CV error', type='b',main='10 folds CV')

lm2 <- glm(wage~cut(age, 8), data=Wage)

ggplot(data=Wage,aes(x=age,y=wage))+
   geom_point(col=I(gray(0.5)))+
   geom_smooth(method = 'lm',formula = y~cut(x,8),se=F,col=I('red'))+
   labs(title='Step Function Model with 8 Cuts')


```


#Q9

##a

```{r, message=FALSE, warning=FALSE}
library(MASS)
library(ggplot2)
data('Boston')

#a
lm1 <-  lm(nox ~ poly(dis, 3), data = Boston)
summary(lm1)

ggplot(data=Boston,aes(x=dis,y=nox))+
   geom_point(col=I(gray(0.5)))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 3),se = F)+
   labs(title='Cubic Polynomial Regression')
```

##b

RSS would decrease with the increase of degree in Polynomial Regression.

```{r}
ggplot(data=Boston,aes(x=dis,y=nox))+
   geom_point(col=I(gray(0.5)))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 1),se = F,col=I(colors()[10]) )+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 2),se = F,col=I(colors()[20]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 3),se = F,col=I(colors()[30]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 4),se = F,col=I(colors()[40]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 5),se = F,col=I(colors()[50]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 6),se = F,col=I(colors()[60]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 7),se = F,col=I(colors()[70]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 8),se = F,col=I(colors()[80]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 9),se = F,col=I(colors()[90]))+
   geom_smooth(method = 'lm',formula = y ~ poly(x, 10),se = F,col=I(colors()[100]))+
   labs(title=' Polynomial Regression with degree from 1 to 10')


rss1 <- numeric(10)
for (i in 1:10) {
    lm.fit <- lm(nox ~ poly(dis, i), data = Boston)
    rss1[i] <- sum(lm.fit$residuals^2)
}
plot(rss1,
     type='b',
     main='RSS of polynomial degrees from 1 to 10',
     xlab='degree',
     ylab='RSS')

```


##c

I use a 10-fold cross validation to find the best polynomial degree and the best degree is **3**.

```{r}
#c
library(boot)
cvs <- numeric(10)
for (i in 1:10) {
    glm1 <-  glm(nox ~ poly(dis, i), data = Boston)
    cvs[i] <- cv.glm(Boston, glm1, K = 10)$delta[2]
}

plot(cvs,
     xlab = 'Degree', 
     ylab = 'CV error', 
     type = 'b',
     main='CV error Vs Degree')

which.min(cvs)

```

##d

I split this range in roughly equal 4 intervals and establish knots at [4,7,11]

```{r}
library(splines)
sp1 <-  lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11) ), data = Boston)
summary(sp1)

ggplot(data=Boston,aes(x=dis,y=nox))+
   geom_point(col=I(gray(0.5)))+
   geom_smooth(method = 'lm',formula = y ~ bs(x, df = 4, knots = c(4, 7, 11) ),se = F)+
   labs(title='Regression Spline With 4 Degree')
```

##e

RSS would decrease with the increase of degree in spline.But I would choose 5 degree.Because when degree is greater than 5, the decline trend of CV error is much lower.

```{r}
cvs <- numeric(20)
for (i in 3:20) {
    sp2 <- lm(nox ~ bs(dis, df = i), data = Boston)
    cvs[i] = sum(sp2$residuals^2)
}
plot(3:20,
     cvs[-c(1:2)],
     type='b',
     xlab='Degree',
     ylab = 'CV error',
     main='CV error Vs Degree')
```

#Q10

##a

The best variables are  "PrivateYes",  "Apps"  , "Accept" , "Top10perc"  , "F.Undergrad","P.Undergrad" ,"Room.Board" , "Books" ,  "PhD"  ,  "Terminal"  ,  "S.F.Ratio","perc.alumni" ,"Expend"  and  "Grad.Rate"  

```{r, message=FALSE, warning=FALSE}
data('College')
library(mgcv)
library(leaps)
set.seed(2441)
ind <- sample(x = 1:2,size = nrow(College),replace = T,prob = c(0.7,0.3))
train_data <- College[ind==1,]
test_data <- College[ind==2,]

r1 <- regsubsets(Outstate ~ ., data = train_data, nvmax = 17, method = 'forward')
r1_summ <-summary(r1)

par(mfrow = c(1, 3))
plot(r1_summ$cp, 
     xlab = 'Number of Variables', 
     ylab = 'Cp', 
     type = 'b',
     main=paste0('best model with id = ', which.min(r1_summ$cp)) )
plot(r1_summ$bic, 
     xlab = 'Number of Variables', 
     ylab = 'BIC',
     type = 'b',
     main=paste0('best model with id = ', which.min(r1_summ$bic)))
plot(r1_summ$adjr2, 
     xlab = 'Number of Variables', 
     ylab = 'Adjusted R2', 
     type = 'b',
     main=paste0('best model with id = ', which.max(r1_summ$adjr2)))
par(mfrow = c(1, 1))

names(coef(r1, id = 14))
```

##b

```{r}
gam1 <- gam(Outstate ~Private+
              s(Apps,k=5)+s(Accept,k=5)+s(Top10perc,k=5)+
              s(F.Undergrad,k=5)+s(P.Undergrad,k=5)+s(Room.Board,k=5)+
              s(Books,k=5)+s(PhD,k=5)+s(Terminal,k=5)+s(S.F.Ratio,k=5)+
              s(perc.alumni,k=5)+s(Expend,k=5)+s(Grad.Rate,k=5),
            data=train_data)
par(mfrow=c(3,3))
plot(gam1)
par(mfrow=c(1,1))
```

##c

The RMSE is **1907.477**.


```{r, message=FALSE, warning=FALSE}
gam_pred <- predict(gam1,test_data)
err <- gam_pred-test_data$Outstate
library(car)

rmse <- sqrt(mean(err^2))
rmse

par(mfrow=c(1,3))
plot(err,main='residuals plot in test data')
abline(h=0,col='blue',lty=2,lwd=2)
hist(err,freq = F)
lines(density(err),col='orange')
qqPlot(err)
par(mfrow=c(2,2))
```

