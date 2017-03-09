---
title: "Untitled"
author: "Your Nmae"
date: "2017-03-09"
output: 
  html_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#5.4(5)

```{r, message=FALSE, warning=FALSE}
library(ISLR)
library(boot)
data("Default")
str(Default)
```

##a

```{r}
set.seed(2017)
glm1 <-  glm(default ~ income + balance, data = Default, 
             family = binomial)
summary(glm1)
```

##b & c 

It seems to average around 2.68% test error rate.

-----

ID|test error rate
-----|-----------
1|0.0286
2|0.0276
3|0.0248
4|0.0262

-----


```{r}
my.lr.model <- function(seeds) {
	set.seed(seeds)
	#Split the sample set into a training set and a validation set
	ind <- sample(x=nrow(Default),size=nrow(Default)/2,replace = F)
	training.data <- Default[ind,]
	validation.data <- Default[-ind,]
	# Fit a multiple logistic regression model 
	# using only the training observations
	glm2 <- glm(default ~ income + balance, data = training.data, family = binomial)
	# Obtain a prediction of default status for 
	# each individual inthe validation set
	pred.prob <- predict.glm(object = glm2,newdata = validation.data,type = 'response')
	pred.class <- ifelse(pred.prob>0.5,'Yes','No')
	#Compute the validation set error
	return(mean(pred.class!=validation.data$default))
}
# run my function
my.lr.model(seeds = 1)
my.lr.model(seeds = 2)
my.lr.model(seeds = 3)
my.lr.model(seeds = 4)
```

##d

2.74% mean test error rate with student dummy variable. Using the validation set approach, it didn't appear adding the student dummy variable leads to a reduction in the test error rate.

-----

ID|test error rate
-----|-----------
1|0.0288
2|0.0286
3|0.0248
4|0.0274

-----

```{r}
my.lr.model.2 <- function(seeds) {
	set.seed(seeds)
	#Split the sample set into a training set and a validation set
	ind <- sample(x=nrow(Default),size=nrow(Default)/2,replace = F)
	training.data <- Default[ind,]
	validation.data <- Default[-ind,]
	# Fit a multiple logistic regression model 
	# using only the training observations
	glm2 <- glm(default ~ income + balance+student, data = training.data, family = binomial)
	# Obtain a prediction of default status for 
	# each individual inthe validation set
	pred.prob <- predict.glm(object = glm2,newdata = validation.data,type = 'response')
	pred.class <- ifelse(pred.prob>0.5,'Yes','No')
	#Compute the validation set error
	return(mean(pred.class!=validation.data$default))
}

my.lr.model.2(seeds = 1)
my.lr.model.2(seeds = 2)
my.lr.model.2(seeds = 3)
my.lr.model.2(seeds = 4)
```


##10-fold cross validation and LOOCV


- The  test error rate without student dummy variable is 2.15% with  10-fold cross validation .
- The  test error rate with student dummy variable is 2.15% with  10-fold cross validation .
- The  test error rate with LOOCV is not  calculated, Because the calculation is too large, I had to comment on this code.

```{r}
set.seed(100)
glm3 <-  glm(default ~ income + balance, 
             data = Default,
             family = binomial)
glm4 <-  glm(default ~ income + balance+student, 
             data = Default, 
             family = binomial)
#LOOCV
#cv.glm(data=Default,glmfit=glm3)$delta[1]
#cv.glm(data=Default,glmfit=glm4)$delta[1]
#10-fold cross validation
cv.glm(data=Default,glmfit=glm3,K=10)$delta[1]
cv.glm(data=Default,glmfit=glm4,K=10)$delta[1]
```

#6.8(1)

##a

Best subset selection has the smallest training RSSbecause the other two methods determine models with a path dependency on which predictors they pick first as they iterate to the k'th model.

##b

Best subset selection may have the smallest test RSS because it considers more models then the other methods. However,the other models might have better luck picking a model that fits the test data better.

##c

- i. True. 
- ii. True. 
- iii. False. 
- iv. False. 
- v. False.

#6.8(8)

##a

```{r}
set.seed(2017)
X <- rnorm(100)
eps <- rnorm(100)
```

##b

$$\beta_0=5$$
$$\beta_1=-7$$
$$\beta_2=-3.5$$
$$\beta_3=-5$$


```{r}
beta0 = 5
beta1 = -7
beta2 = 3.5
beta3 = -5
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps
```

##c

- Use regsubsetsregsubsets  and  mallows Cp, suggest that best model  having polynomial of X of degree 3.
- Use regsubsetsregsubsets  and  BIC suggest that best model  having polynomial of X of degree 3.
- Use regsubsetsregsubsets  and  adjusted $R^2$ suggest that best model  having polynomial of X of degree 6.



```{r}
library(leaps)
mydata <- data.frame(y = Y, x = X)
m1<- regsubsets(y ~ poly(x, 10, raw = T), 
                data = mydata, 
                nvmax = 10)
m1
# summary(m1)
# cp
which.min(summary(m1)$cp)

#bic
which.min(summary(m1)$bic)
# r
which.max(summary(m1)$adjr2)
# plot
par(mfrow=c(1,3))
plot(summary(m1)$cp,type='b',ylab='CP',xlab='Size',main='CP')
plot(summary(m1)$bic,type='b',ylab='BIC',xlab='Size',main='BIC')
plot(summary(m1)$adjr2,type='b',ylab=expression('R^2'),xlab='Size',main=expression('R^2'))
par(mfrow=c(1,1))
#coefficients
coefficients(m1, id = 3)
```


##d

We see that mallows Cp and BIC statistics in both  forward and backward stepwise  pick degree 3 variable models except  with adjusted R2 picked  degree 6 .

```{r}
m2 <- regsubsets(y ~ poly(x, 10, raw = T), 
                 data= mydata, 
                 nvmax = 10, 
                 method = "forward")
m3 <- regsubsets(y ~ poly(x, 10, raw = T), 
                 data = mydata, 
                 nvmax = 10, method = "backward")

#forward
which.min(summary(m2)$cp)
which.min(summary(m2)$bic)
which.max(summary(m2)$adjr2)

#backward
which.min(summary(m3)$cp)
which.min(summary(m3)$bic)
which.max(summary(m3)$adjr2)
```

#Null Behavior of Best Subset Selection

##a

- Use regsubsetsregsubsets  and  mallows Cp, suggest that best model  having polynomial of X of  4 variables .
- Use regsubsetsregsubsets  and  BIC suggest that best model  having polynomial of X of  1 variables.
- Use regsubsetsregsubsets  and  adjusted $R^2$ suggest that best model  having polynomial of X of  9 variables.

```{r}
set.seed(107)
X <-  as.data.frame(matrix(rnorm(4200), ncol = 21))
names(X)[1] <- "y"

#a
library(leaps)
m1<- regsubsets(y ~ ., data = X, nvmax = 20)
m1
#summary(m1)
# cp
which.min(summary(m1)$cp)
#bic
which.min(summary(m1)$bic)
# r
which.max(summary(m1)$adjr2)
# plot
par(mfrow=c(1,3))
plot(summary(m1)$cp,
     type='b',
     ylab='CP',
     xlab='Size',
     main='CP')
plot(summary(m1)$bic,
     type='b',
     ylab='BIC',
     xlab='Size',
     main='BIC')
plot(summary(m1)$adjr2,
     type='b',
     ylab=expression('R^2'),
     xlab='Size',
     main=expression('R^2'))
par(mfrow=c(1,1))
```

##b

10-fold CV run 3 times respectively given  3,3,7 variables

```{r}
 predict.regsubsets <- function (object ,newdata ,id ){
 form<-as.formula (object$call [[2]])
 mat<-model.matrix (form ,newdata )
 coefi <-coef(object ,id=id)
 xvars <- names (coefi )
 mat[,xvars ]%*% coefi
 }

#times=1
k <- 10
set.seed (2017)
folds <- sample (1:k,nrow(X),replace =TRUE)
cv.errors <- matrix (NA ,k,20, dimnames =list(NULL , paste (1:20) ))

for(j in 1:k){
 best.fit <- regsubsets (y ~ .,data=X[folds !=j,],nvmax =20)
 for(i in 1:20) {
        pred=predict(best.fit,X[folds ==j,], id=i)
        cv.errors[j,i]=mean((X$y[folds ==j]-pred)^2)
      }
 }
mean.cv.errors <- apply(cv.errors ,2, mean)
plot(mean.cv.errors ,type='b',xlab='size')
which.min(as.numeric(mean.cv.errors))

# times=2
set.seed (2018)
folds <- sample (1:k,nrow(X),replace =TRUE)
cv.errors <- matrix (NA ,k,20, dimnames =list(NULL , paste (1:20) ))

for(j in 1:k){
 best.fit <- regsubsets (y ~ .,data=X[folds !=j,],nvmax =20)
 for(i in 1:20) {
        pred=predict(best.fit,X[folds ==j,], id=i)
        cv.errors[j,i]=mean((X$y[folds ==j]-pred)^2)
      }
 }
mean.cv.errors <- apply(cv.errors ,2, mean)
plot(mean.cv.errors ,type='b',xlab='size')
which.min(as.numeric(mean.cv.errors))

# times=3
set.seed (2019)
folds <- sample (1:k,nrow(X),replace =TRUE)
cv.errors <- matrix (NA ,k,20, dimnames =list(NULL , paste (1:20) ))

for(j in 1:k){
 best.fit <- regsubsets (y ~ .,data=X[folds !=j,],nvmax =20)
 for(i in 1:20) {
        pred=predict(best.fit,X[folds ==j,], id=i)
        cv.errors[j,i]=mean((X$y[folds ==j]-pred)^2)
      }
 }
mean.cv.errors <- apply(cv.errors ,2, mean)
plot(mean.cv.errors ,type='b',xlab='size')
which.min(as.numeric(mean.cv.errors))
```

##c

- CP : 4
- BIC : 1
- R^2 : 9
- 10-K CV : 3 

I will choose 3 variables.
