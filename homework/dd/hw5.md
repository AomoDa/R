---
title: "hw5"
author: "Your Name"
date: "2017-02-21"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



#Q1 FeedFoward

##a

##b

The expression for the output value of the first hidden unit as a function of the values of the
input features is  $$h_1=f(x1,x2)=1.27+0.26x_1-0.34x_2$$

##c

The expression for the value at the output node as a function of the values at the hidden units is

$$z=g(h_1,h_2)$$


#Q2 Scaling


```{r, message=FALSE, warning=FALSE}
library(nnet)
set.seed(1234)
Advertising <- read.csv("C://Users//aomoda//Documents//Advertising.csv")
```


##a

- For linear model,the coefficients of `intercept` is **2.94**  and the weight of `bias` in nnet is **2.94**,which appear to be the same.
- For linear model,the coefficients of `TV` is **0.05**  and the weight of `TV` in nnet is **0.05**,which appear to be the same.
- For linear model,the coefficients of `Radio` is **0.19**  and the weight of `Radio` in nnet is **0.19**,which appear to be the same.
- For linear model,the coefficients of `Newspaper` is **0.00**  and the weight of `Newspaper` in nnet is **0.00**,which appear to be the same.

```{r}
set.seed(1)
lm_fit <-lm(Sales ~ TV + Radio + Newspaper, data=Advertising)
summary(lm_fit)
linear_fit <- nnet(Sales ~ TV + Radio + Newspaper, 
                   size = 0, 
                   data=Advertising, 
                   skip = T, 
                   trace = F,
                   maxit = 1000,
                   linout=TRUE)
summary(linear_fit)
```

##b

The final sum of squared errors (SSE) once the `nnet()` algorithm has converged is **556.8253**.

```{r}
# fina SSE
sum((linear_fit$fitted.values - Advertising$Sales)^2)
```

##c

The SSE once the method has converged is **30.4687**. compared to before, the magnitude of SSE decline is very large.


```{r}
set.seed(2)
linear_fit_3 <- nnet(Sales ~ TV + Radio + Newspaper, 
                     size = 3, 
                     data=Advertising, 
                     skip = T, 
                     trace = F,
                     maxit = 1000,
                     linout=TRUE)
summary(linear_fit_3)
# fina SSE
sum((linear_fit_3$fitted.values - Advertising$Sales)^2)
```

## d & e 

- The SSE of  nnet model  with one unit is **556.8253**.
- The SSE of  nnet model  with three units is **40.15411**.

```{r}
set.seed(3)
# Advert2
Advert2 <- as.data.frame(cbind(scale(Advertising[,-5]),Advertising[,5]))
names(Advert2) <- names(Advertising)
head(Advert2)

# linear
linear_scale_fit <- nnet(Sales ~ TV + Radio + Newspaper, size = 0, data=Advert2, skip = T, trace = F,maxit = 1000,linout=TRUE)
summary(linear_scale_fit)
#sse
sum((linear_scale_fit$fitted.values - Advert2$Sales)^2)

# 3 h
linear_scale_fit_3 <- nnet(Sales ~ TV + Radio + Newspaper, size = 3, data=Advert2, skip = T, trace = F,maxit = 1000,linout=TRUE)
summary(linear_scale_fit_3)
#sse
sum((linear_scale_fit_3$fitted.values - Advert2$Sales)^2)
```


#Q3 MNIST Revisited

```{r, message=FALSE, warning=FALSE}
library(ROCR)
library(nnet)
load('C://Users//aomoda//Documents//mnist_data.RData')

set.seed(2017)
ind <- sample(x = 2,
              size = nrow(images_df),
              replace = T,
              prob = c(0.8,0.2))
images_train <- images_df[ind==1,]
images_test <- images_df[ind==2,]

```


##a

2 features:

-AUC score with logistic regression model in train data set is **0.8594**.
-AUC score with logistic regression model in test data set is **0.8388**.


```{r}
#a
set.seed(4)
# Pick two features that have some non-zero variability
# X570 and X268
glm0 <- glm(labels~X570+X268,data=images_train,family=binomial)
summary(glm0)
# Evaluate the model with the AUC score.
# train
predc_train <- prediction(predictions = as.vector(glm0$fitted.values),labels = images_train$labels)
performance(prediction.obj = predc_train,measure = 'auc')@y.values[[1]]
# test
pred_test <- predict(object = glm0,newdata = images_test,type = 'response')
predc_test <- prediction(predictions =as.vector(pred_test)  ,labels = images_test$labels)
performance(prediction.obj = predc_test,measure = 'auc')@y.values[[1]]
```

##b

2 features:


-AUC score with neural net with one unit in the hidden layer in train data set is **0.8645**.
-AUC score with neural net with one unit in the hidden layer in test data set is **0.8451**.


```{r}
#b
set.seed(5)
# Create a neural net with one unit in the hidden layer
nnet1 <- nnet(labels~X570+X268, size = 1, data=images_train, skip = T, trace = F,maxit = 1000)
summary(nnet1)
# Evaluate the model with the AUC score.
# train
predc_train <- prediction(predictions = as.vector(nnet1$fitted.values),labels = images_train$labels)
performance(prediction.obj = predc_train,measure = 'auc')@y.values[[1]]
# test
pred_test <- predict(object = nnet1,newdata = images_test,type = 'raw')
predc_test <- prediction(predictions =as.vector(pred_test)  ,labels = images_test$labels)
performance(prediction.obj = predc_test,measure = 'auc')@y.values[[1]]
```

##c

2 features:

-AUC score with neural net with three units in the hidden layer in train data set is **0.8742**.
-AUC score with neural net with three units in the hidden layer in test data set is **0.8707**.

```{r}
#c
set.seed(6)
# Create a neural net with more units in the hidden layer
nnet3 <- nnet(labels~X570+X268, size = 3, data=images_train, skip = T, trace = F,maxit = 1000)
summary(nnet3)
# Evaluate the model with the AUC score.
# train
predc_train <- prediction(predictions = as.vector(nnet3$fitted.values),labels = images_train$labels)
performance(prediction.obj = predc_train,measure = 'auc')@y.values[[1]]
# test
pred_test <- predict(object = nnet3,newdata = images_test,type = 'raw')
predc_test <- prediction(predictions =as.vector(pred_test)  ,labels = images_test$labels)
performance(prediction.obj = predc_test,measure = 'auc')@y.values[[1]]
```

##d

8 features:

-AUC score with neural net with three units in the hidden layer in train data set is **0.5124**.
-AUC score with neural net with three units in the hidden layer in test data set is **0.5146**.


```{r}
set.seed(7)
# Create a neural net with more units in the hidden layer
nnet10 <- nnet(labels~X289+X259+X441+X133+X382+X631+X570+X268, size = 5, data=images_train, skip = T, trace = F,maxit = 1000)
summary(nnet10)
# Evaluate the model with the AUC score.
# train
predc_train <- prediction(predictions = as.vector(nnet10$fitted.values),labels = images_train$labels)
performance(prediction.obj = predc_train,measure = 'auc')@y.values[[1]]
# test
pred_test <- predict(object = nnet10,newdata = images_test,type = 'raw')
predc_test <- prediction(predictions =as.vector(pred_test)  ,labels = images_test$labels)
performance(prediction.obj = predc_test,measure = 'auc')@y.values[[1]]
```

#Q4 Overfitting



```{r}
set.seed(20305)
q4 <- as.data.frame(matrix(data = rnorm(n = 11*100),nrow = 100,ncol = 11))
names(q4)[11] <- 'z'
```

##a

The sum of squares of the residuals is **103.0037**.

```{r}
#a
lm0 <- lm(z~.,data=q4)
summary(lm0)
# sum of squares of the residuals
sum(lm0$residuals^2)

```

##b

The sum of squares of the residuals is **51.1312**.

```{r}
#b
set.seed(8)
nnet1<- nnet(z~.,data=q4, size = 2,  skip = T, trace = F,maxit = 2000,linout=TRUE,decay=0.01)
summary(nnet1)
#sse
sum(nnet1$residuals^2)
```

##c

- The sum of squares of the residuals in nnet with 5 hidden units is **51.1312**.
- The sum of squares of the residuals in nnet with 10 hidden units is **0.02728**.

```{r}
#c
# 5 hidden units
nnet5<- nnet(z~.,data=q4, size = 5,  skip = T, trace = F,maxit = 2000,linout=TRUE,decay=0.01)
summary(nnet5)
#sse
sum(nnet5$residuals^2)

# 10 hidden units
nnet10<- nnet(z~.,data=q4, size = 10,  skip = T, trace = F,maxit = 2000,linout=TRUE,decay=0.01)
summary(nnet10)
#sse
sum(nnet10$residuals^2)
```

