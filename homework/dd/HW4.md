---
title: "Untitled"
author: "Your Name"
date: '2017-02-12'
output:
  word_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##Q6

###a

$$P(Y=A)=\frac{exp(0.05 X_1+X_2-6)}{1+exp(0.05  X_1+X_2-6)}$$

when $X_1=40$ and $X_2=3.5$,then $$P(Y=A)=\frac{exp(0.05 \times 40+3.5-6) }{1+exp(0.05 \times 40+3.5-6)}=0.3775$$
The probability that a student who studies for 40 h and
has an undergrad GPA of 3.5 gets an A in the class is **37.75%**.

###b

$$Log \left( \frac{P(Y=A)}{1-P(Y=A)} \right)=-6+0.05X_1+X_2$$

when $X_2=3.5$ and we want to study to
have a 50% chance of getting an A in the class,then 

$$Log \left( \frac{0.5}{1-0.5}\right)=-6+0.5 \times X_1 +3.5 $$.
Finall I get $$X_1=50$$, which means the student in part (a) need **50 hours** to study to have a 50% chance of getting an A in the class.


##Q10

###a

```{r, message=FALSE, warning=FALSE}
library(ISLR)
library(psych)
data("Weekly")
# summary
summary(Weekly)
# cor
cor(Weekly[, -9])
#pairs
pairs.panels(Weekly)
```


###b


`Lag2` appears to be statistically significant at 0.05 level,which means that there is a clear evidence of a real association between `Lag2` and `Direction`.The  coefficient for `Lag2` is 0.05844 ,which is psotive and suggests that if the market had a positive return last week, then it is greater  likely to go up this week.

-----

Predictors|Estimate| Std. Error|z value| P value
----------|--------|-----------|-------|--------
(Intercept)|  0.26686  |  0.08593 |  3.106 | **0.0019**
Lag1  |      -0.04127 |   0.02641 | -1.563|   0.1181   
Lag2  |       0.05844  |  0.02686 |  2.175 |  **0.0296**
Lag3  |      -0.01606  |  0.02666 | -0.602 |  0.5469   
Lag4   |     -0.02779  |  0.02646|  -1.050 |  0.2937   
Lag5    |    -0.01447   | 0.02638|  -0.549 |  0.5833   
Volume |     -0.02274  |  0.03690|  -0.616 |  0.5377   

-----

```{r}
glm1 <-  glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
           data = Weekly,
           family = binomial)
summary(glm1)
```

###c

The diagonal elements of the confusion matrix indicate correct predictions, while the off-diagonals represent incorrect predictions. Hence our model correctly predicted that the our data in 1089 weeks would go up on 557 weeks and that it would go down on 54 weeks, for a total of $557 + 54 = 611$ correct predictions.In this case,  the accuracy rate of my  logistic regression is $611/1089=56.1\%$.

At first glance, it appears that the logistic regression model is working a little better than random guessing. However, this result is misleading because we trained and tested the model on the same set of 1089 observations.
In other words, $100\%− 56.1\% = 43.9\%$ is the training error rate. As we have seen previously, the training error rate is often overly optimistic—it tends to underestimate the test error rate. In order to better assess the accuracy
of the logistic regression model in this setting, we can fit the model using part of the data, and then examine how well it predicts the held out data.



-----

pred| Down | Up
-----|------|---
Down |  54 | 48
Up  |  430| 557

-----

```{r}
glm.prob <- predict(glm1, newdata = Weekly,type = "response")
glm.pred <- ifelse(glm.prob>0.5,'Up','Down')
table(glm.pred,Weekly$Direction)
```



###d

The accuracy rate of my  logistic regression  in test data set is $(9+56)/104=62.5\%$.

-----

pred| Down | Up
-----|------|---
Down |  9 | 5
Up  |  34| 56

-----

```{r}
# train and test data
train.data <- Weekly[Weekly$Year<=2008,]
test.data <- Weekly[Weekly$Year>2008,]
# fit model
glm2 <-  glm(Direction ~ Lag2,data = train.data,family = binomial)
summary(glm2)
# predict
glm2.prob <- predict(glm2, type = "response",newdata =test.data )
glm2.pred <- ifelse(glm2.prob>0.5,'Up','Down')
# confusion matrix
table(glm2.pred,test.data$Direction)
```

##Q13

- Train data set has 361 samples,about 70% of all samples.
- Test data set has 145 samples ,about 30% of all samples.

```{r, message=FALSE, warning=FALSE}
library(MASS)
library(ROCR)
data("Boston")
str(Boston)
crim.med <- median(Boston$crim)
#crime rate above or below the median
Boston$crim.result <- ifelse(Boston$crim>crim.med,1,0)

# train and test data set
set.seed(13)
ind <- sample(x = 2,size =nrow(Boston),replace = T,prob = c(0.7,0.3) )
Boston.train <- Boston[ind==1,-1]
Boston.test <- Boston[ind==2,-1]
```

### logistic regression


-----

Models|algorithm|Predictors| test error rate 
------|-----------|---------|------------------
glm3|logistic regression|all predictors|12.41%
glm4|logistic regression with backward stepwise Algorithm|zn,indus,indus,nox,dis,rad,tax,ptratio,black,lstat,medv|11.03%


-----



```{r, message=FALSE, warning=FALSE}
# logistic regression
glm3 <- glm(crim.result ~ . , data = Boston.train, family = binomial)
summary(glm3)
# test error rate
glm3.prob <- predict(glm3,newdata = Boston.test,type = 'response')
glm3.pred <- ifelse(glm3.prob>0.5,1,0)
mean(glm3.pred!=Boston.test$crim.result)

# Stepwise Algorithm
glm4 <- stepAIC(glm3,direction='backward')
summary(glm4)
# LRT
anova(glm3,glm4,test = 'LRT')
# test error rate
glm4.prob <- predict(glm4,newdata = Boston.test,type = 'response')
glm4.pred <- ifelse(glm4.prob>0.5,1,0)
mean(glm4.pred!=Boston.test$crim.result)
```

###Linear Discriminant Analysis

The test error rate of test data set is 9.65%,which is less than logistic regression.

```{r}

lda1 <- lda(crim.result ~ . , data = Boston.train)
# plot lda
plot(lda1)
# pred
lda1.prob <- predict(lda1,newdata = Boston.test)
# test error rate
table(lda1.prob$class,Boston.test$crim.result)
mean(lda1.prob$class!=Boston.test$crim.result)
```


###k-Nearest Neighbour Classification

-----

Models|algorithm|K| test error rate 
------|-----------|---------|------------------
knn1|KNN|1|6.21%
knn3|KNN|3|7.59%
knn5|KNN|5|8.97%
knn10|KNN|10|13.1%

-----

```{r, message=FALSE, warning=FALSE}
# K=1
library(class)
set.seed(13)
knn1 <- knn(train = Boston.train[,-14],
            test = Boston.test[,-14],
            cl = Boston.train[,14],
            k = 1
            )
# test error rate
mean(knn1!=Boston.test$crim.result)

# K=3
set.seed(13)
knn3 <- knn(train = Boston.train[,-14],
            test = Boston.test[,-14],
            cl = Boston.train[,14],
            k = 3
            )
# test error rate
mean(knn3!=Boston.test$crim.result)

# K=5
set.seed(13)
knn5 <- knn(train = Boston.train[,-14],
            test = Boston.test[,-14],
            cl = Boston.train[,14],
            k = 5
            )
# test error rate
mean(knn5!=Boston.test$crim.result)


# K=10

set.seed(13)
knn10 <- knn(train = Boston.train[,-14],
            test = Boston.test[,-14],
            cl = Boston.train[,14],
            k = 10
            )
# test error rate
mean(knn10!=Boston.test$crim.result)

```

