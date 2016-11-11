---
title: "DSC5103 Assignment 4"
subtitle: 'Regularization Methods in Classification'
date: "Oct 2016"
output:
  html_document:
    highlight: tango
    theme: yeti
---
<!--
comments must be put in an HTML comment form
-->

```{r set-options, echo=FALSE, cache=FALSE}
options(width = 100)  # set output width, turn off scientific notation for big numbers
```

## NOTE:
This assignment is **due at 23:59 of Oct 13, Thursday**. You can work on this file directly and fill in your answers/code below. Please submit the output HTML file (name your file like G1Group02.html if you are from Group 02 of Section G1) onto IVLE/Files/Student Submission/Assignment4 folder.

Also, put the Section/Group and member info below.
```{r}
# Section G?
# Group ??
# Members: YOUR NAMES HERE
```



### Introduction
In this assignment, we will apply regularization methods (Ridge Regression, LASSO, and Elastic Net) to a classification problem, and compare them with traditional Logistic Regression.

Before we start, it is necessary to read through the documentation of the functions **glmnet()** (specifically, the *family* option), **cv.glmnet()** (the *family* and *type.measure* options), and **predict.glmnet()** (the *type* option) in the **glmnet** package, and find out how to run generalized linear models with regularization.

### Data Preparation
First, let us get the data. We will use the **Heart** data from the textbook, available at http://www-bcf.usc.edu/~gareth/ISL/Heart.csv.
```{r}
heart <- read.csv(file="http://www-bcf.usc.edu/~gareth/ISL/Heart.csv", row.names=1)
summary(heart)
head(heart,20)
```
The task is to use the features to predict **AHD**, binary outcome related to some heart disease. 

Some cleaning is necessary because there are NA's and also several categorical variables stored as numerical.
```{r}
# clean the NA's
heart <- na.omit(heart)
# convert to factors
heart$Sex <- as.factor(heart$Sex)
heart$Fbs <- as.factor(heart$Fbs)
heart$RestECG <- as.factor(heart$RestECG)
heart$ExAng <- as.factor(heart$ExAng)
heart$Slope <- as.factor(heart$Slope)
heart$Ca <- as.factor(heart$Ca)
summary(heart)
head(heart,20)
str(heart)
```

Next, we will prepare the training and test dataset for later model comparison.
```{r}
# split training and test data 50/50
N <- nrow(heart)
set.seed(456)
train.index <- sample(1:N, round(N/2))
head(train.index,20)
test.index <- - train.index
head(test.index,20)
```
Because function **glmnet()** only takes data in matrix form, we need a copy of the training and test data in matrix form.
```{r}
# construct x and y matrix for glmnet()
x <- model.matrix(AHD ~ ., heart[train.index, ])[, -1]
x
y <- heart[train.index, "AHD"]
y
x.test <- model.matrix(AHD ~ ., heart[-train.index, ])[, -1]
y.test <- heart[-train.index, "AHD"]
y.test
```


### Questions and Answers
#### 1. [Logistic Regression as a benchmark] Find the optimal Logistic Regression model with **stepAIC()** using the training data, and use the model to predict using the test data.  (1 Mark)

Answer: 

```{r}
# put your R code here inside the blocks
set.seed(456)
heart$AHD <- as.numeric(heart$AHD) - 1
head(heart,20)
glm <- glm(AHD ~ ., data = heart, subset = train.index, family=binomial(link = "logit"))
summary(glm)
library("MASS")
glm.best <- stepAIC(glm, direction="both")
summary(glm.best)
library("ROCR")
glm.best.prob <- predict(glm.best, newdata=heart[test.index,], type="response")
glm.best.prob

glm.best.pred <- (glm.best.prob > 0.5)
glm.best.pred
```



#### 2. [Ridge Regression] Fit a Ridge Regression model on the training data, use cross-validtion to find the optimal $\lambda$, and use the optimal model to predict on the test data.  (1 Mark)

Answer: 

```{r}
# put your R code here inside the blocks
set.seed(456)
library("glmnet")
set.seed(456)
ridge.mod <- glmnet(x, y, family=c("binomial"), alpha=0)
ridge.cv <- cv.glmnet(x, y, family="binomial",  type.measure="class", alpha=0)

plot(ridge.cv)

ridge.lam <- ridge.cv$lambda.1se
log(ridge.lam)
ridge.lam
min(ridge.cv$cvm) + ridge.cv$cvsd[which.min(ridge.cv$cvm)]
points(log(ridge.lam), min(ridge.cv$cvm) + ridge.cv$cvsd[which.min(ridge.cv$cvm)], cex=3)

ridge.mod.prob <- predict(ridge.mod, type="response", s=ridge.lam, newx=x.test, exact=TRUE)
ridge.mod.prob 

```


#### 3. [LASSO] Fit a LASSO model on the training data, use cross-validtion to find the optimal $\lambda$, and use the optimal model to predict on the test data.  (1 Mark)

Answer: 

```{r}
# put your R code here inside the blocks
set.seed(456)
lasso.mod <- glmnet(x, y, family="binomial", alpha=1)
lasso.cv <- cv.glmnet(x, y, family="binomial",  type.measure="class", alpha=1)
plot(lasso.cv)

lasso.lam <- lasso.cv$lambda.1se
log(lasso.lam)
lasso.lam
min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)]
points(log(lasso.lam), min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)], cex=3)

lasso.mod.prob <- predict(lasso.mod, type="response", s=lasso.lam, newx=x.test, exact=TRUE)
lasso.mod.prob 
```


#### 4. [Elastic Net] Fit an Elastic Net model on the training data, use cross-validtion to find the optimal $\alpha$ and $\lambda$, and use the optimal model to predict on the test data.  (1 Mark)

Answer: 

```{r}
# put your R code here inside the blocks
set.seed(456)
K <- 10
n <- nrow(x)
fold <- rep(0, n)
shuffled.index <- sample(n, n, replace=FALSE)
fold[shuffled.index] <- rep(1:K, length.out=n)
table(fold)

alphas <- seq(0, 1, 0.01)
en.cv.error <- data.frame(alpha=alphas)
for (i in 1:length(alphas)){
     en.cv <- cv.glmnet(x, y, family="binomial", type.measure="class", alpha=alphas[i], foldid=fold)
     en.cv.error[i, "lambda.1se"] <- en.cv$lambda.1se
     en.cv.error[i, "error.1se"] <- min(en.cv$cvm) + en.cv$cvsd[which.min(en.cv$cvm)]
}
en.cv.error

# optimal lambda and alpha
#en.lam <- en.cv.error[which.min(en.cv.error$error.min), "lambda.min"]
#en.alpha <- en.cv.error[which.min(en.cv.error$error.min), "alpha"]
en.lam <- en.cv.error[which.min(en.cv.error$error.1se), "lambda.1se"]
en.lam
en.alpha <- en.cv.error[which.min(en.cv.error$error.1se), "alpha"]
en.alpha

# plot optimal alpha
plot(en.cv.error$alpha, en.cv.error$error.1se, type="l")
abline(v=en.alpha, lty=2)


en.mod <- glmnet(x, y, family=c("binomial"), alpha=en.alpha)
en.mod.prob <- predict(en.mod, type="response", s=en.lam, newx=x.test, exact=TRUE)
en.mod.prob 
```



#### 5. Compare the above-studied model predictions in terms of misclassification rate, ROC, and AUC.  (1 Mark)

Answer: 

```{r}
# put your R code here inside the blocks

### Misclassification and Plot###
glm.best.pred <- (glm.best.prob  > 0.5)
glm.best.pred
glm.best.pred <- prediction(glm.best.prob, y.test)
glm.best.pred
glm.best.err <- performance(glm.best.pred, measure="err")
glm.best.err

ridge.mod.pred <- (ridge.mod.prob  > 0.5)
ridge.mod.pred
ridge.mod.pred <- prediction(ridge.mod.prob, y.test)
ridge.mod.pred
ridge.mod.err <- performance(ridge.mod.pred, measure="err")
ridge.mod.err

lasso.mod.pred <- (lasso.mod.prob  > 0.5)
lasso.mod.pred
lasso.mod.pred <- prediction(lasso.mod.prob, y.test)
lasso.mod.pred
lasso.mod.err <- performance(lasso.mod.pred, measure="err")
lasso.mod.err

en.mod.pred <- (en.mod.prob  > 0.5)
en.mod.pred
en.mod.pred <- prediction(en.mod.prob, y.test)
en.mod.pred
en.mod.err <- performance(en.mod.pred, measure="err")
en.mod.err

plot(glm.best.err, col="black", ylab="")
plot(ridge.mod.err, col="green", add=TRUE )
plot(lasso.mod.err, col="red", add=TRUE)
plot(en.mod.err, col="blue", add=TRUE)



### ROC and Plot ###
glm.best.ROC <- performance(glm.best.pred, measure="tpr", x.measure="fpr")
plot(glm.best.ROC)

ridge.mod.ROC <- performance(ridge.mod.pred, measure="tpr", x.measure="fpr")
plot(ridge.mod.ROC, col="green", add=TRUE)

lasso.mod.ROC <- performance(lasso.mod.pred, measure="tpr", x.measure="fpr")
plot(lasso.mod.ROC, col="red", add=TRUE)

en.mod.ROC <- performance(en.mod.pred, measure="tpr", x.measure="fpr")
plot(en.mod.ROC, col="blue", add=TRUE)

legend(x=0.65, y=0.3, legend=c("glm.best.ROC", "ridge.mod.ROC", "lasso.mod.ROC", "en.mod.ROC"), lty=c(1, 1, 1, 1), lwd=c(2, 2, 2, 2), col=c("black", "green", "red", "blue"))



### AUC and Plot ###
glm.best.auc <- performance(glm.best.pred, "auc")
as.numeric(glm.best.auc@y.values)

ridge.mod.auc <- performance(ridge.mod.pred, "auc")
as.numeric(ridge.mod.auc@y.values)

lasso.mod.auc <- performance(lasso.mod.pred, "auc")
as.numeric(lasso.mod.auc@y.values)

en.mod.auc <- performance(en.mod.pred, "auc")
as.numeric(en.mod.auc@y.values)

```

