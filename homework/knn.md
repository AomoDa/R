---
title: "DSC5103 Assignment 1"
subtitle: "Simulation and K-Nearest Neighbor Algorithm"
date: "Aug 2016"
output:
  html_document:
    theme: yeti
    highlight: tango
  pdf_document:
    highlight: zenburn
---
<!--
comments must be put in an HTML comment form
-->

## NOTE:
This assignment is **due at 23:59 of Aug 25, Thursday**. You can work on this file directly and fill in your answers/code below. Please submit the output HTML file (name your file like G1Group02.html if you are from Group 02 of Section G1) onto IVLE/Files/Student Submission/Assignment1 folder.

Also, put the Section/Group and member info below.
```{r}
# Section G1
# Group 09
# Members: RUI YANG, RUI BAO, ZHEN YU, WEIYULAI
```



## Part I: Simulation
In this exercise, we will use simulation to illustrate the variability of statistics calculated from random samples. Suppose there is a **normal** population of size **10000**, with mean **100** and standard deviation **15**. Now we draw a sample from the population, of size **100** without replacement, we can calculate sample statistics such as mean and variance. If we further repeat the sampling process many times, say **200**, we will have 200 sets of similar sample statistics. Let's examine these sample statistics.

The necessary parameters are already set up as below.
```{r}
pop.size <- 10000
pop.mean <- 100
pop.sd <- 15

num.of.samples <- 200
sample.size <- 100
```

### Questions and Answers
1. Use random seed **1234** to conduct the simulation (i.e., simulate the population as specified, draw 200 samples, and calculate sample mean and variance for each sample, respectively), evaluate the mean and standard deviation of the sample statistics, and compare with their theoretical values. Draw histograms of the sample statistics. (1 Mark)

Answer: 

```{r}
set.seed(1234)
pop <- rnorm(pop.size,mean=pop.mean,pop.sd) #create a population
mean.of.sample <- numeric(length=length(num.of.samples))
sd.of.sample <- numeric(length=length(num.of.samples))
for (i in seq(1,num.of.samples,by=1)){
  sam <- sample(x=pop,size=sample.size,replace=FALSE)
  mean.of.sample[i] <- mean(sam)
  sd.of.sample[i] <- sd(sam)
}
mean.of.sample.mean <-mean(mean.of.sample)
sd.of.sample.mean <- sd(mean.of.sample)
mean.of.sample.sd <- mean(sd.of.sample)
sd.of.sample.sd <- sd(sd.of.sample)
mean.of.sample
hist(mean.of.sample,breaks=10,probability=TRUE,main="The distribution of mean of 200 samples",xlab="mean of samples",ylab="frequency",xaxt="n",yaxt="n")
axis(side=1,at=seq(95,105,1))
axis(side=2,at=seq(0,0.4,0.05))
curve(dnorm(x,mean=mean.of.sample.mean,sd=sd.of.sample.mean),add=TRUE,col="blue")
curve(dnorm(x,mean=100,sd=1.5),add=TRUE,col="red")#mean equals to sample.mean,sd equals to (pop.sd)/((sample.size)^0.5)
sd.of.sample
hist(sd.of.sample,breaks=10,probability=TRUE,main="The distribution of standard deviation of 200 samples",xlab="standard deviation of samples",ylab="frequency",xaxt="n",yaxt="n")
axis(side=1,at=seq(10,20,1))
axis(side=2,at=seq(0,0.5,0.05))
curve(dnorm(x,mean=mean.of.sample.sd,sd=sd.of.sample.sd),add=TRUE,col="blue")
curve(dnorm(x,mean=15,sd=1),add=TRUE,col="red")

```


## Part II: K-Nearest Neighbor Algorithm

### Introduction
In this assignment, we are going to experiment the K-Nearest Neighbor (KNN) algorithm on a higher-dimensional dataset and experience the deterioration of prediction performance as the dimensionality grows.

The experiment is built on top of the 3rd-order polynomial model discussed in class (knn_demo.R), i.e.,
$$y = \beta_0 + \beta_1 * x + \beta_2 * x^2 + \beta_3 * x^3 + \epsilon, ~~ \epsilon \sim \text{N}(0, \sigma^2)$$
and we are going to introduce an extra 20-dimensional predictor $z$, which does NOT actually play a role in generating $y$. Yet, when in estimation, we do not know the fact and will use both $x$ and $z$ as predictors in the KNN algorithm.

### Generation of the high-dimensional dataset
We first simulate the 3rd-order polynomial datasets as we did in knn_demo.R. 

```{r}
library("ggplot2")

## population parameters
beta0 <- 1
beta1 <- -2
beta2 <- 6
beta3 <- -1
sigma <- 2

set.seed(7890)

## training data
x <- seq(0, 4.95, 0.05)
f_x <- beta0 + beta1 * x + beta2 * x^2 + beta3 * x^3
epsilon <- rnorm(n=100, mean=0, sd=sigma)
y <- f_x + epsilon

## test data
x.test <- seq(0, 5, 0.1)
f_x.test <- beta0 + beta1 * x.test + beta2 * x.test^2 + beta3 * x.test^3
epsilon.test <- rnorm(n=length(x.test), mean=0, sd=sigma)
y.test <- f_x.test + epsilon.test
```
The resulted training and test dataset have `r length(y)` and `r length(y.test)` data points, respectively.

Next, we need to generate $z$, the 20-dimensional predictors, of the same sizes. Each $z$ is a 20-dimensional multivariate normal random variable, with mean being $(0, 0, \ldots, 0)$ and identity covariance matrix (so that the 20 elements are independent standard normal random variables). The resulted $z$ is a 100*20 matrix, with each row being a data point with 20 dimensions.
```{r}
library("mvtnorm")  # package for multivariate normal distribution, INSTALL IT BEFORE RUNNING
z <- rmvnorm(n=100, mean=rep(0, 20))  # covariance matrix is identity matrix by default, no need to specify here
z.test <- rmvnorm(n=51, mean=rep(0, 20))
head(z)
```

Later, we will use $(x, z)$ to predict $y$. Let's first combine $x$ and $z$ into matrices, as required by function knn.reg().
```{r}
train.x <- cbind(x, z)
test.x <- cbind(x.test, z.test)
head(train.x)
```

### Questions

#### 1.	For a fixed $k=15$, fit a KNN model to predict $y$ with $(x, z)$, and measure the training and test MSE. (1 Mark)

Answer: 

```{r}
# visualize the training data (x, y) and the true model f_x
plot.train <- ggplot() + geom_point(aes(x=x, y=y), size=2) + geom_line(aes(x=x, y=f_x)) + theme_bw()
plot.train
# visualize the test data (x, y) and the true model f_x
plot.test <- ggplot() + geom_point(aes(x=x.test, y=y.test), size=2) + geom_line(aes(x=x.test, y=f_x.test)) + theme_bw()
plot.test

## 1. k=15
library("FNN")
model15.train <- knn.reg(train=train.x, test=train.x, y=y, k=15)
str(model15.train)
# plot the fit
plot.train + geom_line(aes(x=x, y=model15.train$pred), col="blue")

# Training MSE
mean((y - model15.train$pred)^2)


## 1b. k=15, test MSE
model15.test <- knn.reg(train=train.x, test=test.x, y=y, k=15)
str(model15.test)

# plot the fit
plot.test + geom_line(aes(x=x.test, y=model15.test$pred), col="blue")

# Test MSE
mean((y.test - model15.test$pred)^2)

```


#### 2.	With the same data, plot the training and test MSE of the KNN model against $k$, and find the optimal $k$ and the corresponding test MSE.  (1 Mark)

Answer: 

```{r}
# k's that will be evaluated
ks <- 1:30
# construct empty vectors for keeping the MSE for each k
mse.train <- numeric(length=length(ks))
mse.test  <- numeric(length=length(ks))

# loop over all the k and evaluate MSE in each of them
for (i in seq(along=ks)) {
    model.train <- knn.reg(train.x, train.x, y, k=ks[i])
    model.test  <- knn.reg(train.x, test.x, y, k=ks[i])
    mse.train[i] <- mean((y - model.train$pred)^2)
    mse.test[i] <- mean((y.test - model.test$pred)^2)
}
mse.train
mse.test

# optimal k
k.opt <- ks[which.min(mse.test)]
# optimal MSE
mse.opt <- min(mse.test)


# plot MSE on Training and Test
ggplot() + geom_line(aes(x=ks, y=mse.train), color="red") + geom_point(aes(x=ks, y=mse.train)) + geom_line(aes(x=ks, y=mse.test), color="blue") + geom_point(aes(x=ks, y=mse.test)) + scale_x_reverse(lim=c(30, 1)) + geom_hline(yintercept=sigma^2, linetype=2) + theme_bw()

k.opt
mse.opt
```

#### 3.	Based on the analysis above, compare the above model with $(x, z)$ being the predictors and the previous model with $x$ only (as in knn_demo.R). Briefly explain why.  (1 Mark)

#Answer: 
x and z: The optimal k is 13 and the optimal mse is 28.26073
only x : The optimal k is 8 and the optimal mse is 3.83
Everytime a dimension is added, just like in a right angle triangle,newMSE = oldMSE + MSE of this new dimension.Also,with the increasing of dimension,we are further away from the irreduciable error.

#### 4.	We have seen that the test MSE is significantly worse than what we had without using predictor $z$ (in knn_demo.R). To better understand the impact of including irrelevant predictors in the KNN algorithm, let's try to include the 20 dimensions of $z$ one by one. So in each round $j$, we construct the predictors by combining $x$ and the first $j$ columns of $z$, then repeat the analysis in Question 2 and find the optimal $k$ and test MSE. At the end, plot the optimal MSE agaist $j$, and interpret the result.  (1 Mark)

Answer: 

```{r}
ks <- 1:30
js <- 1:20
  # construct empty vectors for keeping the MSE for each k
  mse.train <- numeric(length=length(ks))
  mse.test  <- numeric(length=length(ks))
  k.opt.opt <- numeric(length=length(js))
  mse.opt.opt <- numeric(length=length(js))
for (j in seq(along=js)){
  train.x <- cbind(x,z[,1:j])
  test.x <- cbind(x.test, z.test[,1:j])
  # loop over all the k and evaluate MSE in each of them
  for (i in seq(along=ks)) {
    model.train <- knn.reg(train.x, train.x, y, k=ks[i])
    model.test  <- knn.reg(train.x, test.x, y, k=ks[i])
    mse.train[i] <- mean((y - model.train$pred)^2)
    mse.test[i] <- mean((y.test - model.test$pred)^2)
  }
  # optimal k
  k.opt <- ks[which.min(mse.test)]
  # optimal MSE
  mse.opt <- min(mse.test)
  k.opt.opt[j] <- k.opt
  mse.opt.opt[j] <- mse.opt 
}
k.opt.opt
mse.opt.opt

ggplot() + geom_line(aes(x=js, y=mse.opt.opt), color="red") + geom_point(aes(x=js, y=mse.opt.opt)) +scale_x_reverse(lim=c(20, 1)) + geom_hline(yintercept=sigma^2, linetype=2) + theme_bw()

# interpret: In general, as js increases, the mse.opt.opt is also on the increase. But sometimes mse.opt.opt will fluctuate, i.e. not strictly positive correlation at each point because different js has different optimal K value.
```



## Session Info

```{r session-info}
print(sessionInfo(), locale=FALSE)
```
