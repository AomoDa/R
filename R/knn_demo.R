##########################################################
# DSC5103 Statistics
# Session 2. Demo of k-nearest neighbour on a simple 1-dimension data
# 2016.08
#
##########################################################

library("ggplot2")

#############################
### Generating data
# Simulate a 3rd-order polynomial data
# y = beta0 + beta1 * x + beta2 * x^2 + beta3 * x^3 + epsilon
# epsilon ~ N(0, sigma^2)
#############################
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

# visualize the training data (x, y) and the true model f_x
plot.train <- ggplot() + geom_point(aes(x=x, y=y), size=2) + geom_line(aes(x=x, y=f_x)) + theme_bw()
plot.train

## test data
x.test <- seq(0, 5, 0.1)
f_x.test <- beta0 + beta1 * x.test + beta2 * x.test^2 + beta3 * x.test^3
epsilon.test <- rnorm(n=length(x.test), mean=0, sd=sigma)
y.test <- f_x.test + epsilon.test

# visualize the test data (x, y) and the true model f_x
plot.test <- ggplot() + geom_point(aes(x=x.test, y=y.test), size=2) + geom_line(aes(x=x.test, y=f_x.test)) + theme_bw()
plot.test


#############################
### KNN regression
#############################

# knn regression is in package "FNN" 
library("FNN")
?knn.reg


# prepare the X data in the matrix format, as required by knn.reg()
train.x <- matrix(x, ncol=1)
test.x <- matrix(x.test, ncol=1)


## 1. k=15, training MSE
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



## 2. k=1
model1.train <- knn.reg(train=train.x, test=train.x, y=y, k=1)
str(model1.train)

# plot the fit
plot.train + geom_line(aes(x=x, y=model1.train$pred), col="blue")

# Training MSE
mean((y - model1.train$pred)^2)


## 2b. k=1, test MSE
model1.test <- knn.reg(train=train.x, test=test.x, y=y, k=1)
str(model1.test)

# plot the fit
plot.test + geom_line(aes(x=x.test, y=model1.test$pred), col="blue")

# Test MSE
mean((y.test - model1.test$pred)^2)



## 3. k=50
model50.train <- knn.reg(train=train.x, test=train.x, y=y, k=50)
str(model50.train)

# plot
plot.train + geom_line(aes(x=x, y=model50.train$pred), col="blue")

# Training MSE
mean((y - model50.train$pred)^2)



## 4. Training and Test Error plot: to enumerate many many k values and measure MSE
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


