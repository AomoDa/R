##########################################################
# DSC5103 Statistics
# Session 2. Demo of k-nearest neighbour classification on the mixture.example dataset
# 2016.08
#
# -- based on the mixture.example dataset and documentation in the "ElemStatLearn" package
# -- more data details can be found at http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/mixture.example.info.txt
##########################################################


#############################
### Loading data
#############################
# load the dataset "mixture.example" in package "ElemStatLearn")
library("ElemStatLearn")  # run install.packages("ElemStatLearn") if you haven't
?mixture.example
str(mixture.example)


#############################
### Exloration
#############################

# copy important ones out
x <- mixture.example$x
y <- mixture.example$y
prob <- mixture.example$prob
xnew <- mixture.example$xnew
px1 <- mixture.example$px1
px2 <- mixture.example$px2

summary(x)
summary(y)
summary(prob)

# make dataframe for x and y (for ggplot use)
df.training <- data.frame(x1=x[ , 1], x2=x[ , 2], y=y)
summary(df.training)
df.training$y <- as.factor(df.training$y)

# dataframe for plotting the boundary
df.grid <- expand.grid(x1=px1, x2=px2)
df.grid$prob <- prob
summary(df.grid)


# plot X and Y
library("ggplot2")
p0 <- ggplot() + geom_point(data=df.training, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw()
p0
## if use base R plot()
#plot(range(x[, 1]), range(x[, 2]), type="n", xlab="x1", ylab="x2")
#points(x, col=ifelse(y==1, "red", "green"))

# add the true boundary into the plot
p.true <- p0 + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob), breaks=c(0.5))
p.true
## if use base R plot()
#mat.prob <- matrix(prob, length(px1), length(px2))
#contour(px1, px2, mat.prob, levels=0.5, labels="", xlab="x1",
#        ylab="x2", main="Bayes decision boundary")
#points(x, col=ifelse(y==1, "red", "green"))


#############################
### KNN classification
#############################

# knn() is in package "class" 
#library("class")
library("FNN")
?knn

## 1. k=15
model15 <- knn(x, xnew, y, k=15, prob=TRUE)
str(model15)
prob15 <- attr(model15, "prob")
prob15 <- ifelse(model15 == "1", prob15, 1 - prob15)
df.grid$prob15 <- prob15

# plot
p15 <- p.true + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob15), breaks=c(0.5), color="blue", size=1) 
p15
## if use base R plot()
#mat.prob15 <- matrix(prob15, length(px1), length(px2))
#par(mar=rep(2,4))
#contour(px1, px2, mat.prob15, levels=0.5, labels="", xlab="", ylab="", main=
#            "15-nearest neighbour", axes=FALSE)
#points(x, col=ifelse(y==1, "coral", "cornflowerblue"))
#points(grid, pch=".", cex=1.2, col=ifelse(mat.prob15>0.5, "coral", "cornflowerblue"))
#box()

# Training Errors
y_hat15 <- ifelse(knn(x, x, y, k=15) == "1", 1, 0)
# total errors in training
sum(y_hat15 != y)
# misclassification rate in training
sum(y_hat15 != y) / length(y)


## 2. k=1
model1 <- knn(x, xnew, y, k=1, prob=TRUE)
str(model1)
prob1 <- attr(model1, "prob")
prob1 <- ifelse(model1 == "1", prob1, 1 - prob1)
df.grid$prob1 <- prob1

# plot
p1 <- p.true + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob1), breaks=c(0.5), color="blue", size=1) 
p1
## if use base R plot()
#mat.prob1 <- matrix(prob1, length(px1), length(px2))
#par(mar=rep(2,4))
#contour(px1, px2, mat.prob1, levels=0.5, labels="", xlab="", ylab="", main=
#            "1-nearest neighbour", axes=FALSE)
#points(x, col=ifelse(y == 1, "coral", "cornflowerblue"))
#points(grid, pch=".", cex=1.2, col=ifelse(mat.prob1 > 0.5, "coral", "cornflowerblue"))
#box()

# Training Errors
y_hat1 <- ifelse(knn(x, x, y, k=1) == "1", 1, 0)
# total errors in training
sum(y_hat1 != y)
# misclassification rate in training
sum(y_hat1 != y) / length(y)


## 3. k=100
model100 <- knn(x, xnew, y, k=100, prob=TRUE)
str(model100)
prob100 <- attr(model100, "prob")
prob100 <- ifelse(model100 == "1", prob100, 1 - prob100)
df.grid$prob100 <- prob100

# plot
p100 <- p.true + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob100), breaks=c(0.5), color="blue", size=1) 
p100
## if use base R plot()
#mat.prob100 <- matrix(prob100, length(px1), length(px2))
#par(mar=rep(2,4))
#contour(px1, px2, mat.prob100, levels=0.5, labels="", xlab="", ylab="", main=
#            "100-nearest neighbour", axes=FALSE)
#points(x, col=ifelse(y == 1, "coral", "cornflowerblue"))
#points(grid, pch=".", cex=1.2, col=ifelse(mat.prob100 > 0.5, "coral", "cornflowerblue"))
#box()

# Training Errors
y_hat100 <- ifelse(knn(x, x, y, k=100) == "1", 1, 0)
# total errors in training
sum(y_hat100 != y)
# misclassification rate in training
sum(y_hat100 != y) / length(y)


## 4. Test Error
########################################################################
# generate test data:
# The data do not contain a test sample, so we make one,
# using the description of the oracle page 17 of the book: The centers 
# is in the means component of mixture.example, with green(0) first, 
# so red(1).  For a test sample of size 10000 we simulate
# 5000 observations of each class.

# DO NOT WORRY about the secret algorithm for generating test data
library("mvtnorm")
set.seed(123)
centers <- c(sample(1:10, 5000, replace=TRUE), 
             sample(11:20, 5000, replace=TRUE))
means <- mixture.example$means
means <- means[centers, ]
x.test <- rmvnorm(10000, c(0, 0), 0.2 * diag(2))
x.test <- x.test + means
y.test <- c(rep(0, 5000), rep(1, 5000))
########################################################################


# enumerate many many k values and measure misclassification rate
ks <- c(1, 3, 5, 7, 9, 11, 15, 17, 23, 25, 35, 45, 55, 75, 99)
misclass.train <- numeric(length=length(ks))
misclass.test  <- numeric(length=length(ks))

for (i in seq(along=ks)) {
    model.train <- knn(x, x, y, k=ks[i])
    model.test  <- knn(x, x.test, y, k=ks[i])
    misclass.train[i] <- sum(model.train != y) / length(y)
    misclass.test[i] <- sum(model.test != y.test) / length(y.test)
}
misclass.train
misclass.test

# optimal k
k.opt <- ks[which.min(misclass.test)]
# optimal misclassification rate
misclass.opt <- min(misclass.test)


# plot misclassification rate on Training and Test
plot.mse <- ggplot() + geom_line(aes(x=ks, y=misclass.train), color="red") + geom_point(aes(x=ks, y=misclass.train))  + geom_line(aes(x=ks, y=misclass.test), color="blue") + geom_point(aes(x=ks, y=misclass.test))  + scale_x_reverse(lim=c(100, 1))  + theme_bw()
plot.mse
## if use base R plot()
#plot(misclass.train, xlab="Number of NN", ylab="Test error", type="n", xaxt="n")
#axis(1, 1:length(ks), as.character(ks))
#lines(misclass.test, type="b", col='blue', pch=20)
#lines(misclass.train, type="b", col='red', pch=20)
#legend("bottomright", lty=1, col=c("red", "blue"), legend=c("train ", "test "))

# add irreducible error to the plot (the number comes from the data generating model)
bayes.error <- sum(mixture.example$marginal * (prob * I(prob < 0.5) + (1-prob) * I(prob >= 0.5)))
plot.mse + geom_hline(aes(yintercept = bayes.error), linetype="dashed")
