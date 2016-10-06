##########################################################
# DSC5103 Statistics
# Session 6. Demo of Cross-Validation of k-nearest neighbour classification on the mixture.example dataset
# 2016.9
#
# -- based on the mixture.example dataset and documentation in the "ElemStatLearn" package
##########################################################


#############################
### Loading data
#############################
# load the dataset "mixture.example" in package "ElemStatLearn")
library("ElemStatLearn")  # run install.packages("ElemStatLearn") if you haven't



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


# make dataframe for x and y (for ggplot use)
df.training <- data.frame(x1=x[ , 1], x2=x[ , 2], y=y)
summary(df.training)
df.training$y <- as.factor(df.training$y)

# dataframe for plotting the boundary
df.grid <- data.frame(x1=xnew[ , 1], x2=xnew[ , 2])
df.grid$prob <- prob
summary(df.grid)


# plot X and Y
library("ggplot2")
p0 <- ggplot() + geom_point(data=df.training, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw()
p0

# add the true boundary into the plot
p.true <- p0 + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob), breaks=c(0.5))
p.true



#############################
### Test Data
#############################
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

# irreducible error (the number comes from the data generating model)
bayes.error <- sum(mixture.example$marginal * (prob * I(prob < 0.5) + (1-prob) * I(prob >= 0.5)))
########################################################################



#############################
### CV error
#############################
library("FNN")
?knn.cv  # LOOCV for knn

# test knn.cv
mod15cv <- knn.cv(x, y, k=15, prob=TRUE)
str(mod15cv)
mod15cv

# enumerate many many k values and measure misclassification rate
ks <- c(1, 3, 5, 7, 9, 11, 15, 17, 23, 25, 35, 45, 55, 75, 99)
misclass.train <- numeric(length=length(ks))
misclass.test  <- numeric(length=length(ks))
misclass.cv  <- numeric(length=length(ks))

for (i in seq(along=ks)) {
    mod.train <- knn(x, x, y, k=ks[i])
    mod.test  <- knn(x, x.test, y, k=ks[i])
    mod.cv <- knn.cv(x, y, k=ks[i])
    misclass.train[i] <- sum(mod.train != y) / length(y)
    misclass.test[i] <- sum(mod.test != y.test) / length(y.test)
    misclass.cv[i] <- sum(mod.cv != y) / length(y)
}
misclass <- data.frame(k=ks, train=misclass.train, test=misclass.test, cv=misclass.cv)
misclass


# plot misclassification rate on Training, Test, and Cross-Validation
plot.mse <- ggplot() + geom_line(aes(x=ks, y=misclass.train), color="red") + geom_point(aes(x=ks, y=misclass.train))  + geom_line(aes(x=ks, y=misclass.test), color="blue") + geom_point(aes(x=ks, y=misclass.test)) + geom_line(aes(x=ks, y=misclass.cv), color="green") + geom_point(aes(x=ks, y=misclass.cv))  + geom_hline(aes(yintercept = bayes.error), linetype="dashed") + scale_x_reverse(lim=c(100, 1))  + theme_bw()
plot.mse
