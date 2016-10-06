##########################################################
# DSC5103 Statistics
# Session 7. Linear Model Selection with Cross-Validation
# 2016.9
#
# using the regsubsets() function in the "leaps" package
# using the Hitters dataset in package "ISLR"
##########################################################

library("ISLR")
names(Hitters)
dim(Hitters)

# the Salary column has NA's
sum(is.na(Hitters$Salary))
# remove NA's in Salary
Hitters <- na.omit(Hitters)
sum(is.na(Hitters))
dim(Hitters)
n <- nrow(Hitters)  # number of data points in the sample
p <- ncol(Hitters) - 1  # number of predictors in the sample
summary(Hitters)



## full subsets selection
library("leaps")
?regsubsets
regfit.full <- regsubsets(Salary ~ ., data=Hitters)
summary(regfit.full)  # by default, regsubsets() evaluate models up to size 8

# set max model size to 19
regfit.full <- regsubsets(Salary ~ ., data=Hitters, nvmax=19)
summary(regfit.full)

# coefficients in the fitted models
coef(regfit.full, 10)

# outputs
reg.summary <- summary(regfit.full)
names(reg.summary)
# R-squared
reg.summary$rsq
plot(reg.summary$rsq, xlab="Number of Variables", ylab="rsq", type="o")
# Adj R-squared
reg.summary$adjr2
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="o")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)
# Cp
reg.summary$cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type='o')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col="red", cex=2, pch=20)
# BIC
reg.summary$bic
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type='o')
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col="red", cex=2, pch=20)



## forward selection
regfit.fwd <- regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)


## backward selection
regfit.bwd <- regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)


# the three selections may not give the same model
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)



## Choosing optimal p using Cross Validation

# first create a predict() function for regsubsets()
predict.regsubsets <- function(object, newdata, p, ...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=p)
    xvars <- names(coefi)
    mat[,xvars] %*% coefi
}

K <- 10  # k-fold CV
# partion data into K folds
set.seed(1)
fold <- rep(0, n)
shuffled.index <- sample(n, n, replace=FALSE)
fold[shuffled.index] <- 1:K  # rep(1:K, length.out=n)
table(fold)

# data frame for storing the results
predictions <- Hitters[, c("Salary"), drop=FALSE]
predictions$fold <- fold
head(predictions)

# K-fold CV
for(k in 1:K){
    model.k <- regsubsets(Salary ~ ., data=Hitters[fold != k, ], nvmax=19)
    for(i in 1:p){
        pred <- predict(model.k, Hitters[fold == k, ], id=i)
        predictions[fold == k, paste0("pred", i)] <- pred
    }
}
head(predictions)

# calculate cross-validation MSE
cv.residuals <- predictions$Salary - predictions[, 3:21]
cv.mse <- colMeans(cv.residuals ^ 2)
cv.mse
# plot CV MSE against in-sample MSE 
plot(cv.mse, type="o", ylim=c(90000, 160000), col="red")
lines(reg.summary$rss / nrow(Hitters), type="o")

# calculate cross-validation R^2
cv.rsq <- cor(predictions$Salary, predictions[, 3:21])^2
cv.rsq
plot(1:19, cv.rsq, type="o", ylim=c(0.25, 0.6), col="red")
lines(1:19, reg.summary$rsq, type="o")

# optimal p
p_opt <- which.min(cv.mse)



# final optimal model
regfit.best <- regsubsets(Salary ~ ., data=Hitters, nvmax=19)
coef(regfit.best, p_opt)
str(regfit.best)
best.summary <- summary(regfit.best)
best.summary$which[10,]



## compare with stepAIC()
library("MASS")
lr <- lm(Salary ~ ., data=Hitters)
regfit.aic <- stepAIC(lr, direction="both")
summary(regfit.aic)
# happen to be the same ...
coef(regfit.best, p_opt)
