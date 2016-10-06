##########################################################
# DSC5103 Statistics
# Session 6. Demo of Cross-Validation of Logistic Regression classification on the mixture.example dataset
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
### Cross-Validation
#############################
K <- 10  # 10-fold CV
RUN <- 10  # the number of repetitions of CV
I <- 9  # the max polynomical order to consider

#############################
## auto CV with cv.glm()
#############################
library("boot")
# prepare an empty data.frame to save the MSE for each *run* and *i*
err.kfold <- expand.grid(run=1:RUN, i=1:I)
err.kfold$err <- 0

# misclassification error with cutoff=0.5
cost <- function(y, p = 0) {mean(y != (p > 0.5))}

set.seed(1688)
for (run in 1:RUN) {
    for (i in 1:I) {
        glm.fitted <- glm(y ~ poly(x1, i) + poly(x2, i), family=binomial(), data=df.training)
        err.kfold[err.kfold$run == run & err.kfold$i == i, "err"] <- cv.glm(df.training, glm.fitted, cost, K=K)$delta[1]  # specify K=10 for k-fold CV
    }
}
head(err.kfold)
# plot the Error for each run and each i
ggplot(data=err.kfold, aes(x=i, y=err)) + geom_line(aes(color=factor(run))) + scale_x_discrete () + theme_bw()


#############################
## manual k-fold Cross-Validation
#############################
# test randomly partition data into K chunks
folds <- sample(1:K, nrow(df.training), replace=TRUE)
table(folds)

# a better but more complicated way
folds2 <- split(sample(nrow(df.training), nrow(df.training), replace=FALSE), as.factor(1:K))
df.training[folds2[[1]],]
summary(df.training[folds2[[1]],])
summary(df.training[-folds2[[1]],])


## manual k-fold Cross-Validation for misclassification rate
err.kfold2 <- expand.grid(run=1:RUN, i=1:I)
err.kfold2$err <- 0

set.seed(1688)
for (run in 1:RUN) {
    # create a random partition
    folds <- sample(1:K, nrow(df.training),replace=TRUE)
    for (i in 1:I) {
        err <- 0  # overall misclassfication errors in all k folds
        # start the k-fold CV
        for (k in 1:K) {
            glm.fitted <- glm(y ~ poly(x1, i) + poly(x2, i), family=binomial(), data=df.training[folds != k, ])
            glm.prob <- predict(glm.fitted, newdata=df.training[folds == k, ], type="response")
            glm.pred <- ifelse(glm.prob > 0.5, 1, 0)
            glm.err <- sum(glm.pred != df.training[folds == k, "y"])
            err <- err + glm.err
        }
        err.kfold2[err.kfold2$run == run & err.kfold2$i == i, "err"] <- err / nrow(df.training)
    }
}
head(err.kfold2)
# plot the Error for each run and each i
ggplot(data=err.kfold2, aes(x=i, y=err)) + geom_line(aes(color=factor(run))) + scale_x_discrete () + theme_bw()



## manual k-fold Cross-Validation for ROC plots
library("ROCR")
i <- 6
set.seed(16888)
for (run in 1:RUN) {
    # create a random partition
    folds <- sample(1:K, nrow(df.training), replace=TRUE)
    # start the k-fold CV
    for (k in 1:K) {
        glm.fitted <- glm(y ~ poly(x1, i) + poly(x2, i), family=binomial(), data=df.training[folds != k, ])
        df.training[folds == k, "prob"] <- predict(glm.fitted, newdata=df.training[folds == k, ], type="response")
    }
    pred <- prediction(df.training$prob, df.training$y)
    perf <- performance(pred, measure="tpr", x.measure="fpr")
    plot(perf, add=(run != 1))
}
