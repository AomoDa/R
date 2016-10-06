
#  Demo of logistic regression on the Credit Card Default data



#############################
### load Credit Card Default data
#############################
library("ISLR")
?Default
head(Default)
summary(Default)


#############################
### try linear regression first
#############################
# convert Y into numerical variable
Default$default0 <- as.numeric(Default$default) - 1

# default ~ balance
lm0 <- lm(default0 ~ balance, data=Default)
summary(lm0)

# visualize the fit
plot(Default$balance, Default$default0)
abline(lm0, col="red", lwd=2)


#############################
### logistic regression
#############################

## Logistic regression 1: default ~ balance
# link="logit" is default and can be omitted 
glm1 <- glm(default ~ balance, data=Default, family=binomial(link="logit"))
summary(glm1)

# inspect the output
str(glm1)
points(Default$balance, glm1$fitted.values, col="blue")

# prediction by glm1
predict(glm1, newdata=data.frame(balance=c(1000, 2000)), type="response")
# check prediction types
?predict.glm


## Logistic regression 2: default ~ student
glm2 <- glm(default ~ student, data=Default, family=binomial(link="logit"))
summary(glm2)

plot(Default$student, glm2$fitted.values)


## Logistic regression 3: default ~ .
glm3 <- glm(default ~ balance + income + student, data=Default, family=binomial(link="logit"))
summary(glm3)


## model selectin by AIC
library("MASS")
glm.best <- stepAIC(glm3, direction="both")
summary(glm.best)

## update()
glm4 <- update(glm3, . ~ . - income)
summary(glm4)


## other outputs
coef(glm4)  # coefficients
confint(glm4)  # 95% CI for the coefficients
fitted(glm4, type="response")  # fitted values
residuals(glm4, type="deviance")  # residuals

# plots as in linear regression
par(mfrow=c(2,2))
plot(glm4)
par(mfrow=c(1,1))


#############################
### classification
#############################

## categorical prediction (use fixed cutoff = 0.5)
Default$glm4.prob <- predict(glm4, type="response")
Default$glm4.pred <- (Default$glm4.prob > 0.5)

# confusion matrix
confusion.mat <- table(Default$glm4.pred, Default$default)
confusion.mat
TP <- confusion.mat[2, 2]
TN <- confusion.mat[1, 1]
FP <- confusion.mat[2, 1]
FN <- confusion.mat[1, 2]
recall <- TP / (TP + FN)
specificity <- TN / (FP + TN)
precision <- TP / (TP + FP)
accuracy <- (TP + TN) / nrow(Default)


## evaluate the measures with different cutoffs using the ROCR package
library("ROCR")
# STEP 1. construct a "prediction" object (consists of p_hat and Y)
glm4.pred <- prediction(Default$glm4.prob, Default$default)
str(glm4.pred)

# STEP 2. Select the measures using performance()
?performance

## single measure with respect to cutoff
# fpr, fnr, misclassification
glm4.fpr <- performance(glm4.pred, measure="fpr")
str(glm4.fpr)
glm4.fnr <- performance(glm4.pred, measure="fnr")
glm4.err <- performance(glm4.pred, measure="err")

# plot the measures in one figure
plot(glm4.fpr, col="red", ylab="")
plot(glm4.fnr, col="blue", add=TRUE)
plot(glm4.err, col="black", add=TRUE)
legend(x=0.7, y=0.5, legend=c("Error Rate", "False Positive Rate", "False Negative Rate"), lty=c(1, 1, 1), lwd=c(2, 2, 2), col=c("black", "red", "blue"))


# accuracy vs. cutoff
glm4.acc <- performance(glm4.pred, measure="acc")
plot(glm4.acc)
# precision vs. cutoff
glm4.prec <- performance(glm4.pred, measure="prec")
plot(glm4.prec)


## double measure with respect to cutoff
# ROC plot
glm4.ROC <- performance(glm4.pred, measure="tpr", x.measure="fpr")
plot(glm4.ROC)
abline(a=0, b=1, lty=2) # diagonal line
# AUC
glm4.auc <- performance(glm4.pred, "auc")
as.numeric(glm4.auc@y.values)

# Precision-Recall plot
glm4.PR <- performance(glm4.pred, measure="prec", x.measure="rec")
plot(glm4.PR)

# Sensitivity-Specificity plot
glm4.SS <- performance(glm4.pred, measure="sens", x.measure="spec")
plot(glm4.SS)

# Lift chart
glm4.lift <- performance(glm4.pred, measure="lift", x.measure="rpp")
plot(glm4.lift)


## finally, the ultimate cost plot
glm4.cost <- performance(glm4.pred, measure="cost", cost.fp=1, cost.fn=10)
plot(glm4.cost)
# optimal cutoff
str(glm4.cost)
which.min(glm4.cost@y.values[[1]])
glm4.cost@x.values[[1]][which.min(glm4.cost@y.values[[1]])]


#############################
### logistic regression with Probit link
#############################

## Probit regression: default ~ .
glm.probit <- glm(default ~ balance + student, data=Default, family=binomial(link="probit"))
summary(glm.probit)




#############################
### prepare the dataset
#############################
?Titanic
str(Titanic)
Titanic  # it is a 4-dimensional table??

# visualize
library("graphics")
mosaicplot(Titanic, main = "Survival on the Titanic")

# convert to dataframe
Titanic.df <- as.data.frame(Titanic)
Titanic.df

# long to wide shape for logistic regression
library("reshape2")
data <- dcast(Titanic.df, Class + Sex + Age ~ Survived, value.var="Freq")
data$Age <- relevel(data$Age, ref="Adult")
summary(data)
data



#############################
### logistic regression on binomial data
#############################
# model 1
glm1 <- glm(cbind(Yes, No) ~ Class + Sex + Age, data, family = binomial())
summary(glm1)

# model 2
glm2 <- glm(cbind(Yes, No) ~ Class + Sex * Age, data, family = binomial())
summary(glm2)

# model 3
# this is a saturated model with 0 degree of freedom!!
glm3 <- glm(cbind(Yes, No) ~ Class * Sex * Age, data, family = binomial())
summary(glm3)


