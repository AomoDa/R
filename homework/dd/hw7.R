



$$\beta_0=5$$ $$\beta_1=-7$$ $$\beta_2=-3.5$$ $$\beta_3=-5$$

set.seed(100)
X <- rnorm(100)
eps <- rnorm(100)
beta0 = 5
beta1 = -7
beta2 = 3.5
beta3 = -5
Y <- beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps
mydata <- data.frame(y = Y, x = X)

# e
library(glmnet)
q8_e <- model.matrix(y ~ poly(x, 10, raw = T), data = mydata)[, -1]
lasso1 <- cv.glmnet(q8_e, Y, alpha = 1)
# bets lambda
lasso1$lambda.min
#Create plots of the cross-validation error as a function of lambda
plot(lasso1)

#Report the resulting coefficient estimates
coef.glmnet(lasso1,s=lasso1$lambda.min)


#f
library(leaps)

beta7 = 10
Y = beta0 + beta7 * X^7 + eps

mydata_f <- data.frame(y = Y, x = X)


#perform best subset selection
m1 <- regsubsets(y ~ poly(x, 10, raw = T), data = mydata_f, nvmax = 10)
par(mfrow=c(1,3))
plot(summary(m1)$cp,type='b',ylab='cp',main=paste0('CP choose model \n id=',which.min(summary(m1)$cp)),col='red')
plot(summary(m1)$bic,type='b',ylab='bic',main=paste0('BIC choose model \n id=',which.min(summary(m1)$bic)),col='blue')
plot(summary(m1)$adjr2,type='b',ylab='adjr2',main=paste0('R^2 choose model\n id=',which.max(summary(m1)$adjr2)),col='orange')
par(mfrow=c(1,1))
#the resulting coefficient estimates
coefficients(m1, id = 2)


#perform the lasso
q8_f <- model.matrix(y ~ poly(x, 10, raw = T), data = mydata_f)[, -1]
lasso_f <- cv.glmnet(q8_f, Y, alpha = 1)
plot(lasso_f)
# the resulting coefficient estimates
coef.glmnet(lasso_f,s=lasso_f$lambda.min)


#MNIST and Lasso

load("mnist68.RData")
images_df <- mnist68
myv <-  rep(NA,784)
for (j in 1:784){myv[j] <- var(images_df[,j])}
myfeatures <-  (1:784)[myv > quantile(myv, .9)]
mydf <-  images_df[,c(myfeatures,785)]
mydf$labels <- as.numeric(mydf$label==8)

#a
