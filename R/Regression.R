library(MASS)

#############  Data SAheart#######################################################
# read data
SAheart<-read.table('SAheart.txt',header=T)
attach(SAheart)



####logistic regression
## model fitting
model_logit = glm(chd ~ . , family = binomial(link='logit'), data=SAheart)
require(nnet)
model_logit2 <- multinom(chd~.,data=SAheart)

## model outputs
summary(model_logit)
str(model_logit)
model_logit$coef
model_logit$fit
as.numeric(model_logit$fit>0.5)


sum(c(1,as.numeric(SAheart[1,1:8]))*model_logit$coef)
exp(0.4972209)/(1+exp(0.4972209))
model_logit$fit[1]



summary(model_logit2)
str(model_logit2)
model_logit2
model_logit2$fit
as.numeric(model_logit2$fit>0.5)


#### test data
Test_saheart = SAheart[1:50,1:8]

## prediction
pred_logit= predict(model_logit,Test_saheart, type="response")
as.numeric(pred_logit>=0.5)
pred_logit2= predict(model_logit2,Test_saheart)


###### linear discriminant analaysis
model_lda = lda(chd~sbp+tobacco+ldl+adiposity+
typea+obesity+alcohol+age,data=SAheart)
model_lda
str(model_lda)
mean(SAheart$chd)
mean(SAheart$sbp[SAheart$chd==0])

# prediction
pred_lda = predict(model_lda,Test_saheart )


# quadratic discriminant analysis
model_qda = qda(chd~sbp+tobacco+ldl+adiposity+
typea+obesity+alcohol+age,data=SAheart)
model_qda

pred_qda = predict(model_qda,Test_saheart)


# comparison
cbind(pred_lda$class, pred_qda$class, pred_logit2,chd[1:50]+1)
table(pred_lda$class, chd[1:50])
table(pred_qda$class, chd[1:50])
table(pred_logit2, chd[1:50])



detach(SAheart)

##############  Data iris#######################################################

# read data
iris<-read.table('iris.txt',header=T)
attach(iris)


n= dim(iris)[1]
# randomly row-permutate data
iris2 = iris[sample(1:n),]

# linear discriminant analaysis

training = iris2[1:100,]
model_lda = lda( Species ~ SepalLength+ SepalWidth + 
    PetalLength+ PetalWidth,data= training)

# test data
Test = iris2[101:150,1:4]

# prediction
pred_lda = predict(model_lda,Test)


# quadratic discriminant analysis
model_qda = qda(Species ~ .,data=training)
pred_qda = predict(model_qda,Test)

# comparison
data.frame(pred_lda$class, pred_qda$class,iris2[101:150,5])

table(pred_lda$class, iris2[101:150,5])
table(pred_qda$class, iris2[101:150,5])

detach(iris)

library(MASS)

#############  Data SAheart#######################################################
# read data
SAheart<-read.table('SAheart.txt',header=T)
attach(SAheart)



####logistic regression
## model fitting
model_logit = glm(chd ~ . , family = binomial(link='logit'), data=SAheart)
require(nnet)
model_logit2 <- multinom(chd~.,data=SAheart)

## model outputs
summary(model_logit)
str(model_logit)
model_logit$coef
model_logit$fit
as.numeric(model_logit$fit>0.5)


sum(c(1,as.numeric(SAheart[1,1:8]))*model_logit$coef)
exp(0.4972209)/(1+exp(0.4972209))
model_logit$fit[1]



summary(model_logit2)
str(model_logit2)
model_logit2
model_logit2$fit
as.numeric(model_logit2$fit>0.5)


#### test data
Test_saheart = SAheart[1:50,1:8]

## prediction
pred_logit= predict(model_logit,Test_saheart, type="response")
as.numeric(pred_logit>=0.5)
pred_logit2= predict(model_logit2,Test_saheart)


###### linear discriminant analaysis
model_lda = lda(chd~sbp+tobacco+ldl+adiposity+
typea+obesity+alcohol+age,data=SAheart)
model_lda
str(model_lda)
mean(SAheart$chd)
mean(SAheart$sbp[SAheart$chd==0])

# prediction
pred_lda = predict(model_lda,Test_saheart )


# quadratic discriminant analysis
model_qda = qda(chd~sbp+tobacco+ldl+adiposity+
typea+obesity+alcohol+age,data=SAheart)
model_qda

pred_qda = predict(model_qda,Test_saheart)


# comparison
cbind(pred_lda$class, pred_qda$class, pred_logit2,chd[1:50]+1)
table(pred_lda$class, chd[1:50])
table(pred_qda$class, chd[1:50])
table(pred_logit2, chd[1:50])



detach(SAheart)

##############  Data iris#######################################################

# read data
iris<-read.table('iris.txt',header=T)
attach(iris)


n= dim(iris)[1]
# randomly row-permutate data
iris2 = iris[sample(1:n),]

# linear discriminant analaysis

training = iris2[1:100,]
model_lda = lda( Species ~ SepalLength+ SepalWidth + 
    PetalLength+ PetalWidth,data= training)

# test data
Test = iris2[101:150,1:4]

# prediction
pred_lda = predict(model_lda,Test)


# quadratic discriminant analysis
model_qda = qda(Species ~ .,data=training)
pred_qda = predict(model_qda,Test)

# comparison
data.frame(pred_lda$class, pred_qda$class,iris2[101:150,5])

table(pred_lda$class, iris2[101:150,5])
table(pred_qda$class, iris2[101:150,5])

detach(iris)


install.packages("leaps")
install.packages("lasso2")
library(leaps)
library(lasso2)
data(Prostate)
attach(Prostate)


####################################################
## Subset Selection
####################################################

### best subset selction 
# nbest: number of subsets of each size to record
leaps=regsubsets(lpsa~lcavol+lweight+age
      +lbph+svi+lcp+gleason+pgg45, data=Prostate,nbest=5)

# nvmax: maximum size of subsets to examine
leaps=regsubsets(lpsa~., data=Prostate,nvmax=10)

#### 
pdf("bestsubset_leaps.pdf")
par(mfrow=c(2,2))
plot(leaps,scale="r2")
plot(leaps,scale="adjr2")
plot(leaps,scale="Cp")
plot(leaps,scale="bic")
dev.off()

### 
summary(leaps)
reg.summary=summary(leaps)

### the residual sum of squares of the top 10 models
reg.summary$rss

### the R^2 of the top 10 models
reg.summary$rsq

### the adjusted R^2 of the top 10 models
reg.summary$adjr2

### the Cp of the top 10 models
reg.summary$cp



#  plot of RSS, adjusted R^2, Cp and BIC together
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Predictors", ylab="Residual Sum of Squares", type="l", xlim=c(0,11), ylim=c(min(reg.summary$rss), max(reg.summary$rss)))
points(which.min(reg.summary$rss), reg.summary$rss[which.min(reg.summary$rss)], cex=2, pch=20, col="red")

plot(reg.summary$cp, xlab="Number of Predictors", ylab="Cp", type="l", xlim=c(0,11), ylim=c(min(reg.summary$cp),max(reg.summary$cp)))
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], cex=2, pch=20, col="red")

plot(reg.summary$adjr2, xlab="Number of Predictors", ylab="Adjusted R Square", type="l", xlim=c(0,11), ylim=c(0,1))
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], cex=2, pch=20, col="red")

plot(reg.summary$bic, xlab="Number of Predictors", ylab="BIC", type="l", xlim=c(0,11))
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)], cex=2, pch=20, col="red")


### another way of producing C_p plot
subset<-leaps(x=Prostate[,1:8],y=Prostate[,9])
plot(x=subset$size,y=subset$Cp,xlab='size',ylab='Cp')



### full model
fit1<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,
  data=Prostate)
fit1<-lm(lpsa~ .,data=Prostate)

### null model
fit0<-lm(lpsa~1,data=Prostate)

### forward selection
fit.forward<-step(fit0,scope=list(lower=lpsa~1, upper=fit1),direction='forward')
summary(fit.forward)

### backward selection
fit.backward<-step(fit1,scope=list(lower=lpsa~1, upper=fit1),direction='backward')
summary(fit.backward)  

### stepwise regression by AIC criterion
fit.both<-step(fit0,scope=list(lower=lpsa~1, upper=fit1),direction='both')
summary(fit.both)  


####################################################
## Ridge Regression
####################################################
          
library(MASS)  

# in the presence of multi-collinearity 

x1  = rnorm(30)
x2  = rnorm(30,mean=x1,sd=.01)
y   = rnorm(30,mean=5+x1+x2)
lm(y~x1+x2)$coef
lm.ridge(y~x1+x2,lambda=1)

# Prostate example
prostate = scale(Prostate)
prostate = as.data.frame(prostate)
fit.ridge<-lm.ridge(lpsa~lcavol+lweight+age
         +lbph+svi+lcp+gleason+pgg45,
         data=prostate, lambda=seq(0,20,0.1))  
plot(fit.ridge)  
plot(seq(0,20,0.1),fit.ridge$GCV,xlab= expression(lambda),ylab="GCV")

select(fit.ridge)
round(fit.ridge$coef[, which(fit.ridge$lambda == 6.5)], 2)    
fit.ridge<-lm.ridge(lpsa~lcavol+lweight+age  
         +lbph+svi+lcp+gleason+pgg45,   
         data=prostate, lambda=6.5)   
fit.ridge$coef     



####################################################
## Lasso
####################################################
install.packages("glmnet")
library(glmnet)
X = as.matrix(Prostate[,1:8])
y = Prostate$lpsa

fit = glmnet(X,y)
plot(fit)
cvfit = cv.glmnet(X,y)
plot(cvfit)
coef(fit,s=cvfit$lambda.min)
min(cvfit$cvm)



 


##lars
install.packages("lars")
library(lars)
fit.lars = lars(X,y, type="lasso",trace=TRUE)
plot(fit.lars)
cv.fit.lars = cv.lars(X,y,mode="step")
cbind(cv.fit.lars$index,cv.fit.lars$cv)
bestindex = cv.fit.lars$index[which.min(cv.fit.lars$cv)]
which.min(cv.fit.lars$cv)
fit.lars$beta
bestindex
fit.lars$lambda
fit.lars$beta[bestindex,]


cv.fit.lars.f = cv.lars(X,y,mode="fraction")
which.min(cv.fit.lars.f$cv)
bestindex = cv.fit.lars.f$index[which.min(cv.fit.lars.f$cv)]
bestindex
predict.lars(fit.lars,s=bestindex,mode="fraction",type="coefficients") 
p.lars = predict.lars(fit.lars,s=bestindex,mode="fraction",type="coefficients")
p.lars$coefficient 


install.packages("glmnetcr")
library(glmnetcr)

glmnet.fit = glmnet.cr(X, y)
AIC = select.glmnet.cr(glmnet.fit, which = "AIC")
AIC
nonzero.glmnet.cr(glmnet.fit, s = AIC)

BIC = select.glmnet.cr(glmnet.fit, which = "BIC")
BIC
nonzero.glmnet.cr(glmnet.fit, s = BIC)



detach(Prostate)


## read data
bone = read.table("bone.txt",header=T)
attach(bone)


### plot of the data
install.packages("lattice")
library(lattice)
xyplot(spnbmd~age|gender,bone)

## install package
install.packages("splines")
library(splines)

# adding lines in a panel
myPanel <- function(...)
  {
    panel.smooth(...)
    panel.xyplot(...,alpha=.5)
  }


############################################
## fitting piecewise constant model
panel.smooth <- function(x,y,...)
  {
    knots=quantile(x,c(1/3,2/3))
    c1 <- mean(y[x < knots[1]])
    c2 <- mean(y[x >= knots[1] & x < knots[2]])
    c3 <- mean(y[x >= knots[2]])
    x1 <- seq(min(x),knots[1],len=33)
    x2 <- seq(knots[1],knots[2],len=33)
    x3 <- seq(knots[2],max(x),len=33)
    llines(x1,c1,col="black")
    lsegments(x0=knots[1],y0=min(y),x1=knots[1],y1=max(y),col="black",lty=2)
    llines(x2,c2,col="black")
    lsegments(x0=knots[2],y0=min(y),x1=knots[2],y1=max(y),col="black",lty=2)
    llines(x3,c3,col="black")
  }
trellis.par.set(plot.symbol=list(pch=19),plot.line=list(lwd=2))
pdf("fig3_bone_piecewiseconstant.pdf")
xyplot(spnbmd~age|gender,bone,panel=myPanel)
dev.off()


# choose knots for female
x = age[gender=="female"]
    knots=quantile(x,c(1/3,2/3))

############################################
## fitting piecewise linear model
panel.smooth <- function(x,y,...)
  {
    knots=quantile(x,c(1/3,2/3))
    ind1 <- x < knots[1]
    ind2 <- x >= knots[1] & x < knots[2]
    ind3 <- x >= knots[2]
    x1 <- x[ind1]
    x2 <- x[ind2]
    x3 <- x[ind3]
    fit1 <- lm(y[ind1]~x1)
    fit2 <- lm(y[ind2]~x2)
    fit3 <- lm(y[ind3]~x3)
    x1 <- seq(min(x),knots[1],len=33)
    x2 <- seq(knots[1],knots[2],len=33)
    x3 <- seq(knots[2],max(x),len=33)
    llines(x1,predict(fit1,data.frame(x1=x1)),col="black")
    lsegments(x0=knots[1],y0=min(y),x1=knots[1],y1=max(y),col="black",lty=2)
    llines(x2,predict(fit2,data.frame(x2=x2)),col="black")
    lsegments(x0=knots[2],y0=min(y),x1=knots[2],y1=max(y),col="black",lty=2)
    llines(x3,predict(fit3,data.frame(x3=x3)),col="black")
  }

pdf("fig3_bone_piecewiselinear.pdf")
xyplot(spnbmd~age|gender,bone,panel=myPanel)
dev.off()

############################################
## fitting continous piecewise linear model
panel.smooth <- function(x,y,...)
  {
    knots=quantile(x,c(1/3,2/3))
    fit <- lm(y~bs(x,knots=knots,degree=1))
    x0 <- seq(min(x),max(x),len=101)
    llines(x0,predict(fit,data.frame(x=x0)),col="black")
    lsegments(x0=knots[1],y0=min(y),x1=knots[1],y1=max(y),col="black",lty=2)
    lsegments(x0=knots[2],y0=min(y),x1=knots[2],y1=max(y),col="black",lty=2)
  }
pdf("fig3_bone_continuouspiecewiselinear.pdf")
xyplot(spnbmd~age|gender,bone,panel=myPanel)
dev.off()

############################################
## fitting qudractic spline
panel.smooth <- function(x,y,...)
  {
    knots=quantile(x,c(1/3,2/3))
    fit <- lm(y~bs(x,knots=knots,degree=2))
    x0 <- seq(min(x),max(x),len=101)
    llines(x0,predict(fit,data.frame(x=x0)),col="black")
    lsegments(x0=knots[1],y0=min(y),x1=knots[1],y1=max(y),col="black",lty=2)
    lsegments(x0=knots[2],y0=min(y),x1=knots[2],y1=max(y),col="black",lty=2)
  }
xyplot(spnbmd~age|gender,bone,panel=myPanel)


################################################
## fitting cubic spline
panel.smooth <- function(x,y,...)
  {
    knots=quantile(x,c(1/3,2/3))
    fit = lm(y~bs(x,knots=knots,degree=3))
    x0 = seq(min(x),max(x),len=101)
    llines(x0,predict(fit,data.frame(x=x0)),col="black")
    lsegments(x0=knots[1],y0=min(y),x1=knots[1],y1=max(y),col="black",lty=2)
    lsegments(x0=knots[2],y0=min(y),x1=knots[2],y1=max(y),col="black",lty=2)
  }
xyplot(spnbmd~age|gender,bone,panel=myPanel)

################################################
##fitting natural spline
panel.smooth <- function(x,y,...)
  {
    knots=quantile(x,c(1/3,2/3))
    fit = lm(y~ns(x,knots=knots))
    x0 = seq(min(x),max(x),len=101)
    llines(x0,predict(fit,data.frame(x=x0)),col="black")
    lsegments(x0=min(x),y0=min(y),x1=min(x),y1=max(y),col="black",lty=2)
    lsegments(x0=knots[1],y0=min(y),x1=knots[1],y1=max(y),col="black",lty=2)
    lsegments(x0=knots[2],y0=min(y),x1=knots[2],y1=max(y),col="black",lty=2)
    lsegments(x0=max(x),y0=min(y),x1=max(x),y1=max(y),col="black",lty=2)
  }
xyplot(spnbmd~age|gender,bone,panel=myPanel)

################################################
## fitting natural spline with df = 5
panel.smooth <- function(x,y,...)
  {
    fit = lm(y~ns(x,df=5))
    x0 = seq(min(x),max(x),len=101)
    llines(x0,predict(fit,data.frame(x=x0)),col="black",lwd=3)
  }
xyplot(spnbmd~age|gender,bone,panel=myPanel)

# choose knots for female
x = age[gender=="female"]
ns(x,df=5)

## mean and variace of the estimate
panel.smooth <- function(x,y,...)
  {
    fit = lm(y~ns(x,df=5))
    x0 = seq(min(x),max(x),len=101)
    y0 = predict(fit,newdata=data.frame(x=x0),se.fit=T)
    lpolygon(c(x0,rev(x0)),c(y0$fit-2*y0$se.fit,rev(y0$fit+2*y0$se.fit)),col="gray",border=F,...)
    llines(x0,y0$fit,col="black")
  }
xyplot(spnbmd~age|gender,bone,panel=myPanel)

 

################################################
##### smoothing spline
################################################

plot(spnbmd ~ age, data=bone, col =
ifelse(gender=="male", "blue", "red2"),
xlab="Age", ylab="Relative Change in Spinal BMD")
bone.spline.male <- with(subset(bone,gender=="male"),
smooth.spline(age, spnbmd,df=12))
bone.spline.female <- with(subset(bone, gender=="female"),
smooth.spline(age, spnbmd,df=12))
lines(bone.spline.male, col="blue")
lines(bone.spline.female, col="red2")
legend(20,0.20,legend=c("male", "Female"),
col=c("blue", "red2"),lwd=2)



# confidence interval for smoothing spline
# second way
require(mgcv)
myPanel <- function(...)
  {
    panel.xyplot(...,alpha=.3)
    panel.smooth(...)
  }
panel.smooth <- function(x,y,...)
  {
    fit <- gam(y~s(x))
    xx <- seq(min(x),max(x),len=101)
    yy <- predict(fit,newdata=data.frame(x=xx),se.fit=T)
    lpolygon(c(xx,rev(xx)),c(yy$fit-1.96*yy$se.fit,rev(yy$fit+1.96*yy$se.fit)),col=rgb(.6,.6,.6,alpha=.4),border=F,...)
    llines(xx,yy$fit,col="black",lwd=2)
  }
trellis.par.set(plot.symbol=list(pch=19))
xyplot(spnbmd~age|gender,bone,panel=myPanel)

####################################################
# The following code is for section 5.1
####################################################

# simulate random variables
set.seed(999)
n<-20
rnorm(n,10,3) # normal distribution
runif(n,2,3) # uniform distribution between 2 and 3
rbeta(n,2,5) # beta distribution 

hist(rnorm(1000,10,4))
hist(runif(1000,2,3))
hist(rbeta(1000,2,5))
mean(rbeta(10000,2,5))
sd(rnorm(10000,10,4))


####################################################
# The following code is for Section 5.2
####################################################

# population and sample
set.seed(1000)
N<-10000
X<-rnorm(N,10,3)
mean(X)    # the population mean
var(X)    # the population variance
n<-500
x<-sample(X,n,replace=F)
mean(x) # the sample  mean
var(x) # the sample variance

# sample mean and sample variance
fsample<-function(mu,sigma,n,nsim)
{ xbar<-rep(0,nsim);xvar<-rep(0,nsim)
   for(i in 1:nsim)
   {set.seed(i)
    # x contains a random sample of size n of the variable X
    x<-rnorm(n,mu,sigma)
    xbar[i]<-mean(x)
    xvar[i]<-var(x)
   }
   cat('sample mean:',mean(xbar),"\n")
   cat('sample variance:',mean(xvar),"\n")
   # plot the samples
   par(mfrow=c(2,2))  
   hist(xbar);qqnorm(xbar);qqline(xbar);
   hist(xvar);qqnorm(xvar);qqline(xvar);
}

# sample mean and sample variance of samples of size 10 
# from a normal distribution with mean 2 and sigma 2
fsample(2,2,10,10)

# increase simulation times to 100, 1000, 10000
fsample(2,2,10,100)
fsample(2,2,10,1000)
fsample(2,2,10,10000)

# change sample size
mu<-2;sigma<-2;n<-10;nsim<-1000
xbar<-rep(0,nsim);xbar2<-rep(0,nsim);xbar3<-rep(0,nsim)
xvar<-rep(0,nsim);xvar2<-rep(0,nsim);xvar3<-rep(0,nsim)
for(i in 1:nsim)
{set.seed(i)
x<-rnorm(n,mu,sigma);x2<-rnorm(n*10,mu,sigma)
x3<-rnorm(n*100,mu,sigma)
xbar[i]<-mean(x);xbar2[i]<-mean(x2);xbar3[i]<-mean(x3)
xvar[i]<-var(x);xvar2[i]<-var(x2);xvar3[i]<-var(x3)}
c(mean(xbar),mean(xbar2),mean(xbar3))
c(var(xbar),var(xbar2),var(xbar3))
c(mean(xvar),mean(xvar2),mean(xvar3))

# histogram and QQ-plot for xbar
par(mfcol=c(3,2))
hist(xbar);hist(xbar2);hist(xbar3)
qqnorm(xbar);qqline(xbar)
qqnorm(xbar2);qqline(xbar2)
qqnorm(xbar3);qqline(xbar3)


# histogram and QQ-plot for xvar
par(mfcol=c(3,2))
hist(xvar);hist(xvar2);hist(xvar3)
qqnorm(xvar);qqline(xvar)
qqnorm(xvar2);qqline(xvar2)
qqnorm(xvar3);qqline(xvar3)

# 95% confidence interval
alpha<-0.05
c(mean(x)-qt(1-alpha/2,n-1)*sd(x),mean(x)+qt(1-alpha/2,n-1)*sd(x))


# now repeat the above simulation for beta distribuiton 
alpha<-2;beta<-4;n<-10;nsim<-1000
xbar<-rep(NA,nsim);xbar2<-rep(NA,nsim)
xbar3<-rep(NA,nsim)
xvar<-rep(NA,nsim);xvar2<-rep(NA,nsim)
xvar3<-rep(NA,nsim)
for(i in 1:nsim)
{set.seed(i)
x<-rbeta(n,alpha,beta);x2<-rbeta(n*10,alpha,beta)
x3<-rbeta(n*100,alpha,beta)
xbar[i]<-mean(x);xbar2[i]<-mean(x2);xbar3[i]<-mean(x3)
xvar[i]<-var(x);xvar2[i]<-var(x2);xvar3[i]<-var(x3)}
c(mean(xbar),mean(xbar2),mean(xbar3))
c(var(xbar),var(xbar2),var(xbar3))
c(mean(xvar),mean(xvar2),mean(xvar3))

# histogram and QQ-plot for xbar
par(mfcol=c(3,2))
hist(xbar);hist(xbar2);hist(xbar3)
qqnorm(xbar);qqline(xbar)
qqnorm(xbar2);qqline(xbar2)
qqnorm(xbar3);qqline(xbar3)

# histogram and QQ-plot for xvar
par(mfcol=c(3,2))
hist(xvar);hist(xvar2);hist(xvar3)
qqnorm(xvar);qqline(xvar)
qqnorm(xvar2);qqline(xvar2)
qqnorm(xvar3);qqline(xvar3)



# simulated value of the mean squared error of an estimator
# try it for n=10,50, 100  
fMSE<-function(mu,sigma,n,nsim)
{xbar<-rep(NA,nsim)
for(i in 1:nsim)
{set.seed(i)
# x contains a random sample of size n of the variable X
x<-rnorm(n,mu,sigma);xbar[i]<-mean(x)};mse<-mean((xbar-mu)^2)
xvar<-var(xbar);bias<-mean(xbar)-mu
cat('case n=',n, ': mse=', mse, ', var=', xvar, ', bias=', bias,  "\n")}
fMSE(2,2,10,1000)
fMSE(2,2,50,1000)
fMSE(2,2,100,1000)


################### A case study
# function to generate data
generateData <- function(n,p,beta)
  {
    X <- matrix(runif(n*p),n,p)
    y <- rnorm(n,X%*%beta)
    return(list(X=X,y=y))
  }

# best subset selection given data
 bestsubset <- function(Data)
  {
    require(leaps)
    fit = regsubsets(y~X,Data)
    b = numeric(ncol(Data$X)+1)
    names(b)  =  fit$xnames
    bb = coef(fit,which.min(summary(fit)[["cp"]]))
    b[names(bb)] = bb
    return(b)
  }

# ridge regression
ridge <- function(Data,n.lam=501)
  {
    require(MASS)
    lam  =  c(0,exp(seq(0,20,len=n.lam)))
    fit = lm.ridge(y~X,Data,lambda=lam)
    b = coef(fit)[which.min(fit$GCV),]
    return(b)
  }

# lasso variable selection
lasso <- function(Data)
  {
    require(glmnet)
    cvfit = cv.glmnet(Data$X,Data$y)
    b = as.numeric(coef(cvfit,s=cvfit$lambda.min))
    return(b)
  }

# the main body of simulation
N = 1000
n = 50
beta = c(3,-3,rep(0,8))
p = length(beta)
out = array(NA,dim=c(N,p,4),
                 dimnames=list(1:N,1:p,c("Subset","Lasso","Ridge","OLS")))
for (i in 1:N)
  {
    Data = generateData(n,p,beta)
    out[i,,1] = bestsubset(Data)[-1]
    out[i,,2] = lasso(Data)[-1]
    out[i,,3] = ridge(Data)[-1]
    out[i,,4] = coef(lm(y~X,Data))[-1]
  }
bias <- apply(out,2:3,mean)-beta
variance <- apply(out,2:3,var)
MSE <- bias^2+variance
apply(MSE,2,sum)

par(mfrow=c(2,2))
ylim = range(out)
boxplot(out[,,1],col="blue",ylim=ylim,main="Subset")
boxplot(out[,,2],col="blue",ylim=ylim,main="Lasso")
boxplot(out[,,3],col="blue",ylim=ylim,main="Ridge")
boxplot(out[,,4],col="blue",ylim=ylim,main="OLS")

