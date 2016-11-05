
Run the following code to create a data set of returns on two stocks, DATGEN and DEC. library("fEcofin") 
library(mnormt) 
Berndt = berndtlnvest[,5:6] 
names(Berndt) 

Problem 1 Fit a multivariate-t model to Berndt. What are the estimates of the mean vector, DF, and scale matrix?  Include your R program with your work. Include your R code and output with your work. 

Problem 2 What is the distribution of the return on a $100,000 portfolio that is 30% invested in DATGEN and 70% invested in DEC? Include your R code and output with your work. Find 𝑉𝑉𝑅0.05 and 𝐸𝑆0.05 for this portfolio.

Problem 3 Use the model-free bootstrap to find a basic percentile bootstrap confidence interval for 𝑉𝑉𝑅0.05 for this portfolio. Use a 90% confidence coefficient for the confidence interval. Use 250 bootstrap resamples. This amount of resampling is not enough for a highly accurate confidence interval, but will give a reasonably good indication of the uncertainty in the estimate of VaR(0.05), which is all that is really needed. 
 
Problem 4 This problem uses the variable DEC. Estimate the left tail index using the Hill estimator.  Use a Hill plot to select 𝑛𝑐.  What is your choice of 𝑛𝑐? Include your R code and plot with  your work.

```r
library("fEcofin")
library(mnormt)
Berndt = berndtinvest[,5:6] 
Y = as.matrix(Berndt)
loglik = function(par)
{
mu = par[1:2]
A= matrix(c(par[3],par[4],0,par[5]),nrow=2,byrow=T)
scale_matrix = t(A)%*%A
df = par[6]
f = -sum(log(dmt(Y, mean=mu,S=scale_matrix,df=df)))
f
} 
A=chol(cov(Y)) 
start=as.vector(c(apply(Y,2,mean),A[1,1],A[1,2],A[2,2],4))
fit_mvt = optim(start,loglik,method="L-BFGS-B",lower=c(-.2,-.2,-.3,-.3,-.3,2.2), upper=c(.2,.2,.3,.3,.3,15),hessian=T)
muhat = fit_mvt$par[1:2]
dfhat = fit_mvt$par[6]
A= matrix(c(fit_mvt$par[3],fit_mvt$par[4],0,fit_mvt$par[5]), nrow=2,byrow=T)
scaleMatrixhat = t(A)%*%A
options(digits=5)
muhat 
dfhat 
scaleMatrixhat


alpha = .05
w =  c ( 0.3, 0.7)
muP = w %*% muhat
scaleP = sqrt(w %*% scaleMatrixhat %*% w)
VaR05 =100000*( -muP + scaleP*qt(1-alpha,dfhat))
ES05 = 100000*(-muP + scaleP* dt(qt(alpha,dfhat),dfhat)/alpha*(dfhat + qt(alpha,dfhat)"2)/(dfhat-1) )?
muP 
scaleP 
VaR05
ES05

n=dim(Y[1])
w= c(.3,.7) 
Bboot = 100
VaR05boot = rep(0,Bboot) 
Dfhatboot = rep(0,Bboot) 
Y = as.matrix(Berndt)
for (iboot in 1:Bboot)
{
Ind = sample(n,replace=T) 
Yboot =Y[ind,]
Loglik = function(par)
{
mu = par[1:2]
A = matrix(c(par[3],par[4],0,par[5]),nrow= 2,byrow = T)
scale_matrix = t(A)%*%A
df = par[6]
f = -sum(log(dmt(Yboot, mean=mu,S=scale_matrix,df=df)))
f
}
A = chol(cov(Yboot))
start = as.vector(c(apply(Yboot,2,mean),A[1,1],A[1,2],A[2,2],4))
fit_mvt = optim(start,loglik,method="L-BFGS-B",lower=c(-.02,-.02,-.1,-.1,-.1,2), upper = c(.02,.02,.1,.1,.1,15),hessian=T)
muhat=fit_mvt$par[1:2]
dfhatboot[iboot]=fit_mvt$par[6] 
A=matrix(c(fit_mvt$par[3],fit_mvt$par[4],0,fit_mvt$par[5]),nrow=2,byrow=T) scaleMatrixhat t(A)%*%A
?
muP=w %*% muhat
scaleP = sqrt(w %*% scaleMatrixhat %*% w) 
VaR05boot[iboot]=100000*(-muP + scaleP*qt(.95,dfhat))
}
quantile(VaR05boot,c(.05,.95)) 
par(mfrow=c(1,2)) 
plot(density(dfhatboot),main="DF") 
plot(density(VaR05boot),main="VaR05")
```



Consider daily BMW returns in the bmwRet data set in the fEcofin package.  Assume that the returns are i.i.d., even though there may be some autocorrelation and volatility clustering is likely. Suppose that $1000 is invested in BMW stock.)   

Problem 5. Compute nonparametric estimates ofVaR(0.01, 24 hours) and ES(0.01,24 hours). 

Problem 6 Compute parametric estimates of VaR(0.01, 24 hours)  and  ES(0.01,24 hours) assuming  that the returns are normally distributed. 

Problem 7 Compute parametric estimates of VaR(0.01, 24 hours)  and  ES(0.01,24 hours)  assuming  that the  returns are t-distributed. 

Problem 8 Compare  the  estimates in  6,7,8. Which do you feel are most realistic? 

```r
> library(fEcofin) 
> data(bmwRet) 
> r = bmwRet[,2) 
> varNonp =  -1000*quantile(r,.01) 
> ind =  (-1000*r > var) 
> esNonp =  -1000*sum(r[ind])/ sum(ind) 
> varNonp 
1% 
40.798 
> esNonp 
[1] 56.492 
> varNormal = -1000*qnorm(.01,mean=mean(r),sd=sd(r)) 
> ESNormal = 1000*( -mean(r) + sd(r)*dnorm(qnorm(.01))/.01 
> varNormal 
[1]33.986 
> ESNormal 
[1]38.986
> loglik = function(theta){ + x =  (r-theta[1])/theta[2] + - sum(log(dt(x,theta[3])/theta[2])) + } 
> theta0 = c(mean(r),sd(r),5) 
> fit = optim(theta0,loglik) 
> theta = fit$par 
> theta [1]0.00013196 0.00926451 2.98858766 
> alpha = .01 
> VaRt =1000*(-theta[1] + theta[2]*qt(1-alpha,theta[3])) 
>ESt= 1000*(-theta[1] + theta[2]* dt(qt(alpha,theta[3]),theta[3])/alpha* + (theta[3] + qt(alpha,theta[3])-2)/(theta[3]-1)) 
> VaRt 
[1]42.064 
> ESt 
[1]65.058

```

The three sets  of estimated risk measures  are  below.  
 
  
To see which might be most realistic, a normal plot of the returns is shown below.   The  returns have much heavier  tails  than  a  normal distribution, so the  risk  measures  based  on  normality arc  not realistic. There are 6146 returns, so the nonparametric estimates are reasonable, especially VaR.  However, even 6146 returns might not be enough to estimate the tails accurately using nonparametric estimators, so I would prefer the t-based estimates. We can compare the nonparametric and t-based estimators of VaR and ES by resampling. Both model-based (assuming t-distributed returns) and model-free resan1pling could be used. It would be interesting to compare the two types  of estimators using bias, standard deviation, and  mean squared error. 

