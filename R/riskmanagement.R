
setwd('F:/Ranalysis/')

library(fEcofin)
library(mnormt)
library(boot)
library(PerformanceAnalytics)  
library(MASS)


berndtinvest<-read.csv('berndtinvest.csv',header=F,stringsAsFactors = FALSE,sep = ';')
options(digits=5)
Berndt<-berndtinvest[,5:6]
a<-cov(Berndt)
sd(pmt(Berndt,mean=rep(0,1),S=a))
cov(Berndt)



#Problem 1 

Y = as.matrix(Berndt)
loglik = function(par){
  A= matrix(c(par[3],par[4],0,par[5]),nrow=2,byrow=T)
  f =-sum(log(dmt(Y, mean=par[1:2],S=t(A)%*%A,df=par[6])))
  return(f)
} 
A=chol(cov(Y)) 
start=as.vector(c(apply(Y,2,mean),A[1,1],A[1,2],A[2,2],4))

fit_mvt = optim(start,loglik,method="L-BFGS-B",lower=c(-.2,-.2,-.3,-.3,-.3,2.2), upper=c(.2,.2,.3,.3,.3,15),hessian=T)

A= matrix(c(fit_mvt$par[3],fit_mvt$par[4],0,fit_mvt$par[5]), nrow=2,byrow=T)
muhat = fit_mvt$par[1:2]
DF = fit_mvt$par[6]
scaleMatrixhat = t(A)%*%A

muhat
DF
scaleMatrixhat


#------------------------------------------
library(QRM)
fit <- fit.mst(Berndt, method = "L-BFGS-B")
(mu <- fit$mu) # estimated location vector (muhat)
(sigma <- as.matrix(fit$Sigma)) # estimated scale matrix (scaleMatrixhat)
(nu <- fit$df) # estimated degrees of freedom(DF)
#-----------------------------------------



#  Problem 2
alpha = .05
w =c( 0.3, 0.7)
muP=w %*% muhat
dfhat=DF
scaleP=sqrt(w %*% scaleMatrixhat %*% w)

VaR=100000*(-muP+scaleP*qt(1-alpha,dfhat))
ES= 100000*(-muP+scaleP*dt(qt(1-alpha,dfhat),dfhat)/alpha*(dfhat +qt(1-alpha,dfhat)^2)/(dfhat-1) )
result<-data.frame(muP,scaleP,VaR,ES)
result

#-----------------------------------------

VaR()
#-----------------------------------------


#  Problem 3
n=dim(Y[1])
w= c(.3,.7) 
Bboot =250
VaR05boot = rep(0,Bboot)
Dfhatboot = rep(0,Bboot) 
Y = as.matrix(Berndt)
for(iboot in 1:Bboot){
  ind= sample(n,replace=T)
  Yboot =Y[ind,]
  Loglik = function(par){
    mu = par[1:2]
    A = matrix(c(par[3],par[4],0,par[5]),nrow= 2,byrow = T)
    scale_matrix = t(A)%*%A
    df = par[6]
    f = -sum(log(dmt(Yboot, mean=mu,S=scale_matrix,df=df)))
    f
  }
  A=chol(cov(Y))
  start = as.vector(c(apply(Y,2,mean),A[1,1],A[1,2],A[2,2],4))
  fit_mvt = optim(start,loglik,method="L-BFGS-B",lower=c(-.2,-.2,-.3,-.3,-.3,2.2), upper=c(.2,.2,.3,.3,.3,15),hessian=T)
  muhat=fit_mvt$par[1:2]
  Dfhatboot[iboot]=fit_mvt$par[6] 
  A=matrix(c(fit_mvt$par[3],fit_mvt$par[4],0,fit_mvt$par[5]),nrow=2,byrow=T) 
  scaleMatrixhat=t(A)%*%A
  muP=w %*% muhat
  scaleP = sqrt(w %*% scaleMatrixhat %*% w) 
  VaR05boot[iboot]=100000*(-muP+scaleP*qt(.95,dfhat))
}
quantile(VaR05boot,c(.05,.95)) 
par(mfrow=c(1,2)) 
plot(density(Dfhatboot),main="DF") 
plot(density(VaR05boot),main="VaR05")





##Q2


data("bmwRet")
r<-bmwRet[,2]



# Problem 5.
VaR<-1000*VaR(r,p=0.99,method='historical',invert=FALSE) 
ES=1000*ES(r,p=0.99,method='historical',invert=FALSE)
result5<-c(VaR,ES)
result5


# Problem 6 
VaR<-1000*VaR(r,p=0.99,method='gaussian',invert=FALSE)
ES<-1000*ES(r,p=0.99,method='gaussian',invert=FALSE)
result6<-c(VaR,ES)
result6


# Problem 7
loglik = function(theta){
  x=(r-theta[1])/theta[2]
  f=-sum(log(dt(x,theta[3])/theta[2]))
  f
  } 
theta0 = c(mean(r),sd(r),5) 
fit = optim(theta0,loglik) 
theta = fit$par 
alpha = .01 
VaRt =1000*(-theta[1] + theta[2]*qt(1-alpha,theta[3])) 
ESt= 1000*(-theta[1] + theta[2]*dt(qt(1-alpha,theta[3]),theta[3])/alpha*(theta[3]+qt(1-alpha,theta[3])^2)/(theta[3]-1)) 
result7<-c(VaRt,ESt) 


Result<-t(data.frame(result5,result6,result7))
Result

