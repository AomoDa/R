---
title: "q4"
author: "Your Nmae"
date: "2016-12-07"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

##EDA and Method of Moments

If the original data is from  exponential distribution, the following conclusions can be drawn from the moment estimator.
$$E(X)= \frac{1}{ \lambda}$$
thus 
$$\lambda = \frac{1}{E(X)}=0.3798734$$.


If the original data is from  gamma distribution, the following conclusions can be drawn from the moment estimator.

$$\alpha = \frac{E(X)^2}{S^2}=1.269429$$
$$\beta=\frac{S^2}{E(X)^2}=0.7877559$$

```{r, echo=FALSE}
BusData<- scan('C://Users//mali//Documents//BusData.txt')
hist(BusData,breaks = 10,probability = T)
lines(density(BusData,from = 0),col='red')
lambda <- 1/mean(BusData)
alpha <- mean(BusData)^2 / var(BusData)
beta <-  var(BusData) / mean(BusData)^2
```

##Compare and Conclusion

- 从exponential distribution和gamma distribution 的Q-Q图来说，BusData 均匀的分布在Theoretical distribution的周围，虽然有个别异常值，但不影响总体趋势。从图像上来说，BusData 似乎是更符合exponential distribution。
- 绘制exponential distribution和gamma distribution 的核密度图，同时绘制了BusData的核密度图。从核密度图的分布趋势来说，BusData更倾向于exponential distribution。
- 综合考虑，我们认为BusData has exponential distribution with $\lambda=0.38$
- If I am told that the arrival of buses follow a Poisson process, BusData将会趋向于服从poisson distribution.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
##Q-Q
par(mfrow=c(1,2))
set.seed(12345)
## Q-Q plot for exp data against true theoretical distribution:
qqplot(rexp(ppoints(1e3),rate = lambda), BusData,
	main='Q-Q plot for  \n Exponential Distribution',xlab='Theoretical Quantiles' )
qqline(BusData, distribution = function(p) qexp(p, rate = lambda), col = 2)
## Q-Q plot for gamma data against true theoretical distribution:
qqplot(rgamma(ppoints(1e3),shape = alpha,scale = beta), BusData,
	main='Q-Q plot for \n   Gamma Distribution',xlab='Theoretical Quantiles' )
qqline(BusData, distribution = function(p) qgamma(p, shape = alpha,scale = beta), col = 2)
par(mfrow=c(1,1))

## sample
set.seed(100)
a <- seq(0,15,by = 0.1)
plot(density(BusData,from=0),col='orange',ylim=c(0,0.4),main='compare sample distribution ')
points(a,dexp(x =  a,rate = lambda),type='l',col='red',lty=2)
points(a,dgamma(x =  a,shape = alpha,scale = beta),type='l',col='blue',lty=3)
legend('topright',col=c('orange','red','blue'),lty=1:3,legend=c('BusData','exponential','gamma'))
```

##Appendix R Code

```{r, eval=FALSE, include=T}
BusData<- scan('BusData.txt')
hist(BusData,breaks = 10,probability = T)
lines(density(BusData),col='red')

lambda <- 1/mean(BusData)
alpha <- mean(BusData)^2 / var(BusData)
beta <-  var(BusData) / mean(BusData)^2


##Q-Q
par(mfrow=c(1,2))
set.seed(12345)
## Q-Q plot for exp data against true theoretical distribution:
qqplot(rexp(ppoints(1e3),rate = lambda), BusData,
	main='Q-Q plot for  \n Exponential Distribution',xlab='Theoretical Quantiles' )
qqline(BusData, distribution = function(p) qexp(p, rate = lambda), col = 2)
## Q-Q plot for gamma data against true theoretical distribution:
qqplot(rgamma(ppoints(1e3),shape = alpha,scale = beta), BusData,
	main='Q-Q plot for \n   Gamma Distribution',xlab='Theoretical Quantiles' )
qqline(BusData, distribution = function(p) qgamma(p, shape = alpha,scale = beta), col = 2)
par(mfrow=c(1,1))

## sample
set.seed(100)
a <- seq(0,15,by = 0.1)
plot(density(BusData,from=0),col='orange',ylim=c(0,0.4),main='compare sample distribution ')
points(a,dexp(x =  a,rate = lambda),type='l',col='red',lty=2)
points(a,dgamma(x =  a,shape = alpha,scale = beta),type='l',col='blue',lty=3)
legend('topright',col=c('orange','red','blue'),lty=1:3,legend=c('BusData','exponential','gamma'))
```

