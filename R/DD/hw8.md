---
title: "hw8"
author: "Your Nmae"
date: "2016-11-15"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q58

-----

$H_0:$ the variables X and Y are independent.


$H_1:$ the variables X and Y are not independent.

the pvalue of permutation test is 0.789,which indicated we can not reject $H_0$ and the variables X and Y are independent.

-----

```{r}
load('C://Users//mali//Documents//hw8.RData')
chisq_value <- function(tb){
  ex <- outer(rowSums(tb),colSums(tb))/sum(tb)
  return(sum( (tb-ex)^2/ex))
}
N <- 1e4-1
chq <- numeric(N)
set.seed(58)
for (i in 1:N) {
 chq[i] <- chisq_value(table(Problem59$X,sample(Problem59$Y)))
}
chisq_value(table(Problem59$X,Problem59$Y))
pvale <- (sum(chq >=72)+1)/(N+1)
pvale
hist(chq,main='chisq values of permutation test')
abline(v=72,col='red',lwd=2)
```

#Q59

##a

```{r}
Bangladesh <- read.csv('C://Users//mali//Documents//Bangladesh.csv')
par(mfrow=c(1,2))
boxplot(Bangladesh$Arsenic,main='boxplot of Arsenic')
hist(Bangladesh$Arsenic)
par(mfrow=c(1,1))
```

##b

```{r}
bootstrap_mean <- replicate(10000,mean(sample(Bangladesh$Arsenic,nrow(Bangladesh),replace=T)))
par(mfrow=c(1,2))
boxplot(bootstrap_mean,main='boxplot of bootstrap_mean')
hist(bootstrap_mean)
par(mfrow=c(1,1))

```

#Q65

使用mle 计算得出$\lambda=1.87$ for a sample from exponential distribution with size=5 and $\lambda=2$.

```{r}
t <- seq(0.01,10,by = .01)
log.mylikeli.exp <- function(lambda,x){ length(x)*log(lambda) - lambda*sum(x) }
set.seed(6565)
#Make a randomsample of size 5 from an exponential distribution with lambda = 2.
x1 <- rexp(5,rate = 2)
y.2 <- sapply(t,function(t){log.mylikeli.exp(t,x1)})
plot(t,y.2, xlab = 'lambda', ylab = 'loglikelihood', type = 'l', col='red',
   main='loglikelihood function for \n a sample from an exponential distribution  \n with size=5 and lambda=2')
t[which.max(y.2)]
```

#Q66

The sample size is assumed to be $n=1$,and MLE is that, $$L(p|X_1)=p^{X_1} \cdot (1-p)^{1-X_1}$$
So the log MLE is that,
$$log L(p|X_1)  =  X_1 \cdot log(p) + (1-X_1) \cdot log(1-p)$$

$$\frac{d}{dp} log L(p|X_1)  =  \frac{X_1}{p} - \frac{1-X_1}{1-p}=0$$

$$X_1 \cdot (1-p) = (1-X_1) \cdot p$$

$$p=X_1$$


#Q67

The pdf of $X \sim U(\alpha,\beta)$ is $pdf=\frac{1}{\beta-\alpha} \ \ \ X \in [\alpha,\beta]$.

##**Method 1 :**

As we know $Var(X)=E(X^2) - E(X)^2$, so
$$E(X^2)=Var(X) + E(X)^2  =  \frac{(\beta - \alpha)^2}{12} + {\frac{\beta +alpha}{2}}^2  =  \frac{1}{3} \cdot (\alpha^2 + \alpha \beta +\beta^2)  =   \frac{1}{3} (4E(X)^2-\alpha \beta )$$

##**Method 2 :**

Use Method of Moments estimate.
$$E(X)=\int_{\alpha}^{\beta} \frac{X_i}{\beta-\alpha} \ \ dx= \frac{\alpha + \beta}{2}$$

$$E(X^2)=\int_{\alpha}^{\beta} \frac{X_i^2}{\beta-\alpha} \ \ dx=\frac{1}{3} \cdot (\alpha^2 + \alpha \beta +\beta^2)=  \frac{1}{3} (4E(X)^2-\alpha \beta )$$


$E(X)=5.8,E(X^2)=43.8$,  and we get 

$$\alpha + \beta =5.8*2=11.6 \ \  , \ \ \ \ \ \alpha \beta=4 * 5.8^2-43.8 * 3= 3.16$$

finally,I get $$\alpha=0.3\ \  \ , \ \ \beta=11.3$$

#Q63

##a

-----

median of Arsenic data column in the Bangladesh data is 22,and the  90th percentile is $[0.5,590.5]$

-----

```{r}
Bangladesh <- read.csv('C://Users//mali//Documents//Bangladesh.csv')
median(Bangladesh$Arsenic)
quantile(Bangladesh$Arsenic,c(0.05,0.95))
```

##b

bias of the median is 1.46349, using the bootstrap 

```{r}
set.seed(63)
bt_median <- replicate(10000,median(sample(Bangladesh$Arsenic,nrow(Bangladesh),replace = T)))
mean(bt_median-22)
```


##c

the bias of the 90th percentile is $[-8.0,14.4]$, using the bootstrap.

```{r}
quantile(bt_median-22,c(0.05,0.95))
```



#Q69

------

$$L(\theta)=\frac{1}{\pi^n \cdot \prod_{i=1}^{n}[1+(X_i - \theta)^2]}$$

$$log L(\theta)= - n log(\pi) - \sum_i^n log(1+(X_i- \theta)^2)$$


the largest number of local maxima that I can observe is 1.13 and the mean of  local maxima that I can observe is -0.0075.

-----


```{r}
#likelihood function
cau_mle <- function(theta,x){
  1/(pi^length(x)*prod(1+(x-theta)^2))
}

set.seed(6900)
N <-12 #12 random samples
theta <- numeric(N)
et <- seq(-30,30,by = 0.01)
par(mfrow=c(2,2))
for (i in 1:N) {
x0 <- rcauchy(n = 10,location = 0)
#Compute the log likelihoods for a range of x0
cau_v <- sapply(et,function(et){cau_mle(et,x0)})
plot(et,cau_v,type='l',main=paste( i,'th sample  mle \n for Cauchy distributions',sep=''),
     xlab='location paramete',ylab='log mle',col=i)
theta[i] <- et[which.max(cau_v)]
}
theta
mean(theta)
max(theta)
```


#Q70


-----

Data  is from location parameter theta =0 of a Cauchy distribution .

Compute 10000 simulations with sample sizes n = 10, 20, 40,100 :

MEAN

method|n=10|n=20|n=40|n=100
------|----|----|----|-----
trim_mean|0.011| -0.013| 0.006 | 0.001
median|0.005| 0.004 |0.001 |0.002 


VAR

method|n=10|n=20|n=40|n=100
------|----|----|----|-----
trim_mean|1.742| 0.425| 0.146 |0.052 
median|0.340 |0.140| 0.064| 0.025 



对于N=10,20,40,100,总有$var(median) < var(trim_mean)$,因此 median estimator 的估计是more effcient.

-----

```{r}
set.seed(70)
mat_theta_trim_mean <- matrix(NA,ncol = 4,nrow = 10000,
   dimnames=list(1:10000,c('n10','n20','n40','n100')))
mat_theta_median <- matrix(NA,ncol = 4,nrow = 10000,
   dimnames=list(1:10000,c('n10','n20','n40','n100')))

for (i in 1:4) {
 N <- c(10,20,40,100)
 mat_theta_trim_mean[,i] <- replicate(10000,mean(rcauchy(N[i]),trim = 0.10))
 mat_theta_median[,i] <-replicate(10000,median(rcauchy(N[i])))
}

round(colMeans(mat_theta_trim_mean),3)
round(apply(mat_theta_trim_mean,2,var),3)
round(colMeans(mat_theta_median),3)
round(apply(mat_theta_median,2,var),3)

par(mfrow=c(2,2))
hist(mat_theta_trim_mean[,1],main='trimmed mean estimated \n n=10')
hist(mat_theta_trim_mean[,2],main='trimmed mean estimated \n n=20')
hist(mat_theta_trim_mean[,3],main='trimmed mean estimated \n n=40')
hist(mat_theta_trim_mean[,4],main='trimmed mean estimated \n n=100')
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(mat_theta_median[,1],main='median estimated \n n=10')
hist(mat_theta_median[,2],main='median estimated \n n=20')
hist(mat_theta_median[,3],main='median mean estimated \n n=40')
hist(mat_theta_median[,4],main='median mean estimated \n n=100')
par(mfrow=c(1,1))
```
