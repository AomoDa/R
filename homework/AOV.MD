---
title: "Untitled"
author: "Your Nmae"
date: "2016年10月21日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#part2

##方差已知

当总体方差未知的时候，采用Z检验，其中Z统计量为$|z|=0.02197953$
经过计算之后$u$的区间为$[97.38975,103.2696]$，其中显著性水平$\alpha=0.05$。

```{r}
set.seed(222)
y <- rnorm(n = 100,mean = 100,sd = 15)
mean(y)
upp <- qnorm(p = 0.975)*15/sqrt(100) + mean(y)
upp
low <- qnorm(p = 0.025)*15/sqrt(100) + mean(y)
low
z_stat <- (mean(y)-100)/15
z_stat
```

##Bonferroni 

模拟20次Bonferroni校正，发现有一次失败，第一类假设错误率为 1/20=0.05，当$\alpha=0.05$的时候。

```{r}
set.seed(200)
k <-20
alpha <- 0.05/k
real <-c()
for (i in 1:k) {
y <- rnorm(n = 100,mean = 100,sd = 15)
mean(y)
upp <- qnorm(p = 0.975)*15/sqrt(100) + mean(y)
low <- qnorm(p = 0.025)*15/sqrt(100) + mean(y)
real[i] <- ifelse(low<100 & upp >100,1,0 )
}
real
```


##方差未知

- 当总体方差未知的时候，采用T检验，计算的T统计量为 $|t|=0.2294253$.
- 经过计算之后$u$的区间为$[97.4783,103.1811]$,其中自由度为$df=100-1=99$,显著性水平$\alpha=0.05$

当df相近的时候，T分布和Z分布相似。

```{r}
set.seed(222)
y <- rnorm(n = 100,mean = 100,sd = 15)
mean(y)
sample_var <- var(y)
upp <- mean(y)+qt(p = 0.975,df = 99) * sqrt(sample_var/100)
upp
low <- mean(y)+qt(p = 0.025,df = 99) * sqrt(sample_var/100)
low
t_stat <- (mean(y)-100) / (sqrt(sample_var/100) )
t_stat
```

#part3

经过计算，当v=15.30128的时候，两条虚线相交。

```{r}
sg0 <- (1000:2000)*.01
sg1 <- 18
n <-100
y_ss <- 28850.62
y_chisq <- y_ss / sg0^2
p_value <- 1-pchisq(y_chisq,n-1)
plot(sg0,p_value,pch='.')
abline(h=0.05,lty=2)
#计算sg0为某一个值得的时候，p_value=0.05
a <-sqrt(y_ss/qchisq(p = 0.95,df = 99) )
a
abline(v=a ,lty=2)
```




