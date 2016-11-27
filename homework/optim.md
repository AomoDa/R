---
title: "Untitled"
author: "Your Name"
date: "2016年11月27日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q13

##a


$$\theta_1=16.501$$
$$\theta_2=83.332$$
$$\theta_3=0.273$$
$$\theta_4=0.067$$

```{r}
x <- read.table('C://Users//AomoDa//Documents//ZeroPrices.txt',header = T)
summary(x)
forward_rate_fun <- function(para) {
  p <- para[1]+(para[2]+para[3]*x$T)*exp((-para[4]*x$T))
  return(sum((x$price-p)^2) )
}
fit <- optim(par = c(-10,1,1,1), 
      fn = forward_rate_fun,
      hessian=TRUE, 
      control=list(maxit=10000))
fit
```

##b

```{r}
with(x,plot(`T`,price,type='l',main='ZeroPrices',col='red',lty=3))
with(x,points(`T`,price,type='p',col='red',pch=19))
para <- fit$par
forward_rate_pred <- para[1]+(para[2]+para[3]*x$T)*exp((-para[4]*x$T))
points(x$T,forward_rate_pred,type='p',pch=8,col='blue')
legend('topright',col=c('red','blue'),pch=c(19,8),legend = c('real','fitted'))
```


