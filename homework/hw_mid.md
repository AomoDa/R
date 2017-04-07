---
title: "Untitled"
author: "Your Nmae"
date: "2017年4月7日"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Q1

##a

```{r}
q1_func <- function(x,k) {
 x <- as.character(x)
 n <- nchar(x)
 k <- ifelse(n>k, k, n)
 table_length <- n - k + 1
 tb_cnt <- as.vector(table_length)
 for (i in 1: table_length ) {
 	tb_cnt[i] <- substr(x,i,i+k-1)
 }
 rt <- as.table(sort(table(tb_cnt),decreasing = T))
 return(rt)
}

# test function
q1_func(x="CAGACAAAAC",k=3)
```

##b

```{r}
x <- ''
for (i in 1:10000) {
  x <- paste0(x,sample(x = c('A','C','G','T'),size = 1))
}
# test function
q1_func(x=x,k=3)
```

#Q2

##a

```{r}
q2a_func <-function(x){ 
	low <- t.test(x)$conf.int[1]
	upp <- t.test(x)$conf.int[2]
	return(list(lower=low,upper=upp))
}
# test function
q2a_func(x=rnorm(100))
```

##b

```{r}
q2b_func <- function(n) {
  sim <- replicate(n = 20,expr = q2a_func(rnorm(n = n)))
  return(list(lower =unlist(sim[1,]),upper =unlist(sim[2,])))
}

# test function
q2b_func(20)
```

##c

```{r}
q2c_func <- function(x) {
	par(xpd = NA)
    plot(NA, NA, type = "n", xlim = c(-2, 2), ylim = c(1, 20), xlab = "95% confidence interval", ylab = "Times")
    for (i in 1:20) {
    lines(x=c(x$lower[i],x$upper[i]),y=c(i,i))}
}

# test function
q2c_func(x = q2b_func(n=20))
```

##d

```{r}
q2d_func <- function(x) {
	par(xpd = NA)
    plot(NA, NA, type = "n", xlim = c(-2, 2), ylim = c(1, 20), xlab = "95% confidence interval", ylab = "Times")
    lines(x=c(0,0),y=c(0,20),col='red',lty=2)
    for (i in 1:20) {
    lines(x=c(x$lower[i],x$upper[i]),y=c(i,i),col=i)}
}

# test function
q2d_func(x = q2b_func(n=20))
```

##e

```{r}
q2e_func <- function(mu){
    sim <- replicate(n = 20,expr = q2a_func(rnorm(n = 100,mean=mu)))
    x <- list(lower =unlist(sim[1,]),upper =unlist(sim[2,]))
    par(xpd = NA)
    plot(NA, NA, type = "n", xlim = c(-2+mu, 2+mu), ylim = c(1, 20), 
    	xlab = "95% confidence interval", ylab = "Times",main=paste0('true mean is ',mu))
    lines(x=c(mu,mu),y=c(0,20),col='red',lty=2)
    for (i in 1:20) {
    lines(x=c(x$lower[i],x$upper[i]),y=c(i,i),col=i)}
}

# test function
par(mfrow=c(1,2))
q2e_func(mu=3)
q2e_func(mu=5)

```


#Q3

##a

```{r}
data(airquality)
y <- airquality[airquality$Ozone>160 & !is.na(airquality$Ozone>120), ]
plot(airquality$Ozone,type='l',xlab='Date',ylab='Ozone')
abline(h=120,lty=2,col='green')
abline(h=140,lty=2,col='orange')
abline(h=160,lty=2,col='red')
text(x = row.names(y),y=y$Ozone,labels = paste0(y$Month,'-',y$Day),col='red')
plot(airquality$Ozone,type='h',xlab='Date',ylab='Ozone')
abline(h=120,lty=2,col='green')
abline(h=140,lty=2,col='orange')
abline(h=160,lty=2,col='red')
text(x = row.names(y),y=y$Ozone,labels = paste0(y$Month,'-',y$Day),col='red')
```

##b

```{r}
with(airquality,plot(Solar.R,Ozone,main='Ozone VS Solar.R'))
abline(lm(Ozone~Solar.R,data=airquality),lty=2,col='red',lwd=2)
```

##c

```{r}
coplot(Ozone~Solar.R | cut(Wind,3),data=airquality,bg = "pink")
coplot(Ozone~Solar.R | cut(Temp,3),data=airquality,bg = "pink")
coplot(Ozone~Solar.R | cut(Wind,3)*cut(Temp,3),
       data=airquality,bg = "pink")
```

#Q4

##a

```{r}
x <- c(3.43,3.45,2.5,3.86,1.52)
y <- c(0.86,4.05,3.18,3.84,2.52,3.99,2.08,4.26,2.57,3.54)

q4a_func <- function(x,y){
	all_data <- rbind(data.frame(g='x',value=x),data.frame(g='y',value=y))
	all_data$rank <- rank(all_data$value)
	w1 <- all_data$rank[all_data$g=='x']
	w2 <- all_data$rank[all_data$g=='y']
	n1 <- length(x)
	n2 <- length(y)
	u1 <-  sum(w1)-n1*(n1+1)/2
	u2 <-  sum(w2)-n2*(n2+1)/2
    return(min(c(u1,u2)))
}

#test function
q4a_func(x,y)
```

##b & c 

```{r}
q4b_func <- function(x,y){
 u <- numeric(1e4)
 for (i in 1:1e4) {
	a <- c(x,y)
	ind <- sample(x = length(x)+length(y),size = length(x))
	x1 <- a[ind]
	y1 <- a[-ind]
	u[i] <- q4a_func(x1,y1)
 }
 # two sided p value
 p_value <- ifelse(mean(u>20)>0.5,1-mean(u>20),mean(u>20))*2
 return(p_value)
}
#test function
set.seed(1)
q4b_func(x,y)
```

##d

```{r}
n1 <- length(x)
n2 <- length(y)
m_u <- n1*n2 / 2
sigma_u <- sqrt(n1*n2*(n1+n2+1)/12   )
u <- q4a_func(x,y)
z <- (u-m_u)/sigma_u

# two sided p value
p <- pnorm(z)
p_value <- ifelse(p>0.5,1-p,p)*2
p_value
```

