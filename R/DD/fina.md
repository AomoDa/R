---
title: "Final"
author: "Your Nmae"
date: "2016-12-13"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Part 1: Probability Theory  

##Q1

###a

Consider a continuous random variable $X$ that has values in an interval $[0, a]$, with cdf $F(x)=cx^2$ for $0 \leq x \leq a$ and F(x) = 1 for $x \geq a$, where $c$ is some constant.

So $$P(x \leq a)=F(a)=c \cdot a^2=1$$
Thus $$c=\frac{1}{a^2}$$

###b

- The 25th percentile  $X_{25\%}$ has $F(X_{25\%})= \frac{1}{a^2} \cdot X_{25\%}^2=\frac{1}{4}$, so $X_{25\%}=\frac{a}{2}$.

- The median,  $X_{median}$ has $F(X_{median})= \frac{1}{a^2} \cdot X_{median}^2=\frac{1}{2}$, so $X_{median}=\frac{\sqrt 2 a}{2}$.

- The 75th percentile  $X_{75\%}$ has $F(X_{75\%})= \frac{1}{a^2} \cdot X_{75\%}^2=\frac{3}{4}$, so $X_{25\%}=\frac{\sqrt 3 a}{2}$.

##Q2

###a


```{r}
set.seed(200)
x <- rexp(n = 10000,rate = 1)
stop_time <- c(2,4,6,8)

for (i in stop_time) {
	a <- x
	a[a>i] <- i
	a <- as.numeric(a)
	assign(paste0('a',i),value = a)
}
plot.ecdf(x,lty=1,lwd=1,xlim=c(0,10))
plot.ecdf(a8,col='red',lty=1,
          lwd=2,add=T,xlim=c(0,10))
plot.ecdf(a6,col='blue',lty=1,
          lwd=1,add=T,xlim=c(0,10))
plot.ecdf(a4,col='green',lty=1,
          lwd=2,add=T,xlim=c(0,10))
plot.ecdf(a2,col='yellow',lty=1,
          lwd=1,add=T,xlim=c(0,10))
legend('bottomright',
       lty=1,
       lwd=c(1,2,1,2,1),
       col=c('black','red','blue','green','yellow'),
       legend=c('Xi','X*=8','X*=6','X*=4','X*=2'),
       ncol=2 )

```


###b

- The bias of this estimator with simulations is 0.588 when $X^*=1$
- The bias of this estimator with simulations is 0.158 when $X^*=2$
- The bias of this estimator with simulations is 0.004 when $X^*=5$

```{r}
set.seed(2000)
x <- rexp(n = 10000,rate = 1)
# set x*
stop_time <- c(1,2,5)
for (i in stop_time) {
	a <- x
	a[a>i] <- i
	 a <- as.numeric(a)
	assign(paste0('a',i),value = a)
}

1/mean(a1)-1
1/mean(a2)-1
1/mean(a5)-1
```



##Q3






##Q4

###a

- $E(|X|)=0.8$
- $var(|x|)=0.36$

```{r}
set.seed(300)
x <- rnorm(1e5)
round(mean(abs(x)),2)
round(var(abs(x)),2)
```

###b


$E(|X|)=\frac{2 \sigma}{\sqrt{2 \pi} }$ and $Var(|X|)=\sigma^2 - \frac{2 \sigma^2}{\pi}$, And my calculation procedure is as follows


Let $X \sim N(0,\sigma^2)$

$$\begin{array}{rcl} E(|X|) &=& \int_{- \infty}^{+ \infty} |X| \cdot \frac{1}{\sqrt{2 \pi} \sigma } e^{- \frac{x^2 }{2 \sigma^2}}  dx \\ &=&  \frac{2 \sigma}{\sqrt{2 \pi} } \int_0^{+ \infty} z \cdot e^{- \frac{z^2}{2}} dz \\ &=&  \frac{2 \sigma}{\sqrt{2 \pi} }  \end{array} $$

where $z= \frac{x}{\sigma}$


$$\begin{array}{rcl}Var(|X|) &=& E(|X|^2)-(E(|X|))^2 \\ &=&  E(X^2)-(E(|X|))^2 \\ &=& Var(X)-(E(X))^2-(E(|X|))^2  \\ &=& \sigma^2 - \frac{2 \sigma^2}{\pi} \end{array} $$


#Part 2 Statistic Problems

##Q5


