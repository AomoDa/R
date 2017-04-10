---
title: "Untitled"
author: "Your Nmae"
date: '2017-04-10'
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Problem 4


- Use SANN algorithm and $10^6$ iterations,the maximize is **14.41**.
- Use BFGS algorithm and $10^6$ iterations,the maximize is **12.41**.

```{r}
load(url("http://www.stat.umn.edu/geyer/3701/data/q5p4.rda"))
optim(par = rep(0,10),
      fn = function(x) -sally(x),
      method = 'SANN',
      control = list(maxit=1e6))
optim(par = rep(0,10),
      fn = function(x) -sally(x),
      method = 'BFGS',
      control = list(maxit=1e6))
```

# Problem 5

When $X=-1.69$,$Y=0.75$ and $Z=-0.75$,the minimize is **2.38**.

```{r}
q5<- function(theta){
  stopifnot(length(theta)==3)
  x <- theta[1]
  y <- theta[2]
  z <- theta[3]
  rt <- ifelse(sum(theta^2)<4,Inf,x^2 + y^4 + z^6 + sin(x+y+z))
  return(rt)
}

optim(par = c(-1,-1,-2),fn = q5)
```

# Problem 6

The same as Problem 5.

```{r}
q5_grr <- function(theta){
 q6 <- expression(x^2+y^4+z^6+sin(x+y+z))
 x <- theta[1]
 y <- theta[2]
 z <- theta[3]
 if(sum(theta^2)<4) {
  rt <- c(Inf,Inf,Inf)
 }
  else {
  rt <- c( eval(D(expr = q6,name = 'x')),
          eval(D(expr = q6,name = 'y')),
          eval(D(expr = q6,name = 'z')))
  }
}

optim(par = c(-1,-1,-2),fn = q5,gr = q5_grr)
```

