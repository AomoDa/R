---
title: "Untitled"
author: "Your Nmae"
date: "2016-12-02"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Part 1

##a

##b

##c


The proportion of patients have Physician's estimate of Karnofsky score greater than/equal to/less than the patient's estimate is 153.

```{r}
#load the data into R
lung<-read.csv("C://Users//mali//Documents//lung.csv",
               header=T)
# compute the proportion of patients have 
# Physician's estimate of Karnofsky score 
# greater than/equal to/less than the patient's estimate?
summary(lung$ph.karno>=lung$pat.karno)
```

##d


-----

age categorises| rows|interval|mean weight loss
---------------|-----|--------|----------------
A|76|$[39,59]$|10.36
B|76|$(59,68]$|9.56
C|76|$(68,82]$|9.51

-----


```{r}
# compute the rank of age
age_rank <- rank(lung$age,ties.method = 'first')
# Create a new variable which categorises age 
# into one of three categories, so that there are
# roughly the same number of individuals in each category.
lung$age_categorises <- cut(age_rank,breaks = 3,
                            labels = c('A','B','C'))
#For each category, calculate the rows and age range
table(lung$age_categorises)
aggregate(age~age_categorises,
          data=lung,FUN =range)
#For each category, calculate the mean weight loss
aggregate(wt.loss~age_categorises,
          data=lung,FUN = mean)
```

##e

- The male range of mean calories consumed is 2584 and the female is 2354.
- The death range of mean calories consumed is 2584 and the censoring is 2354.


```{r}
# define the range function 
range_value <- function(x){
  return(max(x)-min(x))
}

# Calculate the range, across indicator, of mean calories consumed
aggregate(meal.cal~status,
          data=lung,
          FUN =range_value)
#Calculate the range, across sex, of mean calories consumed
aggregate(meal.cal~sex,
          data=lung,
          FUN =range_value)
```

##f

It seems to be wrong that the number of calories consumed at meals is correlated to survival time. Because the line of  calories consumed at meals is correlated to survival time is a straight line,which indicated there is no any connections between them.

```{r}
library(ggplot2)
#Plot a graph of calories consumed at meals VS survival time
ggplot(data=lung,aes(x=meal.cal,y=time))+
  geom_point(na.rm = T)+
  geom_smooth(method = 'lm',na.rm = T)+
  labs(title='calories consumed at meals VS survival time')

```

#Part 2

##a


```{r}
# define g(x)=sin(x)^2
f <- function(x) {return(sin(x)^2)}
#Graph the function using plot
x <- seq(0,2*pi,length.out = 1000)
plot(x,f(x),type='l',main = expression(sin(x)^2))
```

##b

The approximation is 3.38 with $m=100.

```{r}
#Write a function mcint that takes m as an argume
mcint <- function(m){
  a <- 0 # from
  b <- 2*pi # to 
  g <- replicate(m,f(runif(1,a,b)))
  #a random sample from 
  # a uniform distribution on theinterval [a, b].
  rt <- (b-a) / m * sum(g) # approximation
  return(rt)
}
set.seed(1234)
# Use mcint function to approximate 
# the integral using m = 100
mcint(100)
```

##c

```{r}
# use integrate function  to calculate 
# the area under the curve between 0 and 2*pi
integrate(f = f,lower = 0,upper = 2*pi)
```

##d

$$\int_0^{2 \pi} sin^2x \ dx =\int_0^{2 \pi} \frac{1}{2} (1- cos2x) \ dx =  (\frac{x}{2} - \frac{sin2x}{4}) |_0^{2 \pi}=\pi $$

##e

With the increase of m, the ratio of my answer to the
exact answer is tending to zero.

```{r}
#generate a sequence from 1000 to 10000 by step 1 
mydat <- data.frame(m=seq(1000,10000,by = 1))
set.seed(2000)
# Use apply and mcint to approximate 
# the area under the curve using m 
 y <- apply(mydat,1,mcint)
#work out the ratio to the exact answer
 ratio <- (y-pi) / pi *100
#Produce a line plot of this ratio against m.
plot(mydat$m,ratio,
     type='l',
     xlab='m',
     main='a line plot of this ratio against m.')
abline(h=0,col='orange',lty=2,lwd=2)
```

