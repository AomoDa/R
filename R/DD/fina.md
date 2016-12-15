---
title: "Final"
author: "Your Nmae"
date: '2016-12-13'
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

- The 25th percentile  $X_{25\%}$ has $F(X_{25\%})= \frac{1}{a^2} \cdot X_{25\%}^2=\frac{1}{4}$, so $X_{25\%}=\frac{1}{2} a$.

- The median,  $X_{median}$ has $F(X_{median})= \frac{1}{a^2} \cdot X_{median}^2=\frac{1}{2}$, so $X_{median}=\frac{\sqrt 2}{2} a$.

- The 75th percentile  $X_{75\%}$ has $F(X_{75\%})= \frac{1}{a^2} \cdot X_{75\%}^2=\frac{3}{4}$, so $X_{25\%}=\frac{\sqrt 3}{2} a$.

##Q2

###a

I choose $X^* \in \{2,4,6,8 \}$, and then draw the cdf of the $X_i$ and of the $Y_i$ in the same coordinate system.I find their CDF completely overlapped in the cdf plots. But  the difference is that CDF curve will immediately rise to 1 after $X*$ ,when $X*>=4$,the cdf of $X_i$ and $Y_i$ are almost the same.

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

- The true $\lambda^{-1}=1^{-1}=1$
- The bias of this estimator with simulations is -0.3701 when $X^*=1$
- The bias of this estimator with simulations is -0.1363 when $X^*=2$
- The bias of this estimator with simulations is -0.0039 when $X^*=5$
- The bias of this estimator will more close to the true value with the increase of simulation size.


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

mean(a1) - 1
mean(a2) - 1
mean(a5) - 1
```



##Q3


Given a sample of size $n$ from a Bernoulli distribution with success parameter $p$. Suppose there are exactly $x$ successes in the sample. And then the unbiased estimate  of $p$ is $\frac{x}{n}$.

Each sampling is independent of each other  and  the probability of success is $\frac{x}{n}$. So the total number of successes with n experiment is $C^a_n \cdot {(\frac{x}{n})}^a \cdot  {(1- \frac{x}{n})}^{n-a}$,which is  a $B(n,\frac{x}{n})$ distribution,where a is the number of successes.





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

I have computed the bias of this estimator for $n = 10, 20, 30 ...100$ and plot the boxplot of the bias.The boxplot of the bias tell me that the bias will decrease with the increase of size $n$.When the sample are more than  80, the bias would be  very close to zero.


-----

`size`| `bias_mean`| `bias_upp` |`bias_low`
------|------------|------------|----------
   10  |    0.74  |  10.24   | -5.56
  20  |    0.33  |   5.94  |  -4.22
  30   |   0.14  |   4.57 |   -3.86
 40    |  0.11  |   3.91  |  -3.11
  50   |   0.13 |    3.90 |   -2.71
  60   |   0.19 |    3.46  |  -2.42
   70  |    0.17 |    3.24  |  -2.16
   80   |   0.05  |   2.80|    -2.42
   90  |    0.07  |   2.75 |   -2.21
 100   |   0.07   |  2.54|    -2.26

-----



```{r, message=FALSE, warning=FALSE}
library(ggplot2)
library(plyr)
load('C://Users//AomoDa//Documents//final2016.RData')
duration <- Q2_16$duration
sizes <- seq(from = 10,to = 100,by = 10)
true_med <- median(duration)
set.seed(500)

rt <- data.frame(stringsAsFactors = F)
for (i in sizes) {
	a <- replicate(n=1000,
	               median(sample(x=duration,
	                             size=i,
	                             replace=T)))
	a <- a - true_med
	rt <- rbind(rt,data.frame(size=i,bias=a))
}

ggplot(data=rt,
       aes(x=as.factor(size),y=bias,fill=as.factor(size)))+
  geom_boxplot(show.legend=F)+
  labs(x='size',title='boxplot of bias')+
  lims(y=c(-10,15))

ddply(.data = rt,
      .variables = .(size),
      .fun = summarise,
      bias_mean=round(mean(bias),2),
      bias_upp=round(quantile(bias,0.975),2),
      bias_low=round(quantile(bias,0.025),2))
```

##Q6

In the study of this problem, I adopt a  robust method.

- First,I use  permutation test with $H_0 :U_E = U_W$ and $H_1 :U_E > U_W$.And I compute the p value,which is $0.01$ and I reject $H_0$.
- Second I use F test with $H_0:\sigma^2_e = \sigma^2_w$. I want to know whether their variances are equal because the next T test needs to use this conclusion. The pvalue is $0.1396$ and I  cat not reject $H_0$, which means $\sigma^2_e = \sigma^2_w$.
- Next I use t test with the same variance.The $H_0 :U_E \leq U_W$,And I compute the p value,which is $0$ and I  reject $H_0$.
- Finally,the result of permutation test and t test is the same .And I have enough confidence to decide $U_E > U_W$.
- $U_E=46.49$ and $U_W=43.64$,which means casual riders have different average trip lengths on weekdays and on weekends and casual riders have more large trip lengths on weekends than weekdays.


```{r, message=FALSE, warning=FALSE}
q6 <- subset(Q2_16,subset =account=='Casual' ,select = c('duration','day.of.week'))
aggregate(duration~day.of.week,data=q6,mean)
library(gplots)
# 95% CI of average trip lengths 
plotmeans(duration~day.of.week,
          data=q6,n.label = F,
          ci.label = T,
          ylim=c(43,48),
          main='95% CI of average trip lengths  ')
# histogram
ggplot(data=q6,aes(x=duration,y=..density..,fill=day.of.week))+
  geom_histogram(bins = 50,col=I('white'),show.legend = F)+
  lims(x=c(1,100))+facet_wrap(~day.of.week)

#PERMUTATION TESTS
#H0: U_E = U_W
#H1: U_E > U_W
table(q6$day.of.week)

N <- 1e3-1
result <-c()
len <- nrow(q6)
set.seed(600)
for (i in 1:N) {
   ind <- sample(len,size=94126,replace=F)
   result[i] <- mean(q6$duration[ind]) - mean(q6$duration[-ind])
}
observed <- 46.49141-43.63772
(sum(result >= observed) + 1)/(N + 1)
#F test to compare two variances
var.test(duration~day.of.week,data=q6)
#Welch Two Sample t-test
t.test(duration~day.of.week,
       data=q6,
       var.equal=T,
       alternative='greater')
```


##Q7


My answer is **7**,sample size should n be over **7** such that a 95% confidence interval for the difference of mean trip lengths does not contain zero with probability 80%.

On the other hand, I repeat that my simulation program useing  different cycle times and I  can also draw the same conclusion .(The answer maybe 8,becuase in my many simulations the size must be 8 ,but  In most cases it is 7.)


```{r}
# Trips by Registered users
duration_reg <- subset(Q2_16,
                       subset =account=='Registered',
                       select = duration,drop = T)
# Trips by casual users
duration_cas <- subset(Q2_16,
                       subset =account=='Casual',
                       select = duration,drop = T)
set.seed(700)
# sample size n is from 1 to 10
N_size <- seq(from=1,to = 10,by = 1)
p<-c()

for (i in N_size) {
	a <- data.frame(low=numeric(),upp=numeric())
	for (j in 1:100) {
    diff_mean<- replicate(100,mean(sample(duration_cas,size=i))- mean(sample(duration_reg,size=i)))
    # compute 95% confidence interval for the difference
    # from two random samples of trip lengths 
    # for casual and registered users, 
    # each of size n.
    a[j,1] <- quantile(diff_mean,0.025)
    a[j,2] <- quantile(diff_mean,0.975)
	}
	# compute the probability that 95% confidence interval 
	# for the difference of mean trip 
	# lengths does not contain zero
   probability <- 1 - mean(a$low<0 & a$upp>0)
   # print the information of size n 
print(paste0('i= ',i,' ,and then p = ',probability,' .'))
}
```




##Bonus question

The distance between the two station is certain. So the duration between the two station has a certain rule and I think it is a normal distribution.

And I think the duration would be longer when a bicycle  is in the repair shop.The critical values to decide  whether a bicycle  is in the repair shop maybe a duration when the duration are over 95% or 99% of all people.The 95% can be define $q_{95 \% } \cdot \sigma_{duration}$ and 99% can be define $q_{99 \% } \cdot \sigma_{duration}$.

- When I use 95% critical values,the  probability  which a bicycle is in the repair shop  is 5.23%.
- When I use 99% critical values,the  probability  which a bicycle is in the repair shop  is 2.91%.





```{r}
# define probability bicycle in the repair shop function
repear_shop <- function(x,alpha=0.95) {
    rn <-length(x)
	if( rn==1) a <- 0
	if(rn>1){
		avg <- mean(x)
		xsd <- sd(x)
		upp <- avg + qnorm(alpha) * xsd
		a <- sum( x >upp  )
	}
	return(a)
}
# apply my function 
a <- ddply(.data = Q2_16[,c(1,4,6)],
           .variables = .(start.number,end.number),
           .fun = summarise,
           repear_num_95 =repear_shop(duration,alpha=0.95),
           repear_num_99 =repear_shop(duration,alpha=0.99),
           rn=length(duration) )
# compute the probability
#95 %
sum(a$repear_num_95)/nrow(Q2_16)*100

#99 %
sum(a$repear_num_99)/nrow(Q2_16)*100

```

