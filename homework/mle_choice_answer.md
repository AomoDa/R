---
title: "DSC5103 Assignment 2"
subtitle: "Maximum Likelihood Estimation & Choice Models"
author: "Tong Wang"
date: "Sep 2016"
output:
  html_document:
    theme: yeti
    highlight: tango
  pdf_document:
    highlight: zenburn
---
<!--
comments must be put in an HTML comment form
-->

## NOTE:
This assignment is **due at 23:59 of Sep 8, Thursday**. You can work on this file directly and fill in your answers/code below. Please submit the output HTML file (name your file like G1Group02.html if you are from Group 02 of Section G1) onto IVLE/Files/Student Submission/Assignment2 folder.

Also, put the Section/Group and member info below.
```{r}
# Section G1
# Group 9
# Members: RUI YANG,RUI BAO,ZHEN YU,YULAI WEI
```


## Part I: Maximum Likelihood Estimation of Willingness-To-Pay Distribution
### Introduction
Willingness-To-Pay (WTP) is the maximum amount that a customer is willing to pay for a product. A customer will purchase the product if and only if his/her WTP is larger than or equal to the price. For a population of customers, their WTP collectively forms a distribution. Understanding the WTP distribution plays a critical role in pricing and other related activities.

There are not many good ways to learn the WTP distribution. Often, we need to estimate it from the customers' actual purchase decisions. In the following dataset, we simulate 100 historical transactions with the customers' true **WTP**, the **price** offered to the customer, and **purchase** that indicates whether the customer took the deal or not. Below is how the data file is generated

```{r}
set.seed(1234)  # set seed for random number generator
N <- 100  # number of data points
wtp <- rlnorm(N, meanlog=3, sdlog=0.5)  # WTP follows a log-normal distribution with (3, 0.5)
data <- data.frame(price=round(runif(N, min=10, max=40)))  # price is uniform[10, 40], rounded to integers
data$purchase <- (wtp >= data$price)  # purchase is TRUE if WTP >= price
summary(data)
```

The true WTP has mean `r mean(wtp)` and standard deviation `r sd(wtp)`, and its distribution is as below.
```{r}
hist(wtp, breaks=15, probability=TRUE)
curve(dlnorm(x, 3, 0.5), from=0, to=80, add=TRUE)
```

In the following, we shall try to estimate the WTP distribution based on the data with **price** and **purchase** information only.

### Questions and Answers

#### 1. Suppose that we believe that the customers are from a population with their WTP following a *normal* distribution $N(\mu, \sigma^2)$. Estimate $\mu$ and $\sigma^2$ using MLE. (1 Mark)

Answer: 

```{r}
theta0 <- c(mean(wtp), sqrt((sum((wtp - mean(wtp))^2)) / N))
theta0
LL1 <- function(theta, data) {
    mu <- theta[1]
    sigma <- theta[2]
    ll1 <- pnorm(data$price, mean=mu, sd=sigma, lower.tail=FALSE, log.p=TRUE)
    ll2 <- pnorm(data$price, mean=mu, sd=sigma, lower.tail=TRUE, log.p=TRUE)
    ll <- ifelse(data$purchase=="TRUE", ll1, ll2)
    return(sum(ll))
}

output1 <- optim(c(0, 1), LL1, method="L-BFGS-B", lower=c(-Inf, 1e-6), upper=c(Inf, Inf), control=list(fnscale=-1), data=data)
output1 
theta1 <- output1$par
theta1

```



#### 2. Now suppose the customers??? WTP follows a *Gamma* distribution with shape parameter $\alpha$ and rate parameter $\beta$, estimate $\alpha$ and $\beta$ using MLE. (hint: check out the pgamma() function in R) (1 Mark)

Answer: 

```{r}
theta02 <- c((mean(data$price))^2/(sum((wtp - mean(wtp))^2)/N),(mean(data$price)/((sum((wtp - mean(wtp))^2))/N)))
theta02
LL2 <- function(theta, data) {
    alpha <- theta[1]
    beta <- theta[2]
    ll1 <- pgamma(data$price,shape=alpha,scale=1/beta,lower.tail=FALSE, log.p=TRUE)
    ll2 <- pgamma(data$price,shape=alpha,scale=1/beta,lower.tail=TRUE, log.p=TRUE)
    ll <- ifelse(data$purchase=="TRUE", ll1, ll2)
    return(sum(ll))
}
output2 <- optim(c(1, 1), LL2, method="L-BFGS-B",lower= c(1e-6, 1e-6), upper=c(Inf, Inf), control=list(fnscale=-1), data=data)
output2
theta2 <- output2$par
theta2

```



#### 3. Plot the distributions estimated in Q1 and Q2 together with the true WTP distribution from the data generating model. Briefly comment on the estimated distributions. (1 Mark)

Answer: 

```{r}
hist(wtp, breaks=15, probability=TRUE)
curve(dlnorm(x, 3, 0.5), from=0, to=80, add=TRUE)
curve(dnorm(x,theta1[1],theta1[2]),add=TRUE,col="blue")
curve(dgamma(x,theta2[1],theta2[2]),add=TRUE,col="red")

```



## Part II: MLE of the Probit model
### Introduction
In the previous questions, we tried to estimate the WTP distribution from their binary purchase decisions and price information. There, we assumed WTP is simply random and is drawn from a distribution every time a customer shows up. Now think about a more general version of the problem in the sense that the WTP actually depends on other factors such as product quality, weather, or whatever. For the sake of exercise, suppose there are two such (continuous) factors **X1** and **X2** that we are able to keep track of. Every customer, at the time of walking into your shop, first observes $X_1$ and $X_2$, then thinks about what his/her WTP is, and finally compare the WTP with the price and make a purchase decision. The whole dataset we will have should look like this:

| Transaction id | Price | X1 | X2 | Purchase |
|---|---|---|---|---|
| 1 | 30 | 1.8 | 24 | FALSE |
| 2 | 26 | 2.5 | 12 | FALSE |
| ... | ... | ... | ... | ... |

### Generating Data
Let's construct a simple linear model that incorporates such dependency on factors $X_1$ and $X_2$:
$$WTP = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \epsilon.$$ 
Here $\epsilon \sim N(0, \sigma^2)$. We next simulate a dataset like the table above.

```{r}
# true parameters
beta0 <- -2
beta1 <- 3
beta2 <- -1
sigma <- 2

set.seed(1234)  # set seed for random number generator
N <- 200  # number of data points
X1 <- runif(N, min=1, max=3)  # X1 ~ Uniform(1, 3)
X2 <- rexp(N, rate=1)  # X2 ~ Exponential(1)
price <- rgamma(N, shape=2, rate=1)  # price ~ Gamma(2, 1)
data <- data.frame(id=1:N, x1=X1, x2=X2, price=price)

# simulate the observations according to the model we built
epsilon <- rnorm(N, mean=0, sd=sigma)  # epsilon ~ N(0, sigma)
wtp <- beta0 + beta1 * data$x1 + beta2 * data$x2 + epsilon
data$purchase <- (wtp >= data$price)
head(data)
```

Finally, we can split the simulated data into two parts (60%--40%). The first part will be the training data, and the second part will be used for testing. Since the data points are independent, we can just take the first 60% as a random training sample.
```{r}
data.train <- data[1:round(N * 0.6), ]
data.test <- data[(round(N * 0.6) + 1):N, ]
```

### Questions and Answers

#### 1. Estimate the model parameters using MLE on the training data. Hint: how to write the likelihood? It should be related to Prob(Purchase == 1) and Prob(Purchase == 0). (1 Mark)

Answer: 

```{r}
datab<-data.train
LL3 <- function(theta, data) {
    beta0 <- theta[1]
    beta1<- theta[2]
    beta2<- theta[3]
    e.beta <- theta[4]
    ll1<- pnorm(datab$price-beta0-beta1*datab$x1-beta2*datab$x2,0,e.beta,lower.tail = TRUE,log.p = TRUE)
    ll2<-pnorm(datab$price-beta0-beta1*datab$x1-beta2*datab$x2,0,e.beta,lower.tail = FALSE,log.p = TRUE)
    ll <-ifelse(datab$purchase=="FALSE",ll1,ll2)
    return(sum(ll))
}
output3 <- optim(c(0,0,0,1), LL3, method="L-BFGS-B", lower=c(-Inf, -Inf,-Inf,1e-6), upper=c(Inf, Inf,Inf,Inf), control=list(fnscale=-1), data=datab)
output3 
theta3 <- output3$par
theta3
```


#### 2. Having obtained the estimate, can you predict ???Purchase??? decision of future customers when Price, X1, and X2 are given? Use the second half of the data (Test) for prediction, and compare the true ???Purchase??? and predicted ???Purchase??? in the Test dataset. (1 Mark)

Answer: 

```{r}
datac<-data.test
datac$pre <- ifelse(datac$price-(theta3[1]+theta3[2]*datac$x1+theta3[3]*datac$x2) >= 0,"FALSE","TRUE")
right <- 0
for (i in 1:80) {
  if (datac$purchase[i]==datac$pre[i]){
      right<-right +1 
     }
}
right
error <- 1-(right/80)
error
```




## Session Info

```{r session-info}
print(sessionInfo(), locale=FALSE)
```
