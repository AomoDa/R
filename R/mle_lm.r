
#  Demo of Maximum Likelihood Estimation



#############################
### Demo 1. Estimating normal mean and standard deviation
### -- this numerical MLE procedure is really not necessary because we have already obtained the analytical estimation. this is purely for demo purpose.
#############################

## simulate a sample of N data points from population N(3, 2)
set.seed(12345)  # set seed for random number generator
N1 <- 100
data1 <- data.frame(x=rnorm(N1, mean=3, sd=2))
plot(data1$x)

## Theoretical estimates by MLE
theta0 <- c(mean(data1$x), sqrt(sum((data1$x - mean(data1$x))^2) / N1))


## Numerical MLE using the sample
# Step 1. formulate the log-likelihood function
# !!! in the logL function, the first argument is parameters to estimate, and the second is data
LL1 <- function(theta, data) {
    mu <- theta[1]
    sigma <- theta[2]
    ll <- dnorm(data$x, mean=mu, sd=sigma, log=TRUE)
    return(sum(ll))
}

# Step 2. invoke optim() to find the optimal parameters
?optim
# !!! optim() requires initial value of the parameters
# !!! also be careful of the range of each parameter, e.g., variance cannot be negative. specify ranges by specified by "lower" and "upper"
# !!! by default, optim() does minimization, adding "control=list(fnscale=-1)" turns it to maximization
output1 <- optim(c(0, 1), LL1, method="L-BFGS-B", lower=c(-Inf, 1e-6), upper=c(Inf, Inf), 
                control=list(fnscale=-1), data=data1)
output1 # optimal parameters are in output1$par
theta1 <- output1$par
theta1



#############################
### Demo 2. Estimating a linear function
#############################

## simulate a sample of N data points from population y = 2 + 3x + epsilon
set.seed(54321)  # set seed for random number generator
N2 <- 100
x2 <- runif(N2)  # say x is uniformly drawn from [0,1]
epsilon <- rnorm(N2)  # epsilon is N(0, 1)
y2 <- 2 + 3 * x2 + epsilon
plot(x2, y2)
abline(a=2, b=3, col="red")
data2 <- data.frame(x=x2, y=y2)

## MLE using the sample
# Step 1. formulate the log-likelihood function
# !!! in the logL function, the first argument is parameters to estimate, and the second is data
LL2 <- function(theta, data) {
    beta0 <- theta[1]
    beta1 <- theta[2]
    sigma <- theta[3]
    x <- data$x
    y <- data$y
    ll <- dnorm(y - beta0 - beta1 * x, mean=0, sd=sigma, log=TRUE)
    return(sum(ll))
}

# Step 2. invoke optim() to find the optimal parameters
# !!! optim() requires initial value of the parameters
# !!! also be careful of the range of each parameter, e.g., variance cannot be negative. specify ranges by specified by "lower" and "upper"
# !!! by default, optim() does minimization, adding "control=list(fnscale=-1)" turns it to maximization
output2 <- optim(c(0, 0, 1), LL2, method="L-BFGS-B", lower=c(-Inf, -Inf, 1e-6), upper=c(Inf, Inf, Inf), 
                control=list(fnscale=-1), data=data2)
output2 # optimal parameters are in output2$par
output2$par


## compare MLE with linear regression (Least Square estimate)
regression <- lm(y ~ x, data=data2)
summary(regression)
# it seems the estimate of sigma is different in OLS and MLE. this is due to /(N-1) versus /N.


#############################
### Demo 3. Estimating normal mean and standard deviation with censored observations
#############################

## use the same sample as in Demo 1: N1 data points from population N(3, 2)
data3 <- data1
data3$T <- runif(N1, min=3, max=5)  # the censoring thresolds
data3$x.censored <- pmin(data3$x, data3$T)  # censored observation of x is min(x, T)
plot(data3$x.censored)
mean(data3$x.censored)
sd(data3$x.censored)

## MLE using the sample
# Step 1. formulate the log-likelihood function
# !!! in the logL function, the first argument is parameters to estimate, and the second is data
LL3 <- function(theta, data) {
    mu <- theta[1]
    sigma <- theta[2]
    ll1 <- dnorm(data$x.censored, mean=mu, sd=sigma, log=TRUE)
    ll2 <- pnorm(data$x.censored, mean=mu, sd=sigma, lower.tail=FALSE, log.p=TRUE)
    ll <- ifelse(data$x.censored < data$T, ll1, ll2)
    return(sum(ll))
}

# Step 2. invoke optim() to find the optimal parameters
# !!! optim() requires initial value of the parameters
# !!! also be careful of the range of each parameter, e.g., variance cannot be negative. specify ranges by specified by "lower" and "upper"
# !!! by default, optim() does minimization, adding "control=list(fnscale=-1)" turns it to maximization
output3 <- optim(c(0, 1), LL3, method="L-BFGS-B", lower=c(-Inf, 1e-6), upper=c(Inf, Inf), 
                control=list(fnscale=-1), data=data3)
output3 # optimal parameters are in output3$par
theta3 <- output3$par
theta3

# compare with the estimates under perfect observation
theta1


# Demo of Maximum Likelihood Estimation of the Logit model



## prepare data
# load data
data <- read.csv("MLE_Logit.csv", header=TRUE)
summary(data)

# constants for list price and unit cost
MSRP <- 25000
Cost <- 15000

# Convert absolute price levels into percentage of MSRP, which will be used as an independent variable in the analysis.
data$Percentage <- data$Price / MSRP
head(data)


## Data Exploration
hist(data$Percentage)



#############################
### 1. Single segment, price only
#############################

## MLE
# define a function to calculate Log-likelihood of the data.
LL1 <- function(theta, data) {
    a <- theta[1]
    b <- theta[2]
    choice.prob <- 1 / (1 + exp(- a - b * data$Percentage))
    ll <- log(choice.prob * data$Win + (1 - choice.prob) * (1 - data$Win))
    return(sum(ll))
}

# Optimize for the most likely parameter estimate, starting from an arbitrary number a=0 and b=0.
output1 <- optim(c(0, 0), LL1, method="L-BFGS-B", control=list(fnscale=-1), data=data)
output1
a1 <- output1$par[1]
b1 <- output1$par[2]
ll1 <- output1$value


## Alternatively, by Logistic Regression.
glm(Win ~ Percentage, data=data, family=binomial(link="logit"))


## Given the estimates of a and b, what is the optimal price that should be offered?
EProfit1 <- function(percent) {
    return ((percent * MSRP - Cost) / (1 + exp(- a1 - b1 * percent)))
}

output1b <- optim(1, EProfit1, method="L-BFGS-B", lower=0, upper=1, control=list(fnscale=-1))
output1b
percent1 <- output1b$par
profit1 <- output1b$value



#############################
### 2. Two segments, price only
#############################

# add a column "Segment", which indicates whether the data record is from the "Police" segment (row 1 to 2000) or "Corporate" segment (row 2001 to 4000).
data$Segment <- "Police"
data[2001:4000, "Segment"] <- "Corporate"
data$Segment <- as.factor(data$Segment)
summary(data)

## MLE for Police
output.p2 <- optim(c(0, 0), LL1, method="L-BFGS-B", control=list(fnscale=-1), data=subset(data, Segment == "Police"))
output.p2
a.p2 <- output.p2$par[1]
b.p2 <- output.p2$par[2]

## MLE for Corporate
output.c2 <- optim(c(0, 0), LL1, method="L-BFGS-B", control=list(fnscale=-1), data=subset(data, Segment == "Corporate"))
output.c2
a.c2 <- output.c2$par[1]
b.c2 <- output.c2$par[2]

## Alternatively, the above MLE can be replaced by two Logistic Regressions.
glm(Win ~ Percentage, data=subset(data, Segment == "Police"), family=binomial(link="logit"))
glm(Win ~ Percentage, data=subset(data, Segment == "Corporate"), family=binomial(link="logit"))
# or
glm(Win ~ Percentage * Segment, data=data, family=binomial(link="logit"))



#############################
### 3. Two segments, price + units (segment independent)
#############################

## MLE
LL3 <- function(theta, data) {
    a.p <- theta[1]
    b.p <- theta[2]
    a.c <- theta[3]
    b.c <- theta[4]
    c <- theta[5]
    choice.prob.p <- 1 / (1 + exp(- a.p - b.p * data$Percentage - c * data$Units))
    choice.prob.c <- 1 / (1 + exp(- a.c - b.c * data$Percentage - c * data$Units))
    ll.p <- log(choice.prob.p * data$Win + (1 - choice.prob.p) * (1 - data$Win))
    ll.c <- log(choice.prob.c * data$Win + (1 - choice.prob.c) * (1 - data$Win))
    ll <- ifelse(data$Segment == "Police", ll.p, ll.c)
    return(sum(ll))
}

output3 <- optim(c(0, 0, 0, 0, -1), LL3, method="L-BFGS-B", control=list(fnscale=-1), data=data)
output3
a.p3 <- output3$par[1]
b.p3 <- output3$par[2]
a.c3 <- output3$par[3]
b.c3 <- output3$par[4]
c3 <- output3$par[5]
ll3 <- output3$value


## Alternatively, by Logistic Regression
glm(Win ~ Percentage * Segment + Units, data=data, family=binomial(link="logit"))


