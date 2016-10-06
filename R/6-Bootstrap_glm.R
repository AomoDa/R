##########################################################
# DSC5103 Statistics
# Session 6. Demo of the Bootstrap on Logistic Regression with rare events
# 2016.9
#
##########################################################
library("plyr")
library("ggplot2")


#############################
## simulate data
#############################
# true population parameters
beta0.true <- -1  # also try -4 later for an unbalanced (5% one's) dataset
beta1.true <- 1

# simulate a sample of size N
set.seed(12345)
N <- 500  # sample size
x <- rnorm(N)  # one-dimensional predictor
eta <- beta0.true + beta1.true * x
prob.true <- 1 / (1 + exp(-eta))
y <- rbinom(n=N, size=1, prob=prob.true)  # or y <- (runif(N) < prob.true)
data <- data.frame(x=x, y=y)
summary(data)



#############################
## a quick logistic regression fit
#############################
glm.fitted <- glm(y ~ x, data=data, family=binomial())
summary(glm.fitted)



################ Re-sampling ####################
RUN <- 1000  # number of samples to draw

# data frame for saving the estimates
beta.est <- data.frame(run=1:RUN, beta0.pop=0, beta1.pop=0, beta0.bs=0, beta1.bs=0, beta0.bs2=0, beta1.bs2=0)
head(beta.est)


#############################
### 1. Resample from population (as a benchmark)
# -- to generate many samples from the population (data generating model)
# -- to estimate beta0 and beta1 based on each sample
# -- to evaluate the variability of the estimates across many samples
#############################
set.seed(119)
for (run in 1:RUN) {
    # generate a new sample from population with the SAME x
    x.new <- rnorm(N)  # one-dimensional predictor
    eta.new <- beta0.true + beta1.true * x.new
    prob.new <- 1 / (1 + exp(-eta.new))
    y.new <- rbinom(n=N, size=1, prob=prob.new)
    
    # obtain estimates
    glm.fitted <- glm(y.new ~ x.new, family=binomial())
    beta.est[beta.est$run == run, c("beta0.pop", "beta1.pop")] <- coef(glm.fitted)
}
head(beta.est)

# find mean and sd of the estimates
summarize(beta.est, mean0=mean(beta0.pop), sd0=sd(beta0.pop), mean1=mean(beta1.pop), sd1=sd(beta1.pop))
# plot the distribution of the estimates
ggplot(data=beta.est) + geom_density(aes(x=beta0.pop)) + geom_density(aes(x=beta1.pop)) + theme_bw()



#############################
### 2. Bootstrap by boot()
# -- to generate many samples from **the current sample**
# -- to estimate beta0 and beta1 based on each sample
# -- to evaluate the variability of the estimates across many samples
#############################
library("boot")

# first, need to define the function to obtain estimates
boot.fn <- function(data, index){
    return(coef(glm(y ~ x, data=data, family=binomial(), subset=index)))
}
# test boot.fn on the our sample
boot.fn(data, 1:N)


set.seed(119)
# obtain bootstrap estimates using boot()
?boot
boot.out <- boot(data=data, statistic=boot.fn, R=RUN)
boot.out
str(boot.out)

# save output to the data frame
beta.est[, c("beta0.bs", "beta1.bs")] <- boot.out$t
head(beta.est)

# find mean and sd of the estimates
summarize(beta.est, mean0=mean(beta0.bs), sd0=sd(beta0.bs), mean1=mean(beta1.bs), sd1=sd(beta1.bs))
# plot the distribution of the estimates
ggplot(data=beta.est) + geom_density(aes(x=beta0.pop)) + geom_density(aes(x=beta1.pop)) + theme_bw() + 
    geom_density(aes(x=beta0.bs), color="blue") + geom_density(aes(x=beta1.bs), color="blue")



#############################
### 3. Bootstrap manually
# -- to manually generate many samples from **the current sample**
# -- to estimate beta0 and beta1 based on each sample
# -- to evaluate the variability of the estimates across many samples
#############################
set.seed(119)
for (run in 1:RUN) {
    # generate an index for the bootstrap sample **WITH REPLACEMENT**
    index <- sample(N, N, replace=TRUE)

    # obtain estimates
    glm.fitted <- glm(y ~ x, data=data, family=binomial(), subset=index)
    beta.est[beta.est$run == run, c("beta0.bs2", "beta1.bs2")] <- coef(glm.fitted)
}
head(beta.est)

# find mean and sd of the estimates
summarize(beta.est, mean0=mean(beta0.bs2), sd0=sd(beta0.bs2), mean1=mean(beta1.bs2), sd1=sd(beta1.bs2))
# plot the distribution of the estimates
ggplot(data=beta.est) + geom_density(aes(x=beta0.pop)) + geom_density(aes(x=beta1.pop)) + theme_bw() + 
    geom_density(aes(x=beta0.bs), color="blue") + geom_density(aes(x=beta1.bs), color="blue") + 
    geom_density(aes(x=beta0.bs2), color="red") + geom_density(aes(x=beta1.bs2), color="red")



### zoom-in view of the two plots
# beta0
ggplot(data=beta.est) + geom_density(aes(x=beta0.pop))  + geom_density(aes(x=beta0.bs), color="blue")  + geom_density(aes(x=beta0.bs2), color="red") + theme_bw()
# beta1
ggplot(data=beta.est) + geom_density(aes(x=beta1.pop))  + geom_density(aes(x=beta1.bs), color="blue")  + geom_density(aes(x=beta1.bs2), color="red") + theme_bw()

# compare the sd of estimates
summarize(beta.est, sd.pop=sd(beta0.pop), sd.bs=sd(beta0.bs), sd.bs2=sd(beta0.bs2))
summarize(beta.est, sd.pop=sd(beta1.pop), sd.bs=sd(beta1.bs), sd.bs2=sd(beta1.bs2))

