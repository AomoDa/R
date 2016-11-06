############## Diary of 11/1/16 ###############

# Import GSS2006 data

GSS2006 <- read.csv("../Data/GSS2006.csv", header=TRUE, stringsAsFactors=FALSE)

# Gun Ownership and views towards death penalty

table(GSS2006$OwnGun) 
table(GSS2006$DeathPenalty) 
table(GSS2006$DeathPenalty, GSS2006$OwnGun) -> mytable2

# Look at cross tabulation

mytable2

# Do a chi squared test:

Z <- chisq.test(mytable2)

# Here is the output of this test. 

Z

# However, R produces a lot more information.

names(Z)

Z$expected   # expected cell counts

Z$observed   # observed counts

Z$residuals  # Pearson residuals

Z$stdres     # standardized residuals

# Plug In Estimators

# Bias 
# We have two possible estiamtors of  b  where  X ~U(0,b), b unknown.
# One is 2 times the mean. 
# The other is the maximum of the sample.

b1 = replicate(10000, max(runif(5))) # simulate the maximum
b2 = replicate(10000, 2*mean(runif(5))) # simulate mean times 2

hist(b1,prob = T)
mean(b1)  # clearly this is biased (too small)
sd(b1)    

hist(b2,prob = T)
mean(b2) # looks like this is unbiased
sd(b2)   # larger than that of b1!!

# If we can find a way to correct the bias of b1,
# it will be a better estimator, since its standard deviation is smaller.

#########  Bootstrap  #####

# Define a sample with which to illustrate bootstrap concepts.   

x0 = c(1,2,4,6,10)

# Here is one bootstrap sample (with replacement, same size as original sample) 

boot.x = sample(x0, size = length(x0), replace = T)
boot.x

# We can compute statistics for a bootstrap sample
# just like we can do this for the original sample.
# For example the mean:

mean(sample(x0, size = length(x0), replace = T))

# Do this 10,000 times and record all the boostrap means.

bootstrap.x0 = replicate(100000, mean(sample(x0, size = length(x0), replace = T)) )

# Fraction where the mean is 1.4.
# We worked out that this can only happen if the sample is 
# c(1,1,1,2,2) or some permutation.

mean(bootstrap.x0==1.4)

# We want to find the probability that a bootstrap sample contains 1.    

bootstrap1.x0 = replicate(100000, min(sample(x0, size = length(x0), replace = T)) )

mean(bootstrap1.x0 == 1)

# About 67%.

# Find thr probability that a bootstrap sample contains all 5 numbers.
# Define a function that returns TRUE if this is the case and FALSE otherwise.

mybootfunc = function(y){
  y = sort(y)
  return(sum(abs(y-x0)) == 0)
}

# Now apply it 100,000 times to bootstrap samples. 

bootstrap2.x0 = replicate(100000, mybootfunc(sample(x0, size = length(x0), replace = T)) )

mean(bootstrap2.x0)

# Only a few percent.
# In these cases, any statistic  that we compute equals the value of the 
# statistic for the original sample.

# Illustrate distribution properties of bootstrap statistic.
# Typically, the bootstrap distribution is very similar
# to the distribution of this statistic if 
# fresh samples are taken from the distribution itself.

set.seed(123)

x <- rnorm(25)

# N = 10000 bootstrap samples of sample mean

N = 10000

z <- replicate(N, mean(sample(x,length(x), replace = T)))

# The mean of the bootstrap means is the original sample mean.

mean(z)
mean(x)

qqnorm(z) # distributed approximatelty normally 

plot.ecdf(z)  # empirical cdf of bootstrapped mean 

# define the theoretical cdf of the sample mean of 25 observations from N(0,1)

ecdf.theory <- function(x) pnorm(x,0,1/sqrt(25))

# Plot it on top of bootstraped cdf: 

xx <- seq(-1,1,by=.01) 
lines(xx,ecdf.theory(xx), col = 2, lwd = 2)

# Fairly close! 
# Repeat this with other seeds.

# Estimating the bias of a plug-in estimator using the bootstrap

# Plug-in estimator of Variance:

myvar = function(y){ mean((y-mean(y))^2)}

# Correct variance

var(x)

# Plug-in estimate:

myvar(x)

# This is too small. 
# We can AUTOMATICALLY correct this with the bootstrap.

# Bootstrap myvar:

z <- replicate(N, myvar(sample(x,length(x), replace = T)))

# This shows us how myvar (plug-in estimator) underestimates the true variance.

mean(z)
myvar(x)

# The values differ, showing the bias.

bias.est = (myvar(x) - mean(z))

bias.est

# Bias correction

myvar(x) + bias.est

# Compare to unbiased (theoretically correct) estimate:

var(x)

# very close. This allows us to correct the bias without knowing 
# the correct formula. 

# Confidence intervals for mean repair times 
# for Verizon data, using the bootstrap:  

Verizon <- read.csv("../Data/Verizon.csv")

ilec <- Verizon$Time[Verizon$Group=="ILEC"]
hist(ilec)
mean(ilec)

# This is an estimate of the mean repair time for ILEC customers.
# How accurate is it? 
# Make a bootstrap confidence interval.   

# First simulate boostrap means many times. 

z <- replicate(10000, mean(sample(ilec, length(ilec), replace = T)))
hist(z,breaks =30)

quantile(z, c(.025, .975))

# This is an interval that contains the true mean 
# with approximately 95% probability    

# Do the same thing with the CLEC data. There are only 23 data points.   

clec <- Verizon$Time[Verizon$Group=="CLEC"]
mean(clec)
hist(clec)

# bootstrap sample of population mean: 

z1 <- replicate(10000, mean(sample(clec, length(clec), replace = T)))
hist(z1, prob = T, breaks = 30) # quite skewed
qqnorm(z1) # not normally distributed

# Here is a 95% confidence interval.
# It is much larger than the one for the ILEC customers
# since the sample size is much smaller.

quantile(z1,c(.025,.975))

# Note that the confidence interval is not symmetric about the sample mean.
# The sample mean is off to the left. 

mean(clec) # actual sample mean
mean(z1)  # bootstraped mean which is close to the actual sample mean  

# Consider mean ages of Titanic victims and survivors.

# Load the Titanic data.

Titanic <- read.csv("../Data/Titanic.csv")

head(Titanic)

# Make side-by-side box plots, 
# compute the difference of mean ages for the two groups.
# mean age of survivors is about 4.5 years below that of victims. 

boxplot(Age ~ Survived, data = Titanic)
diff(tapply(Titanic$Age, Titanic$Survived, FUN = mean))

# Test the null hypothesis "mean ages are the same" against the alternative
# "mean age of survivors is lower"using a permutation test.

# To prepare for permutation test, extract all age data

titanic.all <- Titanic$Age

N=100000 # Number of permutations

# Function to make a single permutation (i.e. pick 135 survivors at random) 
# and compute the difference of mean ages

titanicperm <- function(){index <- sample(658,135, rep = F)
return(mean(titanic.all[index]) - mean(titanic.all[-index]))}

# Simulation of null distribution:

z <- replicate(N,titanicperm())

hist(z, breaks = 50, prob = T)
qqnorm(z)

# This is essentially normally distributed.

mean(z < -4.53) # P value of one-sided alternative "mean difference is positive".

# Reject the null hypothesis in favor of the one-sided alternative "mean differences positive."

# Prepare for bootstrap test of the same question:
# Are the main ages of survivors and victims different?

# This will now be examined with confidence intervals.

# Extract age data for survivors and victimes separately.
# This is necessary since we will NOT simulate a null distribution. 

titanic.victim <- Titanic$Age[Titanic$Survived == 0]
titanic.survivor <- Titanic$Age[Titanic$Survived == 1]
hist(titanic.victim)
hist(titanic.survivor,breaks = 20)

# The histogram reveals that many very young children (ages under 10) survived.

mean(titanic.survivor) - mean(titanic.victim)

# Write a function that simulates a single bootstrap value 
# of the difference of means.

titanicbootstrap <- function(){
  return(mean(sample(titanic.survivor,135, replace = T))
         - mean(sample(titanic.victim,523,replace = T)))
}

# The same function, more generic.
# This one will take any pair of two independent samples

diff.bootstrap <- function(x,y){
  return(mean(sample(x,length(x), replace = T))
         - mean(sample(y,length(y),replace = T)))
}

# Make bootstrap sample of difference of means, in two different ways.

z1 <- replicate(N, titanicbootstrap())
z11 = replicate(N, diff.bootstrap(titanic.survivor, titanic.victim))

# Histogram and quantile quantile plot show that the distribution of 
# bootstrap differences of means is close to normal.

hist(z1, breaks = 40, prob = T)
qqnorm(z1)

# The mean of these bootstrapped differences is essentially 
# the same as the original difference of means. 

mean(z1)

# Make several confidence intervals of the difference in means.

quantile(z1, c(.05,.95)) # This is a 90% confidence interval 

quantile(z1, c(.025,.975)) # This is a 95% confidence interval

quantile(z11, c(.005,.995))  # This is a 99% confidence interval

# With very high confidence, we can say that the difference of means 
# is negative and not zero.

################  End of diary of 11/1/16  ###########
