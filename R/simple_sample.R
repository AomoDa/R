

#a

brncntr <- dat$brncntr

set.seed(100)

foreign_sample <- function(x,sample_size=length(x)) {
  a <- sample(x,size=sample_size,replace=T)
  return(mean(a==2))
}
p <- replicate(10000,foreign_sample(brncntr))

# %95 CI
quantile(p,c(0.025,0.975))

#more than one-tenth of the inhabitants in Sweden were foreign born
mean(p >0.10)


#b
p500 <- replicate(10000,foreign_sample(brncntr,sample_size=500))

# %95 CI
quantile(p500,c(0.025,0.975))
#more than one-tenth of the inhabitants in Sweden were foreign born
mean(p500 >0.10)

#c

# 99% CI
quantile(p500,c(0.005,0.995))
