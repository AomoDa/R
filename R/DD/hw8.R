


# 58, 59,  65 with λ=5,n=10 , 
# 66, 67 (2 points each) and  63, 69, 70  (5 points each) 


#Q58






#Q59

##a
par(mfrow=c(1,2))
boxplot(Bangladesh$Arsenic,main='boxplot of Arsenic')
hist(Bangladesh$Arsenic)
par(mfrow=c(1,1))


##b
bootstrap_mean <- replicate(10000,mean(sample(Bangladesh$Arsenic,nrow(Bangladesh),replace=T)))
par(mfrow=c(1,2))
boxplot(bootstrap_mean,main='boxplot of bootstrap_mean')
hist(bootstrap_mean)
par(mfrow=c(1,1))


#Q60
t <- seq(.01,8,by = .01)
log.mylikeli.exp <- function(lambda,x){ length(x)*log(lambda) - lambda*sum(x) }
set.seed(60)
x1 <- rexp(10,rate = 5)
y.2 <- sapply(t,function(t){log.mylikeli.exp(t,x1)})
plot(t,y.2, xlab = 'lambda', ylab = 'loglikelihood', type = 'l', col='red',
   main='loglikelihood function for \n a sample from an exponential distribution')

t[which.max(y.2)]
