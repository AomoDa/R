
#58, 59,  65 (lambda=5,n=10),66,67,63,69,70


# 58, 59,  65 with λ=5,n=10 , 
# 66, 67 (2 points each) and  63, 69, 70  (5 points each) 


#Q58


chisq_value <- function(tb){
  ex <- outer(rowSums(tb),colSums(tb))/sum(tb)
  return(sum( (tb-ex)^2/ex))
}
N <- 1e4-1
chq <- numeric(N)
for (i in 1:N) {
 chq[i] <- chisq_value(table(Problem59$X,sample(Problem59$Y)))
}
chisq_value(table(Problem59$X,Problem59$Y))
pvale <- (sum(chq >=72)+1)/(N+1)
pvale

#Q59

##a
Bangladesh <- read.csv('Bangladesh.csv')
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




#Q63

	



#Q69

log_cau_mle <- function(theta,x){
  -length(x) *log(pi) - sum(log(1+(x-theta)^2))
}
set.seed(690)


N <-100 #100 random samples
theta <- numeric(N)
for (i in 1:N) {
x0 <- rcauchy(n = 10,location = 0)
et <- seq(-30,30,by = 0.01)
#Compute the log likelihoods for a range of x0
cau_v <- sapply(et,function(et){log_cau_mle(et,x0)})
theta[i] <- et[which.max(cau_v)]
}
plot(et,cau_v,type='l',
   main='log mle for Cauchy distributions  \n with location paramete=0',
   xlab='location paramete',ylab='log mle ')
hist(theta)
mean(theta);max(theta)


#Q70



mat_theta_trim_mean <- matrix(NA,ncol = 4,nrow = 10000,
   dimnames=list(1:10000,c('n10','n20','n40','n100')))
mat_theta_median <- matrix(NA,ncol = 4,nrow = 10000,
   dimnames=list(1:10000,c('n10','n20','n40','n100')))

for (i in 1:4) {
 N <- c(10,20,40,100)
 mat_theta_trim_mean[,i] <- replicate(10000,mean(rcauchy(N[i]),trim = 0.10))
 mat_theta_median[,i] <-replicate(10000,median(rcauchy(N[i])))
}

colMeans(mat_theta_trim_mean)
colMeans(mat_theta_median)

par(mfrow=c(2,2))
hist(mat_theta_trim_mean[,1],main='trimmed mean estimated \n n=10')
hist(mat_theta_trim_mean[,2],main='trimmed mean estimated \n n=20')
hist(mat_theta_trim_mean[,3],main='trimmed mean estimated \n n=40')
hist(mat_theta_trim_mean[,4],main='trimmed mean estimated \n n=100')
par(mfrow=c(1,1))


par(mfrow=c(2,2))
hist(mat_theta_median[,1],main='median estimated \n n=10')
hist(mat_theta_median[,2],main='median estimated \n n=20')
hist(mat_theta_median[,3],main='median mean estimated \n n=40')
hist(mat_theta_median[,4],main='median mean estimated \n n=100')
par(mfrow=c(1,1))
