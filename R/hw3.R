

# version:final

#q17

#思路1：
#随机生成10000个服从beta（0.5,0.8）的随机数。
#然后求均值、标准差。

set.seed(100)
x <- rbeta(n = 10000,shape1 = 0.5,shape2 = 0.8)
mean(x)
mean(x^(-1/3))
sd(x)


#q18

#思路：
#随机生成30000个服从beta（0.5,0.8）随机数.然后求出 X/(1+X)的均值。
#重复以上实验100次，得到100个均值。
#求出100次实验的均值，并且计算标准误，得到标准误差为0.0009376958，小于0.001，满足要求。
#结论：X/(1+X)的均值为0.2394527。


set.seed(1000)
expected_value <- function(x){return(mean(x/(1+x)))}
a <- replicate(n = 100,expr = expected_value(rbeta(n = 30000,shape1 = 0.5,shape2 = 0.8)))
mean(a)
(error <- sqrt(sum((a-mean(a))^2) / (length(a)-1)) )



#q19
set.seed(19)
x <- runif(n = 10000,min = 0,max = 1)
y <- runif(n = 10000,min = 0,max = 1)
(p_B  <- sum(y<sin(pi*x))/length(y))
(P_AB <- sum(y<sin(pi*x) & x < (1/3) ) / sum(y<sin(pi*x)))
(P_BA <- sum(y<sin(pi*x) & x < (1/3) ) / sum(x<(1/3) ))



#20
matrix(data = c('TP','FP','FN','TN'),nrow = 2,byrow = T,
        dimnames = list(c("Real=drunk", "Real=sober"),c("Test=drunk", "Test=sober")))

#Specificity = TN / (FP + TN)=1-0.02=0.98
#Sensitivity = TP / (TP + FN)=0.99

#https://en.wikipedia.org/wiki/Confusion_matrix







#21

x <- matrix(data = c(0.6,0.4,0.2,0.8),nrow = 2,byrow = T,
            dimnames = list(c("Xi=1", "Xi=0"),c("Xi+1=1", "Xi+1=0")))
y <- matrix(c(1,0),nrow = 1)

y %*% x %*% x %*% x 


#22

# 思路：
# 生成1000个服从cauchy(0,1)的随机数。
# 从1000个随机数每次随机抽取2个或者10个或者50个数。然后求出平均值。
# 重复上述实验100次。
# 绘制密度图，查看结果。
# Kolmogorov-Smirnov test


set.seed(2222)
x <- rcauchy(n = 1000,location = 0,scale = 1)

# 模拟过程函数
simulations <- function(data,n){
    m<-replicate(n = 100,expr = mean(sample(data,size = n)))
    return(m)
}

s2 <- simulations(data=x,n=2)
s10 <- simulations(data=x,n=10)
s50 <- simulations(data=x,n=50)

# pdf
plot(density(x),xlim=c(-10,10),ylim=c(0,0.4))
lines(density(s2),col='red')
lines(density(s10),col='blue')
lines(density(s50),col='orange')
legend('topright',legend=c('cauchy(0,1)','n=2','n=10','n=50') ,
        col=c('black','red','blue','orange'),lty=1)


#cdf
par(mfrow=c(2,2))
plot.ecdf(x,xlim=c(-10,10))
plot.ecdf(s2,col='red',main='ecdf(n=2)',xlim=c(-10,10))
plot.ecdf(s10,col='blue',main='ecdf(n=10)',xlim=c(-10,10))
plot.ecdf(s50,col='orange',main='ecdf(n=50)',xlim=c(-10,10))
par(mfrow=c(1,1))



#Kolmogorov-Smirnov Tests
# ks检验的p值大于0.05，说明模拟的结果服从cauchy(0,1)分布。
ks.test(s2,y = 'pcauchy',location = 0,scale = 1)
ks.test(s10,y = 'pcauchy',location = 0,scale = 1)
ks.test(s50,y = 'pcauchy',location = 0,scale = 1)





#23

# 构造H函数
H <- function(x,h,plot=TRUE){
    t <- seq(0,5,by=0.01)
    H <- numeric(length = length(t))
    for (i in 1:length(t)) {
        H[i] <- h^(-1) * (sum( x>t[i] &  x < (t[i] +h) ) / sum( x>t[i])    )
    }
    if(plot){
      plot(t,H,type='l')
    }
    return(data.frame(t,H))
}


# exponential distributions
set.seed(23)
exp1 <- H(x=rexp(n = 10000,rate = 1),h = 0.1,plot=F)
exp2 <- H(x=rexp(n = 10000,rate = 2),h = 0.1,plot=F)
exp0.5 <- H(x=rexp(n = 10000,rate = 0.5),h = 0.1,plot=F)
norm <- H(x=abs(rnorm(10000)),h = 0.1,plot=F)

par(mfrow=c(2,2))
plot(exp1,type='s',col='red',main='lamda=1')
plot(exp2,type='s',col='blue',main='lamda=2')
plot(exp0.5,type='s',col='orange',main='lamda=0.5')
plot(norm,type='s',col='green',main='N(0,1)')
par(mfrow=c(1,1))



##-------------------------------------------------------------------------
#plot(exp1,type='s',col='red',ylim=c(0,2))
#points(exp2,type='s',col='blue')
#points(exp0.5,type='s',col='orange')
#points(norm,type='s',col='green')
#legend('topright',legend=c('lamda=1','lamda=2','lamda=0.5','N(0,1)') ,
#        col=c('red','blue','orange','green'),lty=1)
##-------------------------------------------------------------------------


#24
# http://www.bioconductor.org/packages/release/bioc/html/RBGL.html



