
# r+dist---产生随机数
# d+dist---概率密度函数，密度函数
# p+dist---F(x)
# q+dist---分位数

set.seed(1) # 随机种子 ，设定种子之后，随机的结果可以重现。

# 生成随机数
# rnorm()   
rnorm(n = 100,mean = 10,sd=1)

# rf()
rf(n = 10,df1 = 2,df2 = 3)




# 密度函数 f(x)
# d+dist---概率密度函数，密度函数

dnorm()

dnorm(x=0,mean = 0,sd=1)

dnorm(x=1,mean = 0,sd=1)
dnorm(x=-1,mean = 0,sd=1)


# F(x)
# pnorm()

pnorm(q = 5,mean=0,sd=1)
pnorm(q = 3,mean=0,sd=1)
pnorm(q = 0,mean=0,sd=2)


# 分位数
# qnorm()
pnorm(q=0.8416212,mean = 0,sd=1)





# Normal distribution
# Gaussian distribution

x <- seq(-5,5,length.out=100)
y <- dnorm(x,0,1)
  
plot(x,y,col="red",xlim=c(-5,5),ylim=c(0,1),type='l',lwd=2)
lines(x,dnorm(x,0,0.5),col="green",lwd=2)
lines(x,dnorm(x,0,2),col="blue",lwd=2)
lines(x,dnorm(x,-2,1),col="yellow",lwd=2)
legend("topright",legend=paste("m=",c(0,0,0,-2)," sd=", c(1,0.5,2,1)), lwd=1, col=c("red", "green","blue","yellow"))



# boot

table(sample(x=1:6,size = 120000,replace = T,prob = rep(1/6,6)))

# 中心极限定理
# 大数定理