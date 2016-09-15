
#-----------------------------------------------------------------------------------------------
# Part a
#-----------------------------------------------------------------------------------------------
data(ChickWeight)
weight <- ChickWeight$weight # 将weight数据提取出来，作为一个vector，这样方便操作一点。

# summary
mean(weight)
var(weight)
sd(weight)
hist(weight,freq = F,ylim = c(0,0.01),density = 20,breaks=20)
lines(density(weight),col='red',lwd=2)

# Quartile and Boxplot
quantile(weight)
boxplot(weight,col=3)
title(main = 'boxplot of weight')


# qqplot and Shapiro-Wilk normality test
qqnorm(weight)
qqline(weight)

shapiro.test(weight)


#-----------------------------------------------------------------------------------------------
# Part b
#-----------------------------------------------------------------------------------------------

## Jackknife 
r <- length(weight) # 分组组数
j_sd <- numeric(r) # 建立空数组，为了保存抽样值
j_md <- numeric(r) # 建立空数组，为了保存抽样值

for (i in 1:r) {
    j_sd[i] <-sd(weight[-i])
    j_md[i] <-median(weight[-i])
}

# 计算 bias
(bias_jk_sd <- (r-1) * (mean(j_sd)-sd(weight))) # 计算 the standard deviation的 bias
(bias_jk_sd <- (r-1) * (mean(j_md)-median(weight))) # 计算 the median的bias

#-----------------------------------------------------------------------------------------------
##Bootstrap

n <- length(weight) # 每组元素数量
B <- 2000 # 分组组数
b_sd <- numeric(r) # 建立空数组，为了保存抽样值
b_md <- numeric(r) # 建立空数组，为了保存抽样值

set.seed(1)  # 设定随机种子，可以使得结果重现。
# 模拟Bootstrap，重复2000次。
for (i in 1:B) {
    ind <- sample(x = 1:n,size = n,replace = T)
    b_sd[i] <- sd(weight[ind])
    b_md[i] <- median(weight[ind])
}

# 计算 bias
(bias_bs_sd <- mean(b_sd -sd(weight)))  # 计算 the standard deviation的 bias
(bias_bs_md <- mean(b_md -median(weight))) # 计算 the median的bias


#-----------------------------------------------------------------------------------------------
# Part c
#-----------------------------------------------------------------------------------------------

library(HH)
library(gplots)

#这里a 是个魔性代码。。。仅仅是为了绘制图的时候使用。。。没有任何计算的意义
a <- c(c(rep(' ',6),'1',rep(' ',5)),
   c(rep(' ',6),'2',rep(' ',5)),
   c(rep(' ',6),'3',rep(' ',5)),
   c(rep(' ',6),'4',rep(' ',5)))


# 绘制均值图
plotmeans( weight~ interaction(Time , Diet, sep ="   "),
           connect=list(1:12,13:24,25:36,37:48),
           barwidth=2,
           col="dark green",
           data=ChickWeight,n.label=F,legend=a,xlab=''
)
abline(v=c(13,25,37), lty=2,col='red')


ancova(weight~Diet*Time,data=ChickWeight)

aggregate(weight~Diet,data=ChickWeight[ChickWeight$Time==0,],FUN = mean)
summary(aov(weight~Diet,data=ChickWeight[ChickWeight$Time==0,]))
aggregate(weight~Diet,data=ChickWeight[ChickWeight$Time==21,],FUN = mean)
summary(aov(weight~Diet,data=ChickWeight[ChickWeight$Time==21,]))



