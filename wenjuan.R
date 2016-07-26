
library(foreign)
library(klaR)
library(arules)
library(arulesViz)
library(car)
library(plyr)

# 定义读取数据函数，并对数据重命名。
data_get <- function(){
require(foreign)
x <- read.spss(file = 'spss.sav',to.data.frame = T,use.value.labels = F)
x <- data.frame(x[,-1])
x <- as.data.frame(apply(X = x,MARGIN = 2,function(x) as.numeric(x) ))
names(x) <- c('社区餐厅','送餐服务','上门做饭','社区药店','康复理疗','家庭医生','家庭病床','生活照料','帮助洗澡','打扫卫生','帮助购物','代办代缴','上门维修','外出接送_陪同散步','老年学校','安全呼救','结对关怀','文化娱乐','锻炼健身')
return(x)
}

x <-data_get() # 读取数据

# 分类数据，并且对数据打上标签
data_factor <- as.data.frame(apply(X = x,MARGIN = 2,function(x) factor(x = x,levels = 1:5,labels = c('非常不需要','不需要','视情况而定','需要','非常需要'))))
str(data_factor,list.len=5)
data_factor_new <- as.data.frame(apply(X = data_num,MARGIN = 2,function(x)car::recode(x," c(-1,0)='不需要';1='视情况而定';c(2,3)='需要' ") ))
str(data_factor_new,list.len=5)

# 描述性统计分析 量化数据分析

#将分类数据数值化。非常不需要=-1分，不需要=0分，视情况而定=1分，需要=2分，非常需要=3分
data_num <- x - 2



## 箱图和条形图

bar_num <- function(data){
bar_num<- colMeans(data)
bar_num<-bar_num[order(bar_num,decreasing = F)]
bar_ok <- data.frame(num=as.vector(bar_num),names=names(bar_num))
return(bar_ok)
}

##总体分析
par(las=2)
barplot(bar_num(data_num)[,1],horiz = T,main='总体需求分数条形图',cex.names=0.7,names.arg=bar_num(data_num)[,2],density = 20,xlim=c(0,2))
abline(v=0.5,lty=2,col='orange',lwd=2)
abline(v=1,lty=2,col='red',lwd=2)
abline(v=1.5,lty=2,col='blue',lwd=2)
par(las=1)

###不同就餐协助方式需求程度分布
par(mfrow=c(2,1))
boxplot(data_num[,1:3],ylab='需求分数',main='不同就餐协助方式需求分数箱形图')
par(las=2)
barplot(bar_num(data_num[,1:3])[,1],horiz = T,main='不同就餐协助方式需求分数条形图',cex.names=0.7, names.arg=bar_num(data_num[,1:3])[,2],density = 20)
par(mfrow=c(1,1),las=1)

###不同医疗服务的需求程度分布

par(mfrow=c(2,1))
boxplot(data_num[,4:7],ylab='需求分数',main='不同医疗服务需求分数箱形图')
par(las=2)
barplot(bar_num(data_num[,4:7])[,1],horiz = T,main='不同医疗服务需求分数条形图',cex.names=0.7,names.arg=bar_num(data_num[,4:7])[,2],density = 20)
par(mfrow=c(1,1),las=1)

###不同生活服务的需求程度分布
par(mfrow=c(2,1))
boxplot(data_num[,8:14],ylab='需求分数',main='不同生活服务需求分数箱形图')
par(las=2)
barplot(bar_num(data_num[,8:14])[,1],horiz = T,main='不同生活服务需求分数条形图',cex.names=0.6,names.arg=bar_num(data_num[,8:14])[,2],density = 20)
par(mfrow=c(1,1),las=1)

###精神文化与健康服务
par(mfrow=c(2,1))
boxplot(data_num[,15:19],ylab='需求分数',main='不同就餐精神文化与健康服务分数箱形图')
par(las=2)
barplot(bar_num(data_num[,15:19])[,1],horiz = T,main='不同精神文化与健康服务需求分数条形图',cex.names=0.7,names.arg=bar_num(data_num[,15:19])[,2],density = 20)
par(mfrow=c(1,1),las=1)






#apriori关联分析


rules <- apriori(data = data_factor_new,parameter = list(supp=0.3,conf=0.8,target='rules',minlen=2),appearance = list(rhs=c('安全呼救=需要','上门维修=需要','社区药店=需要' ),default='lhs' ))
quality(rules) <- round(quality(rules),2)
r<-sort(rules,by='lift')
inspect(r)

plot(r,method ='grouped',control=list(col=gray(1:10/10)))
plot(r,method ='matrix',control=list(col=gray(1:10/10)))


rules_q <- apriori(data = data_factor_new,parameter = list(supp=0.3,conf=0.8,target='rules',minlen=2),appearance = list(lhs=c('安全呼救=需要','上门维修=需要','社区药店=需要' ),default='rhs' ))
quality(rules_q) <- round(quality(rules_q),2)
r_q<-sort(rules_q,by='lift')
inspect(r_q)

plot(r_q,method ='grouped',control=list(col=gray(1:10/10)))
plot(r_q,method ='matrix',control=list(col=gray(1:10/10)))

# k-modes 聚类分析

set.seed(123)
km5 <- kmodes(data = data_factor_new,modes = 5,iter.max = 10)
km6 <- kmodes(data = data_factor_new,modes = 6,iter.max = 10)
km7 <- kmodes(data = data_factor_new,modes = 7,iter.max = 10)
km8 <- kmodes(data = data_factor_new,modes = 8,iter.max = 10)


better_model <- function(){
final_target<-function(data,model) {
x <- data
x$model <- model$cluster
b <- NA
for (i in 1:length(model$size)) {
d_one <- function(x){
a<-sum(model$modes[i,1:19]==x[1:19])
ad <- ((19-a)*2) / (38-a)
return(ad)}
xx <- x[x$model==i,]
b[i]<-sum(apply(X = xx,MARGIN = 1,FUN = d_one))
}
return(sum(b))
}
x <- data.frame(
km5=c(final_target(data = data_factor_new,model = km5),sum(km5$withindiff)),
km6=c(final_target(data = data_factor_new,model = km6),sum(km6$withindiff)),
km7=c(final_target(data = data_factor_new,model = km7),sum(km7$withindiff)),
km8=c(final_target(data = data_factor_new,model = km8),sum(km8$withindiff)),
row.names=c('相异度测量值','k_modes目标函数值')
)
return(round(x,0))
}

better_model()
km7$size
km7$modes


km_plot_sum <- function() {
four<-function(x){
y<-data.frame(就餐方面=mean(as.numeric(x[1:3])), 医疗方面=mean(as.numeric(x[4:7])),生活方面=mean(as.numeric(x[8:14])),社区服务=mean(as.numeric(x[15:19])))
return(y)
}
require(plyr)
x <- data_num
x$km <- km7$cluster
km_fac<-ddply(.data = x,.variables = .(km),function(x) apply(x,2,mean))[,1:19]
km_f<-round(rbind(four(km_fac[1,]),four(km_fac[2,]),four(km_fac[3,]),four(km_fac[4,]),four(km_fac[5,]),four(km_fac[6,]),four(km_fac[7,])),2)
barplot(t(km_f),beside = T,horiz = T,density = c(10,20,30,40),main='kmodes分类结果对比')
abline(v = 1,col='red',lty=2)
abline(v = 1.5,col='blue',lty=2)
legend('topright',legend = c('就餐方面','医疗方面','生活方面','社区服务'),density = c(10,20,30,40))
km_f[km_f >=1.5] <- '高'
km_f[km_f >=1 &km_f <1.5 ] <- '中'
km_f[km_f <1] <- '低'
return(km_f)
}
km_plot_sum()


# 选择分类为7的分类
km_plot_detail <- function(n){
require(plyr)
x <- data_num
x$km <- km7$cluster
km_fac<-ddply(.data = x,.variables = .(km),function(x) apply(x,2,mean))[,1:19]
if(n==1) {
par(mfrow=c(2,2),las=2)
barplot(as.matrix(sort(km_fac[1,],decreasing = F) ),horiz = T,cex.names = 0.8,density = 20,main='kmodes第1分类需求分数排行')
barplot(as.matrix(sort(km_fac[2,],decreasing = F) ),horiz = T,cex.names = 1,density = 20,main='kmodes第2分类需求分数排行')
barplot(as.matrix(sort(km_fac[3,],decreasing = F) ),horiz = T,cex.names = 0.8,density = 20,main='kmodes第3分类需求分数排行')
barplot(as.matrix(sort(km_fac[4,],decreasing = F) ),horiz = T,cex.names = 1,density = 20,main='kmodes第4分类需求分数排行')
par(mfrow=c(1,1),las=1)}
if(n==2){
par(mfrow=c(2,2),las=2)
barplot(as.matrix(sort(km_fac[5,],decreasing = F) ),horiz = T,cex.names = 0.8,density = 20,main='kmodes第5分类需求分数排行')
barplot(as.matrix(sort(km_fac[6,],decreasing = F) ),horiz = T,cex.names = 1,density = 20,main='kmodes第6分类需求分数排行')
barplot(as.matrix(sort(km_fac[7,],decreasing = F) ),horiz = T,cex.names = 0.8,density = 20,main='kmodes第7分类需求分数排行')
par(mfrow=c(1,1),las=1)}}

km_plot_detail(1)
km_plot_detail(2)















#############################################################################3

# 忽略先
# 层次聚类
# 分类变量离散化
dist_factor_temp <- function(data){
x <-data.frame()
y <-data.frame()
for (i in 1:ncol(data)){
x<-tapply(X=rep(1,nrow(data)),INDEX=list(row.names(data), data[,i]),FUN=sum)
x[is.na(x)] <- 0
if(nrow(y)==0) y<-x else y <-cbind(y,x)
}
return(y)
}

data_dist_factor_temp <- dist_factor_temp(data_factor_new)
# 分类变量距离计算
data_dist_factor <- dist(x = data_dist_factor_temp,method = 'binary')

hc <- hclust(d = data_dist_factor,method = 'average')



################



