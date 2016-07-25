---
title: "问卷分析要点"
author: "作者"
date: "2016年7月25日"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# 介绍及说明
为了能够完整运行本文涉及的算法需要安装和加载以下几个算法包，如果没有安装包，需要使用install.packages("package")安装。安装成功之后，使用以下命令加载。

* 这里我把R语言代码和结果全部写在了这里，有些图和表需要绘制成统计表格，这个你来处理。
* 这里只是分析的结果汇总和要点分析，并不是严格按照论文的方式来阐述的，论文的主题思路和论证仍需要你来完成。

```{r, message=FALSE, warning=FALSE}
library(foreign)
library(klaR)
library(arules)
library(arulesViz)
library(car)
library(plyr)
```
#数据准备和预处理

进行数据的读取和处理工作，主要为了方便后续的数据分析及结果整理。
* 这是数据的处理过程，论文中一笔带过就行，我在这里展示出来，主要是方便你看的明白。
* 有些表格仍然需要你自己去绘制统计表格。

##分类数据

将问卷的答案数据分类。


### 原始分类数据

将原始数据读取完成之后，格式化数据，按照对每个问卷的选项按照需要程度的高低，从低到高分别设置为 “非常不需要”,“不需要”,“视情况而定”,“需要”,“非常需要”五个分类。

### 重新分类数据
考虑到实际情况，将原始数据重新分类合并，将“非常不需要”和“不需要”合并为“不需要”，“视情况而定保留”，“需要”和“非常需要”合并为“需要”。

##量化数据
考虑到实际情况，每个问卷题目表达的是答卷者对每个项目的需求程度，因此将原始分类数据量化，用“需求分数”来替换原始分类数据。分别设置为：“非常不需要”为-1分，“不需要”为0分，“视情况而定”为1分，“需要”为2分，“非常需要”为3分。量化后的数据用数值型变量表示，这种数据处理更加直观可靠。

```{r}
# 定义读取数据函数，并对数据重命名。
data_get <- function(){
require(foreign)
setwd('C:/Users/AomoDa/Documents')
x <- read.spss(file = 'spss.sav',to.data.frame = T,use.value.labels = F)
x <- data.frame(x[,-1])
x <- as.data.frame(apply(X = x,MARGIN = 2,function(x) as.numeric(x) ))
names(x) <- c('社区餐厅','送餐服务','上门做饭','社区药店','康复理疗','家庭医生','家庭病床','生活照料','帮助洗澡','打扫卫生','帮助购物','代办代缴','上门维修','外出接送_陪同散步','老年学校','安全呼救','结对关怀','文化娱乐','锻炼健身')
return(x)
}
x <-data_get() # 读取数据
# 分类数据，并且对数据打上标签

### 原始分类数据
data_factor <- as.data.frame(apply(X = x,MARGIN = 2,function(x) factor(x = x,levels = 1:5,labels = c('非常不需要','不需要','视情况而定','需要','非常需要'))))
str(data_factor,list.len=5)

#  量化数据分析
#将分类数据数值化。非常不需要=-1分，不需要=0分，视情况而定=1分，需要=2分，非常需要=3分
data_num <- x - 2

### 重新分类数据
data_factor_new <- as.data.frame(apply(X = data_num,MARGIN = 2,function(x)car::recode(x," c(-1,0)='不需要';1='视情况而定';c(2,3)='需要' ") ))
str(data_factor_new,list.len=5)

```

# 描述性统计分析和探索性数据分析

这里对数据进行简单的统计分析。我这里是量化之后的需求程度需求分数的分析，你原来的是原始分类数据的分析，两部分结合起来分析效果更佳。你把我的部分加进你原来的论文即可。

*  频率统计你做了，就用你的就行。我这里做了一点你没有考虑到的。你如果觉得可以，把我分析的部分加进你的论文就行。
* 统计表格需要你自己绘制。
* 条形图和箱图 其实用Excel也可以绘制，你写论文的时候可以用Excel绘制，我的图和表格全部是用R语言绘制的，你觉得可以就用，不行就用Excel。

## 总体分析


```{r}
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
```

##不同就餐协助方式需求程度分布

```{r}
par(mfrow=c(2,1))
boxplot(data_num[,1:3],ylab='需求分数',main='不同就餐协助方式需求分数箱形图')
par(las=2)
barplot(bar_num(data_num[,1:3])[,1],horiz = T,main='不同就餐协助方式需求分数条形图',cex.names=0.7, names.arg=bar_num(data_num[,1:3])[,2],density = 20)
par(mfrow=c(1,1),las=1)
```

##不同医疗服务的需求程度分布
```{r}
par(mfrow=c(2,1))
boxplot(data_num[,4:7],ylab='需求分数',main='不同医疗服务需求分数箱形图')
par(las=2)
barplot(bar_num(data_num[,4:7])[,1],horiz = T,main='不同医疗服务需求分数条形图',cex.names=0.7,names.arg=bar_num(data_num[,4:7])[,2],density = 20)
par(mfrow=c(1,1),las=1)
```

##不同生活服务的需求程度分布
```{r}
par(mfrow=c(2,1))
boxplot(data_num[,8:14],ylab='需求分数',main='不同生活服务需求分数箱形图')
par(las=2)
barplot(bar_num(data_num[,8:14])[,1],horiz = T,main='不同生活服务需求分数条形图',cex.names=0.6,names.arg=bar_num(data_num[,8:14])[,2],density = 20)
par(mfrow=c(1,1),las=1)
```

##精神文化与健康服务

```{r}
par(mfrow=c(2,1))
boxplot(data_num[,15:19],ylab='需求分数',main='不同就餐精神文化与健康服务分数箱形图')
par(las=2)
barplot(bar_num(data_num[,15:19])[,1],horiz = T,main='不同精神文化与健康服务需求分数条形图',cex.names=0.7,names.arg=bar_num(data_num[,15:19])[,2],density = 20)
par(mfrow=c(1,1),las=1)
```

#apriori关联分析



```{r}
rules <- apriori(data = data_factor_new,parameter = list(supp=0.3,conf=0.8,target='rules',minlen=2),appearance = list(rhs=c('安全呼救=需要','上门维修=需要','社区药店=需要' ),default='lhs' ))
quality(rules) <- round(quality(rules),2)
r<-sort(rules,by='lift')
inspect(r)
```


```{r}
plot(r,method ='grouped',control=list(col=gray(1:10/10)))
plot(r,method ='matrix',control=list(col=gray(1:10/10)))
```

# k-modes 聚类分析


##建模及模型选择
```{r}
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
```


##模型总体分析
```{r}
km_plot_sum <- function() {
four<-function(x){
y<-data.frame(就餐方面=mean(as.numeric(x[1:3])), 医疗方面=mean(as.numeric(x[4:7])),生活方面=mean(as.numeric(x[8:14])),社区服务=mean(as.numeric(x[15:19])))
return(y)
}
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
```


##模型详细分析

```{r}
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
```

