

#--------------------------------------
# 题目1.缺失值分析处理-10分
# 程诗尧
#-----------------------------------
Sys.time()
library(VIM)
library(mice)
x <- read.csv('example_1.csv')
x$status <- as.factor(x$status)
# 缺失数据统计
md.pattern(x)
# 缺失数据可视化
aggr(x,number=T,prop=F)
#多重插补法填充数据
Sys.time()
imp <- mice(x,set.seed=100)
full_example <- complete(imp)
summary(full_example)


#--------------------------------------
# 题目2.描述性统计-15分
# 程诗尧
#-----------------------------------
Sys.time()
#统计填补后的数据集full_example的样本数
nrow(full_example)
#统计填补后的数据集full_example的3个分类变量：
# status,type,province income  
# 各个分类的记录数和各个分类的占比
#status
table(full_example$status)
prop.table(table(full_example$status))
# type
table(full_example$type)
prop.table(table(full_example$type))
# province
table(full_example$province)
prop.table(table(full_example$province))
#income
table(full_example$income)
prop.table(table(full_example$income))
#统计填补后的数据集full_example的3个连续变量：
# length	wide	high  的均数,最小值，最大值，中位数
summary(full_example[,c('length','wide','high')])
# 标准差
apply(full_example[,c('length','wide','high')],MARGIN = 2,FUN = sd)



#--------------------------------------
# 题目3.交叉分析-15分
# 程诗尧
#-----------------------------------

library(gmodels)
#status与type
#程诗尧
Sys.time()
with(full_example,CrossTable(status,type,expected = F,prop.chisq = F,format = 'SPSS',digits = 2))
#status与province
#程诗尧
Sys.time()
with(full_example,CrossTable(status,province,expected = F,prop.chisq = F,format = 'SPSS',digits = 2))
#status与income
# 程诗尧
Sys.time()
with(full_example,CrossTable(status,income,expected = F,prop.chisq = F,format = 'SPSS',digits = 2))


#统计填补后的数据集full_example的3个连续变量与status之间的关系
#程诗尧
Sys.time()
library(plyr)
q3_summary <- function(x){
	return(c(xmean=mean(x),
		xsd=sd(x),
		xmin=min(x),
		xmax=max(x),
		xmedian=median(x)
		) 
	)
}
round(aggregate(cbind(length,wide,high)~status,data=full_example,FUN = q3_summary),2)


#--------------------------------------
# 题目4.t检验、卡方检验、方差分析-15分
# 程诗尧
#-----------------------------------

#做t检验
#程诗尧
Sys.time()
t.test(length~status,data=full_example)
t.test(wide~status,data=full_example)
t.test(high~status,data=full_example)

#卡方检验
#程诗尧
Sys.time()
with(full_example,chisq.test(status,type,correct = F))
with(full_example,chisq.test(status,province,correct = F))
with(full_example,chisq.test(status,income,correct = F))

#方差分析F检验
#程诗尧
Sys.time()
summary(aov(length~province,data=full_example))
summary(aov(wide~province,data=full_example))
summary(aov(high~province,data=full_example))


#--------------------------------------
# 题目5.挖掘算法：随机森林、决策树-30分
# 程诗尧
#-----------------------------------

#随机森林算法
#程诗尧
Sys.time()
#填补后的数据集full_example按照7:3划分为建模数据集
set.seed(500)
ind <- sample(x = 2,size = nrow(full_example),replace = T,prob = c(0.7,0.3))
table(ind)
#划分训练样本和测试样本
full_example_train <- full_example[ind==1,]
full_example_test <- full_example[ind==2,]

#建立模型
#程诗尧
Sys.time()
library(randomForest)
rf <- randomForest(status~length+wide+high+type+province+income,data=full_example_train)
plot(rf)
varImpPlot(rf)

#预测模型
#程诗尧
Sys.time()
#预测
pred <- predict(rf,newdata = full_example_test,type = 'class')
#准确度交叉表格
table(full_example_test$status,pred)


#--------------------------------------
# 题目6.图形绘制：散点图，折线图，箱图，直方图,饼图-15分
# 程诗尧
#-----------------------------------
#程诗尧
Sys.time()

#a.填补后的数据集full_example以length	wide为x轴和y轴做散点图-3分
with(full_example,plot(length,wide,main='length VS wide散点图(程诗尧)'))

#b.填补后的数据集full_example以id length 为x轴和y轴做折线图-3分
with(full_example,plot(id,length,type='l',main='length 折线图(程诗尧)'))

#c.填补后的数据集full_example以type  length为x轴和y轴做箱图-3分
boxplot(length~type,data=full_example,main='type VS length 箱图 (程诗尧)')

#d.填补后的数据集full_example以length为绘图变量做直方图-3分
hist(full_example$length,main='length 直方图 (程诗尧)')

#e.填补后的数据集full_example以province为分类变量做饼图-3分
pie(table(full_example$province),main='province饼图 (程诗尧)')
