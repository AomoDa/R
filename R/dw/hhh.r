
# y1	毛利率
# y2	利润率
# y3	增长率
# y4	客户满意度
# y5	销售租金比
# y6	人均销售（店员）
# x1	平均单价
# x2	海报费用
# x3	装修道具
# x4	微信文章发布数
# x5	微信文章平均阅读量
# x6	折扣
# x7	店铺类型
# x8	店铺等级
# x9	店铺格式
# x10	店铺面积
# x11	城市等级
# x12	开业时间（月份数）
# x13	店员人数

library(mice)
library(ggplot2)
library(plotly)
library(psych)
library(caret)
library(quantreg)
library(rqPen)

x <- read.csv('mydata.csv',header = T,stringsAsFactors = F)
str(x)

#----------------------------------------------
#Multivariate Imputation by Chained Equations
#----------------------------------------------

set.seed(6666)
x_imp <- mice(data = x,method = 'pmm')
plot(x_imp)
xx <- complete(x_imp,action = 4)

#----------------------------------------------
# PCA
#----------------------------------------------

# data scale
y <- xx[,1:6]
y$y4 <- y$y4 / 100
y$y5 <- 1 / y$y5
y$y6 <- scale(y$y6)
summary(y)
pairs.panels(y)

# 确定公因子个数
# 两种分析方法都建议2个公因子
VSS(y,rotate = 'varimax',digits=2)
fa.parallel(y,fa='pc')

# PCA 分析
pc1 <- principal(y,nfactors = 2,rotate = 'varimax')
pc1
biplot(pc1)

# 提取分析结果
pca_y <- pc1$scores

# 合并结果：
## 计算系数
coef_pca <- c(0.33,0.23)
coef_pca <- coef_pca / sum(coef_pca)
## 合并成评价指标Y
Y <- round(pca_y %*% as.matrix(coef_pca),4)




#----------------------------------------------
# EDA
#----------------------------------------------

myqa <- function(x,q) {
	qa <- quantile(x,q)
	cut_qa <- cut(x,breaks = c(min(x),qa),include.lowest = T,labels = q)
	return(cut_qa)
}


mydf <- cbind(data.frame(y=Y),xx[,-c(1:6)])
# 计算Y的分位
mydf$QUAN <- myqa(mydf$y,q = c(0.2,0.8,1))

# Y 与店铺级别的关系
ggplot(data = mydf,aes(x=x9,y=y,fill=x9))+
    geom_boxplot(outlier.colour = 'red',outlier.shape = 16,show.legend = F)+
    ylim(-4,4)+theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店铺格式',y='Y',title='Y VS x9')

ggplot(data = mydf,aes(x=x7,y=y,fill=x7))+
    geom_boxplot(outlier.colour = 'red',outlier.shape = 16,show.legend = F)+
    ylim(-4,4)+theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店铺类型',y='Y',title='Y VS x7')

ggplot(data = mydf,aes(x=x8,y=y,fill=x8))+
    geom_boxplot(outlier.colour = 'red',outlier.shape = 16,show.legend = F)+
    ylim(-4,4)+theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店铺等级',y='Y',title='Y VS x8')


# 线性关系
ggplot(data = mydf,aes(x=x4,y=y,col=as.factor(QUAN)))+
    geom_point(show.legend = F,position='jitter')+
    geom_smooth(col=I(gray(0.2)),method = 'lm',se = F,lty=2)+
    ylim(-4,4)+theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='微信文章发布数',y='Y',title='Y VS x4')

ggplot(data = mydf,aes(x=x12,y=y,col=as.factor(QUAN)))+
    geom_point(show.legend = F,position='jitter')+
    geom_smooth(col=I(gray(0.2)),method = 'lm',se = F,lty=2)+
    ylim(-4,4)+theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='开业时间（月份数）',y='Y',title='Y VS x12')


ggplot(data = mydf,aes(x=x13,y=y,col=as.factor(QUAN)))+
    geom_point(show.legend = F,position='jitter')+
    geom_smooth(col=I(gray(0.2)),method = 'lm',se = F,lty=2)+
    ylim(-4,4)+theme_bw()+facet_wrap(~QUAN, nrow = 1)+
    labs(x='店员人数',y='Y',title='Y VS x13')

#----------------------------------------------
# Quantile Regression
#----------------------------------------------

# http://topepo.github.io/caret/available-models.html
getModelInfo(model = 'rqnc')
getModelInfo(model = 'rqlasso')

train(y~.,data=mydf[,-15],method='rqnc')
set.seed(200)
lamb <- expand.grid(lambda=seq(0,1,by=0.05))
tc <- trainControl(method = "repeatedcv",number = 3,repeats = 3)

train(y~.,
	data=mydf[,-15],
	method='rqlasso',
	tau=c(seq(0.2,0.8,0.99)),
	tuneGrid=lamb,
	metric='Rsquared',
	trControl=tc)
