
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
# x13	培训
# x14	店员人数
# x15	店铺代码
# x16	店铺名称
# x17	月份

library(mice)
library(ggplot2)
library(plotly)
library(psych)

x <- read.csv('mydata.csv',header = T,stringsAsFactors = F)
str(x)

#----------------------------------------------
#Multivariate Imputation by Chained Equations
#----------------------------------------------

x_imp <- mice(data = x,method = 'pmm')
plot(x_imp)
xx <- complete(x_imp,action = 3)



#----------------------------------------------
# PCA
#----------------------------------------------

# data scale
y <- xx[,1:6]
y$y4 <- y$y4 / 100
y$y5 <- 1 / y$y5
y$y6 <- scale(y$y6)
summary(y)

#
pairs.panels(y)
