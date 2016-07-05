
library(xlsx)
library(psych)
library(MASS)
library(car)
library(ggplot2)

x <- read.xlsx2(file = '数据.xlsx',sheetIndex = 1,header = T,
    colClasses = c('character',rep('numeric', 14)),stringsAsFactors=F)
x <- as.data.frame(x[,-1],row.names = x$own)
x$river <- as.factor(x$river)

head(x)
str(x)
summary(x)

hist(x$value,freq = F,main = '房价的频率分布图和密度图',
     xlab='房价',ylab='频率/密度')
lines(density(x$value),col='red')
shapiro.test(x$value)

## 社区安全因素
cor(x$crime,x$value,method = 'spearman')
plot(x$crime,x$value)
qplot(x=crime,y=log(value),data=x,geom='point',alpha=I(0.8)) + 
      geom_smooth(se = FALSE)+
      geom_smooth(method = 'lm', se = FALSE,col='red')+
      labs(x='犯罪率',y='房价',title='房价 VS 犯罪率')

## 环境

# 工业
qplot(x=industry,y=value,data=x,geom='point',alpha=I(0.8)) + 
      geom_smooth(se = FALSE)+
      geom_smooth(method = 'lm', se = FALSE,col='red')+
      labs(x='工业数量',y='房价',title='房价 VS 工业数量')

# 污染物
qplot(x=nox,y=value,data=x,geom='point',alpha=I(0.8)) + 
      geom_smooth(se = FALSE)+
      geom_smooth(method = 'lm', se = FALSE,col='red')+
      labs(x='一氧化氮物含量',y='房价',title='房价 VS 一氧化氮物含量 ')

###河流
t.test(value~river,data=x)
qplot(x=river,y=value,data=x,geom='boxplot',fill=river,show.legend=F)+
      labs(x='是否有河流穿过',y='房价',title='房价 VS 河流 ')

## 教育
qplot(x=ptratio,y=value,data=x,geom='point',alpha=I(0.8)) + 
      geom_smooth(se = FALSE)+
      geom_smooth(method = 'lm', se = FALSE,col='red')+
      labs(x='学生/教师比例',y='房价',title='房价 VS 学生/教师比例 ')

corr.test(x[,-4],method = 'spearman')
pairs.panels(x[,-4],method = 'spearman',show.points = F)

#回归方程

set.seed(100)
int <- sample(x=1:2,size = nrow(x),replace = T,prob = c(0.95,0.05))

x.train <- x[int==1,]
x.test <- x[int==2,]


lm1 <- lm(value ~ crime + biglots + industry + river + nox + rooms + age + 
    distance + highway + tax + ptratio + black + lowstat,data=x.train)

stepAIC(lm1,direction = 'backward')

lm2 <-lm(formula = value ~ crime + biglots + river + nox + rooms + 
    distance + highway + tax + ptratio + black + lowstat, data = x.train)

anova(lm1,lm2)

par(mfrow=c(2,2))
plot(lm2)
par(mfrow=c(1,1))

lm.p <- predict.lm(object = lm2,newdata = x.test,interval = 'prediction',level = 0.95)

res <- as.data.frame(cbind(x.test$value,lm.p))
res$res <- res$V1-res$fit

ggplot(data=res,aes(x=1:35))+geom_path(aes(y=fit),group=1,col='blue')+geom_path(aes(y=V1),group=1,col='red')+
   geom_path(aes(y=lwr),group=1,lty=2)+geom_path(aes(y=upr),group=1,lty=2)+
   scale_x_continuous(breaks = 1:35,labels = row.names(res))+
   theme(axis.text.x=element_text(angle=90))+
   labs(x='',y='房价',title='房价预测 VS 实际')


hist(res$res,freq=F,ylim=c(0,0.15))
lines(density(res$res),col='red')

qqnorm(res$res)
qqline(res$res)

ggplot(data=res,aes(x=1:35,y=res,fill=res))+geom_bar(stat='identity')+
   scale_x_continuous(breaks = 1:35,labels = row.names(res))+
   theme(axis.text.x=element_text(angle=90))+
   labs(x='',y='房价残差',title='房价预测残差示意图')




