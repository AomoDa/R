---
title: "Untitled"
author: "Your Name"
date: "2016年11月19日"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##地图
```{r, message=FALSE, warning=FALSE}
library(maptools)
library(ggplot2)
library(psych)
library(car)
# plot map
mydata <- read.csv('C://Users//AomoDa//Documents//mydata.csv',header = T,stringsAsFactors = F)
china_map<-readShapePoly("C://Users//AomoDa//Documents//bou2_4p.shp")

plot_gdp_map  <- function(){
 require(maptools)
 require(ggplot2)
 china_map_data <-china_map@data
 china_map_data$id <- 0:923
 china_map_all <- fortify(china_map)
 china_map_all$id <- as.numeric(china_map_all$id)
 xx <- merge(china_map_data,mydata,by.x='NAME',by.y='Province',all.x=T)
 mydata_ok <- merge(china_map_all,xx,all.x=T)
 ggplot(mydata_ok, aes(x = long, y = lat, group = group,fill=GDP))+
    geom_polygon( aes(fill=GDP))+
    geom_path(colour = "grey40")+
    labs(x='',y='',title='GDP分布')+
    scale_fill_gradient(low ='white' ,high ='red')

}

plot_gdp_map()
```


## boxplot and histogram
```{r}
ggplot(data=mydata,aes(x=region,y=GDP,fill=region))+geom_boxplot()
ggplot(data=mydata,aes(x=GDP,fill=region))+
   geom_histogram(alpha=1,bins=3,col=I('white'))+facet_wrap(~region,ncol = 3)

ggplot(data=mydata,aes(x=district,y=GDP,fill=district))+geom_boxplot()

ggplot(data=mydata,aes(x=GDP,fill=district))+
   geom_histogram(alpha=1,bins=3,col=I('white'))+facet_wrap(~district,ncol = 3)
```


##plot pairs

###Populstion vs GDP
```{r}
##Populstion vs GDP
ggplot(data=mydata,aes(x=Populstion,y=GDP))+geom_point()+
   geom_smooth(method = 'lm')+labs(title='Populstion vs GDP')
```

###RSCG vs GDP
```{r}
ggplot(data=mydata,aes(x=RSCG,y=GDP))+geom_point()+
   geom_smooth(method = 'lm',col=I('red'))+labs(title='RSCG vs GDP')
```

###LFRvs GDP

```{r}
ggplot(data=mydata,aes(x=LFR,y=GDP))+geom_point()+
   geom_smooth(method = 'lm',col=I('orange'))+labs(title='LFR vs GDP')
```

##Fitting Linear Models

```{r}
### 建立全变量的线性回归方程。
lm1 <- lm(GDP~.-Province-1,data=mydata)

### 利用向后逐步回归方法确定最优模型lm2
lm2 <- step(lm1,direction = 'backward')
###观察变量显著性，发现CPI的变量不显著。
summary(lm2)

### 更新模型，去掉CPI变量，建立模型lm3
lm3 <- update(lm2,.~.-CPI)
###观察变量显著性，发现所有变量全部显著，方差整体F检验通过，调整R方99.82%
summary(lm3)
```

###GDP真实值，线性回归预测值对比

```{r}
plot(mydata$GDP,type='p',pch=16,col='blue',main='GDP真实值，线性回归预测值对比')
points(lm3$fitted.values,col='red',type='b',pch=17,lty=3)
legend('topright',col=c('blue','red'),pch=c(16,17),lty=c(1,3),legend=c('真实值','预测值'))
```

### 线性回归残差图
```{r}

# 通过Q-Q图观察出线性回归的残差的基本正态分布。
par(mfrow=c(1,2))
plot(residuals(lm3),main='线性回归残差图')
abline(h=0,lty=2,lwd=2,col='red')
qqPlot(lm3,main='线性回归残差Q-Q图')
par(mfrow=c(1,1))

# 使用W检验线性回归的残差，P值不显著，得出服从正态分布。
shapiro.test(residuals(lm3))
```

## 结论

GDP 可以使用以下公式估计：

$$GDP = 0.43562*FAI + 1.13566 * RSCG + 1.54184 * gross.export + 1.64349 * LFR$$
