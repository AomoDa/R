---
title: "Untitled"
author: "Your Nmae"
date: "2017年1月12日"
output: 
  word_document: 
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##import data

```{r, message=FALSE, warning=FALSE}
library(xlsx)
library(psych)
library(corrplot)
library(ggplot2)
library(effects)
mydata <- read.xlsx2('C://Users//mali//Documents//data.xlsx',
                      sheetIndex = 4,
                      stringsAsFactors=F, 
                      colClasses=c(rep('numeric',33),'character')
                     )

str(mydata)

```

##EDA

```{r}
mydata$q29 <- factor(mydata$q29,levels = 1:3,labels = c('18-25','26-30','31-35'))
ggplot(data = mydata,aes(x=q29,fill=q29))+geom_bar()
mydata$q30 <- factor(mydata$q30,levels = 1:6,labels = c('理科','工科','人文艺术','经管',NA,'其他'))
ggplot(data = mydata,aes(x=q30,fill=q30))+geom_bar()
mydata$q31 <- factor(mydata$q31,levels = c(1,3,4,7),labels = c('中专及以下','大专','本科','硕士及以上'))
ggplot(data = mydata,aes(x=q31,fill=q31))+geom_bar()

ggplot(data = mydata,aes(x=q32,fill=q32))+geom_bar(show.legend = F)

```


## corrplot

```{r}
cor.m <- cor(mydata[,3:30],use='complete.obs')
cor.m #相关系数矩阵
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor.m, order = "AOE", col = col3(10))
```

##PCA

###Scree plots

```{r}
# Scree plots
# the number of components =  2 
fa.parallel(x = cor.m,n.obs = nrow(mydata),fa = 'pc')
```


主成分分析，提取累计方差解释百分比， SS loading

```{r}
pc1 <- principal(r = cor.m,
	nfactors = 2,
	n.obs = nrow(mydata),
	rotate = 'none',
	scores = T
	)
pc1
```

###varimax rotate

```{r}
# 极大方差旋转方法
pc2 <- principal(r = cor.m,
	nfactors = 2,
	n.obs = nrow(mydata),
	rotate = 'varimax',
	scores = T
	)
pc2
fa.plot(pc2)
fa.diagram(pc2)
```

###loadings

```{r}
# 旋转成分载荷
pc2$loadings
```

###weights

```{r}
# 主成分得分系数
pc2$weights
```

##计算因子得分

```{r}

for (i in 1:nrow(mydata)) {
	mydata$rc1[i] <- sum(mydata[i,3:30] * as.numeric(pc2$weights[,1]))
	mydata$rc2[i] <- sum(mydata[i,3:30] * as.numeric(pc2$weights[,2]))
	mydata$satisfy[i] <- as.numeric(mean( as.numeric(mydata[i,c('q23','q24','q25')]),na.rm=T))
	mydata$loyal[i] <- as.numeric(mean(as.numeric(mydata[i,c('q26','q27','q28')]),na.rm=T))
	mydata$final[i] <- as.numeric(mean( as.numeric(mydata[i,c('q26','q27','q28','q23','q24','q25')]),na.rm=T))
}
boxplot(mydata[,35:36],horizontal = T,main='boxplot of rc')
```

##回归

$$satisfy=0.6019 \cdot rc1 +0.3483 \cdot  rc2$$

### 满意度回归
```{r}
# 满意度回归
lm1 <- lm(satisfy~rc1+rc2,data = mydata)
summary(lm1)
lm2 <- lm(satisfy~rc1+rc2-1,data = mydata)
summary(lm2)
anova(lm2,lm1)
plot(allEffects(lm2))
par(mfrow=c(2,2))
plot(lm1)
```

### 忠诚度回归

$$loyal=0.5388 \cdot rc1 +0.4266 \cdot  rc2$$

```{r}
lm3 <- lm(loyal~rc1+rc2-1,data = mydata)
summary(lm3)
anova(lm3)
plot(allEffects(lm3))
par(mfrow=c(2,2))
plot(lm3)

```

### 结果变量回归

$$final=0.5704 \cdot rc1 +0.3875 \cdot  rc2$$

```{r}
lm4 <- lm(final~rc1+rc2-1,data = mydata)
summary(lm4)
anova(lm4)
plot(allEffects(lm4))
par(mfrow=c(2,2))
plot(lm4)
```

