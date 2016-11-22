
x <- read.csv('Hanta.csv',header = T)


#Q1
summary(x)

#coefficient of variation
cv <- function(x){sd(x)/mean(x) }
apply(x,2,cv)


#Q2
##http://www.cnblogs.com/zhangchaoyang/articles/2631907.html

cor(x,method = ='spearman')
library(psych)
pairs.panels(x,method = 'spearman')

#Q3

glm1 <- glm(DiseaseCases~Twinter, data=x,family = poisson(link=log))
glm2 <- glm(DiseaseCases~Twinter+Frost, data=x,family = poisson(link=log))
glm3 <- glm(DiseaseCases~Twinter+Frost+Rain, data=x,family = poisson(link=log))
summary(glm1)
summary(glm2)
summary(glm3)



#Q4

par(mfrow=c(1,2))
hist(glm1$residuals)
qqnorm(glm1$residuals)
qqline(glm1$residuals)
par(mfrow=c(1,1))

##Shapiro-Wilk normality test
shapiro.test(glm1$residuals)


#Q5

##a
k <-3
n <- nrow(x)
(2*k+2)/n
lev3 <- hat(model.matrix(glm3))
lev3>0.5714286

#-------------------------------
library(car)
influencePlot(glm3)

influence.measures(glm3)
#-------------------------------



##b

glm3_update <- glm(DiseaseCases~Twinter+Frost+Rain, data=x[c(-13,-14),],family = poisson(link=log))
summary(glm3_update)


