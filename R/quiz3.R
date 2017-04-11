#Q1

x <- read.csv('weatherdata.csv',header = T)
q1 <- na.omit(subset(x,subset = Month %in% c(5,6,7),select=c('Ozone','Month')))
bartlett.test(Ozone~Month,data=q1)

#Q2
summary(aov(Ozone~Month,data=q1))

#Q3

library(reshape)
y <- read.csv('Traps.csv')
bartlett.test(value~variable,data=melt(y))


#Q4
summary(aov(value~variable,data=melt(y)))


#Q5
library(psych)

a <- melt(y)
pairwise.t.test(x = a$value,g = a$variable,p.adjust.method = 'bonferroni')
