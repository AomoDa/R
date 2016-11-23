
#Q1
library(reshape2)
library(tidyr)

mydata <- data.frame(id=c(1,1,2,2),time=c(1,2,1,2),x1=c(5,3,6,2),x2=c(6,5,1,4))

md <- melt(mydata,id=c('id','time'))
md
unite(md,time,variable,value)


#Q2

x <- read.csv('companies.csv')

x$margin <- round(x$profit/x$revenue,2)
aggregate(revenue~company,data=x,sum)
aggregate(profit~company,data=x,sum)

x <- x[order(x$fy),]
aggregate(margin~company,data=x,diff)

ddply(x,.variables = .(company),.fun = summarise,
       revenue=round(sd(revenue),0),
       profit=round(sd(profit),0),
       margin=round(sd(margin),0))

with(x[x$company=='Apple',],plot(fy,margin,type='b',lwd=2,lty=2,col='red',ylim=c(0,0.5)))
with(x[x$company=='Google',],points(fy,margin,type='b',lwd=1,lty=4,col='green',pch=16))
with(x[x$company=='Microsoft',],points(fy,margin,type='b',lwd=1,lty=1,col='blue',pch=6))
legend('topleft',col=c('red','green','blue'),lwd=c(2,1,1),pch=c(1,16,6),
    legend=c('Apple','Google','Microsoft'),lty=c(2,4,1))


#Q3

library(reshape2)
#Air quality example
names(airquality) <- tolower(names(airquality))
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)
acast(aqm, day ~ month ~ variable)
acast(aqm, month ~ variable, mean)
acast(aqm, month ~ variable, mean, margins = TRUE)
dcast(aqm, month ~ variable, mean, margins = c("month", "variable"))


#----------------------------------------------------------------------------------------

#Q1



#Q2
Aye <- sample(c("Yes", "Si", "Oui"), 177, replace = TRUE)
Bee <- sample(c("Hum", "Buzz"), 177, replace = TRUE)
(A <- table(Aye, Bee))
addmargins(A,margin = c(1,2))
prop.table(A,margin =2)
prop.table(A,margin =1)

#Q3

library(ggplot2)
str(economics)
cor(economics$pce,economics$psavert)

#----------------------------------------------------------------------------------------

#Q1

x <- read.csv('data_sample.csv')



#Q2

#chisq
set.seed(100)
x1 <- rchisq(10,df = 3)
x2 <- rchisq(20,df = 3)
x3 <- rchisq(40,df = 3)
x4 <- rchisq(100,df = 3)

par(mfrow=c(2,2))
plot(density(x1),main='chisq n=10,df=3')
plot(density(x2),main='chisq n=20,df=3')
plot(density(x3),main='chisq n=40,df=3')
plot(density(x4),main='chisq n=100,df=3')
par(mfrow=c(1,1))


##CTI
set.seed(100)
x1 <- replicate(100,mean(rchisq(10,df = 3)))
x2 <- replicate(100,mean(rchisq(10,df = 3)))
x3 <- replicate(100,mean(rchisq(10,df = 3)))
x4 <- replicate(100,mean(rchisq(10,df = 3)))

par(mfrow=c(2,2))
plot(density(x1),main='CTL with chisq n=10,df=3')
plot(density(x2),main='CTL with chisq n=20,df=3')
plot(density(x3),main='CTL with chisq n=40,df=3')
plot(density(x4),main='CTL with chisq n=100,df=3')
par(mfrow=c(1,1))


#Q3
##1sd
pnorm(q=1,mean=0,sd=1)-pnorm(q=0,mean=0,sd=1)
pnorm(q=0,mean=0,sd=1)-pnorm(q=-1,mean=0,sd=1)

##2sd
pnorm(q=2,mean=0,sd=1)-pnorm(q=1,mean=0,sd=1)
pnorm(q=-1,mean=0,sd=1)-pnorm(q=-2,mean=0,sd=1)

##3sd

pnorm(q=3,mean=0,sd=1)-pnorm(q=2,mean=0,sd=1)
pnorm(q=-2,mean=0,sd=1)-pnorm(q=-3,mean=0,sd=1)

## other
1-pnorm(q=3,mean=0,sd=1)
pnorm(q=-3,mean=0,sd=1)


#----------------------------------------------------------------------------------------

#Q1


set.seed(200)
name <- paste('x',1:10,sep='')
file.name <- paste('data',1:10,'.csv',sep='')
data_all <- matrix(NA,ncol=10,nrow=10)
for (i in 1:10) {
 data_all[,i] <- assign(name[i],value = rnorm(10,2,5))
 write.csv(data_all[,i],file.name[i])
}

head(data_all)

#Q2

x <- read.csv('parameter.csv')
ind <- sample(x=1:3,size = nrow(x),replace = T)
x1 <- x[ind==1]
x1 <- x[ind==2]
x1 <- x[ind==3]


#Q3
set.seed(666)

a <- sample(1:6,size=1000,replace = T,prob = rep(1/6,6))
b <- sample(1:6,size=1000,replace = T,prob = rep(1/6,6))
mean(a==b)

#Q4

barplot(prop.table(table(a+b)),ylab='Freq')
