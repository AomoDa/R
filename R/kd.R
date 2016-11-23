
#Q1
library(reshape2)
mydata <- data.frame(id=c(1,1,2,2),time=c(1,2,1,2),x1=c(5,3,6,2),x2=c(6,5,1,4))
md <- melt(mydata,id=c('id','time'))
md
dcast(md,id~variable+time)


library(tidyr)
unite(mydata,x1_time,x1,time) 
unite(mydata,x2_time,x2,time)


#Q2

x <- read.csv('companies.csv')

#2.1
x$margin <- round(x$profit/x$revenue,2)

#2.2
aggregate(revenue~company,data=x,sum)

#2.3
aggregate(profit~company,data=x,sum)

#2.4
x <- x[order(x$fy),]
aggregate(margin~company,data=x,diff)

#2.5
ddply(x,.variables = .(company),.fun = summarise,
       revenue=round(sd(revenue),0),
       profit=round(sd(profit),0),
       margin=round(sd(margin),0))

#2.6
with(x[x$company=='Apple',],plot(fy,margin,type='b',lwd=2,lty=4,col='red',pch=15,ylim=c(0,0.5)))
with(x[x$company=='Google',],points(fy,margin,type='b',lwd=2,lty=2,col='green',pch=16))
with(x[x$company=='Microsoft',],points(fy,margin,type='b',lwd=2,lty=1,col='blue',pch=17))
legend('topleft',col=c('red','green','blue'),lwd=2,pch=15:17,
    legend=c('Apple','Google','Microsoft'),lty=c(4,2,1))


#Q3

library(tidyr)
library(dplyr)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)


#----------------------------------------------------------------------------------------

#Q1
library(ggplot2)
library(dplyr)
library(tidyr)
barplot(as.matrix(aggregate(mpg~cyl+am,data=mtcars,sd) %>% spread(am, mpg))[,-1],beside = T)


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
library(stringr)
library(tidyr)
library(dplyr)
x <- read.csv('data_sample.csv')
xx <- gather(x,Country.Name,na.rm = T)
names(xx) <- c('Country.Name','time','value')
xx$year <- as.numeric(str_extract(xx$time,pattern = '[0-9]{4}'))
xx$quarter <-str_extract(xx$time,pattern = 'Q[1-4]')
xx$mon_num <- as.numeric(str_sub(str_extract(xx$time,pattern = 'M[0-9]{1,2}'),start = 2))

##  data output
year.data <- xx[ is.na(xx$quarter) & is.na(xx$mon_num) ,c(1,4,3) ]
head(year.data)
quarter.data <- xx[!is.na(xx$quarter) ,c(1,4,5,3) ]
head(quarter.data)
month.data <- xx[!is.na(xx$mon_num) ,c(1,4,6,3) ]
month.data$month <-factor(month.data$mon_num,levels = 1:12,labels = month.name)
month.data <-month.data[,-3]
head(month.data )

## write.csv
write.csv(year.data,'year.csv')
write.csv(quarter.data,'quarter.csv')
write.csv(month.data,'month.csv')


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


#Q4

##LM
library(ggplot2)
lm(mpg~hp,data=mtcars)

#nls
nls(mpg~a+b*hp,data=mtcars,start=list(a=1,b=0))


#mle
library(bbmle)
LL <- function(a, b, mu, sigma){
   R = mtcars$mpg - mtcars$hp *b - a 
   -sum(dnorm(R, mu, sigma, log = TRUE))

}
mle2(LL, start = list(a = 1, b = 1, mu = 1, sigma = 1), fixed = list(mu = 0))

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

set.seed(2000)
x <- read.csv('parameter.csv')
ind <- sample(x=1:3,size = nrow(x),replace = T)
x1 <- x[ind==1,]
x2 <- x[ind==2,]
x3 <- x[ind==3,]
write.csv(x1,'block1.csv')
write.csv(x2,'block2.csv')
write.csv(x3,'block3.csv')


#Q3
set.seed(666)

a <- sample(1:6,size=1000,replace = T,prob = rep(1/6,6))
b <- sample(1:6,size=1000,replace = T,prob = rep(1/6,6))
mean(a==b)

#Q4

barplot(prop.table(table(a+b)),ylab='Freq')
