
###hw8

##Do problems 48, 50, 57, 60, 64 (2 points each) and 45, 53, 62 (5 points each)

#Q48

library(ggplot2)
data(chickwts)

qplot(x=feed,y=weight,data=chickwts,geom='boxplot',fill=feed)
aggregate(weight~feed,data=chickwts,mean)

#Q50

my_test <- function(x,mu=0,n=20) {
y <- replicate(10000 ,mean(sample(x,size = n,replace = T)))
p_value <- sum(y>=mu)/10000
return(paste('the p value of my test is :',p_value,sep=' '))
}

set.seed(50)
my_test(rnorm(100),mu = 0)


#Q57

####questions about independence of attributes

###Gender and education
###Race and education
###Happiness and political party
###Views of gun laws and race

####questions about homogeneity of distributions across several populations
##Gender and views of death penalty


#Q60

##a
Bangladesh <- read.csv('Bangladesh.csv',header = T)
Chlorine <- na.omit(Bangladesh$Chlorine)
par(mfrow=c(1,2))
hist(Chlorine,freq = F,ylim=c(0,0.02),breaks=50)
lines(density(Chlorine,from=0),col='red')
qqnorm(Chlorine)
qqline(Chlorine,col='red')
par(mfrow=c(1,1))

##b
z <- replicate(10000, mean(sample(Chlorine, length(Chlorine), replace = T)))
quantile(z,c(0.05,0.95))



#Q64

Titanic <- read.csv('Titanic.csv',header = T)
boxplot(Age ~ Survived, data = Titanic)
aggregate(Age~Survived,data=Titanic,median)
table(Titanic$Survived)

pnum <- 0 
for (i in 1:9999) {
 ind <- sample(1:nrow(Titanic),size=135,replace=F)
 a <- with(Titanic,median(Age[-ind]-median(Age[ind])))
 pnum <- ifelse(a<=2,1,0)+pnum
}
p_value <- (pnum+1)/(9999+1)
p_value


#Q45

###minimum
#p(x=3)=0.5
#p(x=6)=0.3
#p(x=7)=0.15
#p(x=9)=0.05
#p(x=11)=0
#p(x=14)=0
a <- c(3,6,7,9,11,14)
x <- replicate(20,min(sample(a,3,replace=F)))
plot(table(x))


###mean 
y <- replicate(20,mean(sample(a,3,replace=F)))
mean(y);mean(a)
sd(y); sd(a)/sqrt(3)*sqrt((6-3)/(6-1))

#Q53


#Q62
