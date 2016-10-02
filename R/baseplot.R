

#part 1
q1 <- read.csv(file = 'q1.csv',header = T,stringsAsFactors = F)
q1$date <- as.Date(q1$date)
attach(q1)
par(mfrow=c(2,1))
plot(x = date,y = samsung,type='l',col='red',main='samsung',xlab='',ylab='')
plot(x = date,y = lg,type='l',col='blue',main='lg',xlab='',ylab='')
par(mfrow=c(1,1))
detach(q1)



#part 2

q2 <- read.csv(file = 'q2.csv',header = T,stringsAsFactors = F)
attach(q2)
par(mfrow=c(2,1))
boxplot(Nile,horizontal = T,main='Nile boxplot')
boxplot(sunspot,horizontal = T,main='sunspot boxplot')
par(mfrow=c(1,1))
detach(q2)




#part 3
q3 <- as.data.frame(Titanic)

##3.1
(tb1 <- xtabs(Freq~Age+Survived,data=q3[q3$Class=='3rd' & q3$Sex=='Male',]))
mosaicplot(tb1,shade = T,margin=c(1,2))

##3.2
(tb2 <- xtabs(Freq~Age+Class,data=q3[q3$Survived=='No' & q3$Sex=='Female',]))
mosaicplot(tb2,shade = T,margin=c(1,2))



#part5

library(lattice)
parallelplot(~iris[1:4],iris, groups = Species, horizontal.axis = FALSE)
parallelplot(~iris[1:4] | Species, iris,horizontal.axis=F,aspect = 2)


