


#Q2

library(plyr)
library(VGAM)
library(plotrix)


##alpha=0.5
set.seed(2222)
pare <- data.frame(id=1:1e7,pareto_value=rpareto(1e7, scale = 0.5, shape=1) )
pare$seq <- (pare$id-1) %/% 5e4 +1 
mydata <- ddply(.data = pare,.variables = .(seq),.fun = summarise,id=max(id),avg=mean(pareto_value),max=max(pareto_value))
mydata$avg_ok <- cumsum(mydata$avg)/cummax(mydata$seq)
mydata$max_ok <- cummax(mydata$max)
# mydata[,c('id','avg_ok','max_ok')]
plot(mydata$max_ok,type='s',xlab='',ylab='Max Value',lty=2,col=gray(0.5))
plot(mydata$avg_ok,type='s',xlab='',ylab='',new=F)
twoord.stackplot(lx = mydata$id,rx = mydata$id,ldata = mydata$avg_ok,rdata = mydata$max_ok,
	ltype = 'l',rtype = 'l',rcol = gray(0.5),lcol='black',
	lylab='Mean',rylab='Max Value',mar=c(3,3,3,3),las=2)

##alpha=1
set.seed(2222)
pare <- data.frame(id=1:1e7,pareto_value=rpareto(1e7, scale = 1, shape=1) )
pare$seq <- (pare$id-1) %/% 5e4 +1 
mydata <- ddply(.data = pare,.variables = .(seq),.fun = summarise,id=max(id),avg=mean(pareto_value),max=max(pareto_value))
mydata$avg_ok <- cumsum(mydata$avg)/cummax(mydata$seq)
mydata$max_ok <- cummax(mydata$max)
# mydata[,c('id','avg_ok','max_ok')]
plot(mydata$max_ok,type='s',xlab='',ylab='Max Value',lty=2,col=gray(0.5))
plot(mydata$avg_ok,type='s',xlab='',ylab='',new=F)
twoord.stackplot(lx = mydata$id,rx = mydata$id,ldata = mydata$avg_ok,rdata = mydata$max_ok,
	ltype = 'l',rtype = 'l',rcol = gray(0.5),lcol='black',
	lylab='Mean',rylab='Max Value',mar=c(3,3,3,3),las=2)

##alpha=1.2
set.seed(2222)
pare <- data.frame(id=1:1e7,pareto_value=rpareto(1e7, scale = 1.2, shape=1) )
pare$seq <- (pare$id-1) %/% 5e4 +1 
mydata <- ddply(.data = pare,.variables = .(seq),.fun = summarise,id=max(id),avg=mean(pareto_value),max=max(pareto_value))
mydata$avg_ok <- cumsum(mydata$avg)/cummax(mydata$seq)
mydata$max_ok <- cummax(mydata$max)
# mydata[,c('id','avg_ok','max_ok')]
plot(mydata$max_ok,type='s',xlab='',ylab='Max Value',lty=2,col=gray(0.5))
plot(mydata$avg_ok,type='s',xlab='',ylab='',new=F)
twoord.stackplot(lx = mydata$id,rx = mydata$id,ldata = mydata$avg_ok,rdata = mydata$max_ok,
	ltype = 'l',rtype = 'l',rcol = gray(0.5),lcol='black',
	lylab='Mean',rylab='Max Value',mar=c(3,3,3,3),lty=c(1,2))



#Q3
set.seed(300)
repeats <- 60
tosses <- 8
heads <- function(tosses,p) {
	h <- 0
	for (i in 1:tosses) {
       h<- h+ ifelse(runif(1)<p, 1, 0)
	}
  return(h)
}
x <- data.frame(p=numeric(),h=numeric())
p<-0 
rn <-1
while(p<1.01){
	for (i in 1:repeats) {
		x[rn,1] <- p
		x[rn,2] <- heads(tosses,p)
		rn <- rn + 1
	}
	p <- p +0.05
}
plot(jitter(x$p),jitter(x$h),xlab='Balance Parameter P',ylab='Number of Heads Observed')
abline(h=6,lty=3)
abline(v=0.5,lty=3)
