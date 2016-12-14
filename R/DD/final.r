


#Q2
##a
set.seed(200)
x <- rexp(n = 10000,rate = 1)
stop_time <- c(2,4,6,8)

for (i in stop_time) {
	a <- x
	a[a>i] <- i
	a <- as.numeric(a)
	assign(paste0('a',i),value = a)
}

plot.ecdf(x,lty=1,lwd=1,xlim=c(0,10))
plot.ecdf(a8,col='red',lty=1,lwd=2,add=T,xlim=c(0,10))
plot.ecdf(a6,col='blue',lty=1,lwd=1,add=T,xlim=c(0,10))
plot.ecdf(a4,col='green',lty=1,lwd=2,add=T,xlim=c(0,10))
plot.ecdf(a2,col='yellow',lty=1,lwd=1,add=T,xlim=c(0,10))
legend('bottomright',lty=1,lwd=c(1,2,1,2,1),
	col=c('black','red','blue','green','yellow'),
	legend=c('Xi','X*=8','X*=6','X*=4','X*=2'),ncol=2 )


##b

set.seed(2000)
x <- rexp(n = 10000,rate = 1)
# set x*
stop_time <- c(1,2,5)
for (i in stop_time) {
	a <- x
	a[a>i] <- i
	 a <- as.numeric(a)
	assign(paste0('a',i),value = a)
}

1/mean(a1)-1
1/mean(a2)-1
1/mean(a5)-1


#Q3

#Q4

##a
set.seed(300)
x <- rnorm(100000)
round(mean(abs(x)),2)
round(var(abs(x)),2)


#q5
library(ggplot2)
library(plyr)

load('final2016.RData')
duration <- Q2_16$duration

sizes <- seq(from = 10,to = 100,by = 10)
true_med <- median(duration)
set.seed(500)
rt <- data.frame(stringsAsFactors = F)
for (i in sizes) {
	a <- replicate(n=1000,median(sample(x=duration,size=i,replace=T)))
	a <- a - true_med
	rt <- rbind(rt,data.frame(size=i,bias=a))
}

ggplot(data=rt,aes(x=as.factor(size),y=bias,fill=as.factor(size)))+
   geom_boxplot(show.legend=F)+
   labs(x='size',title='boxplot')++lims(y=c(-10,15))

ddply(.data = rt,.variables = .(size),
	.fun = summarise,bias_mean=round(mean(bias),2),
	bias_upp=round(quantile(bias,0.975),2),
	bias_low=round(quantile(bias,0.025),2))


#Q6
day_of_week <- Q2_16$day.of.week
aggregate(duration~day_of_week,FUN = mean)


#PERMUTATION TESTS
#H0: U_E > U_W
#H1: U_E <= U_W

N <- 1e3-1
result <-c()
len <- length(duration)
set.seed(600)
for (i in 1:N) {
   ind <- sample(len,size=248322,replace=F)
   result[i] <- mean(duration[ind]) - mean(duration[-ind])
}
observed <- 26.35046-18.24986
(sum(result <= observed) + 1)/(N + 1)


ggplot(data=Q2_16,aes(x=duration,y=..density..,fill=day.of.week))+
     geom_histogram(bins = 50,col=I('white'),show.legend = F)+
     lims(x=c(1,100))+facet_wrap(~day.of.week)


#F test to compare two variances
var.test(duration~day_of_week)
#Welch Two Sample t-test
library(gplots)

t.test(duration~day_of_week,var.equal=F,alternative='less')


#Q7

#### ans:7

duration_reg <- subset(Q2_16,subset =account=='Registered',select = duration,drop = T)
duration_cas <- subset(Q2_16,subset =account=='Casual',select = duration,drop = T)


set.seed(700)

N_size <- seq(from=1,to = 10,by = 1)

p<-c()

for (i in N_size) {
	a <- data.frame(low=numeric(),upp=numeric())
	for (j in 1:100) {
    diff_mean<- replicate(200,mean(sample(duration_cas,size=i))-mean(sample(duration_reg,size=i)))
    a[j,1] <- quantile(diff_mean,0.025)
    a[j,2] <- quantile(diff_mean,0.975)
   }
print(1 -mean(a$low<0 & a$upp>0))
}



#Bonus question


repear_shop <- function(x) {
    rn <-length(x)
	if( rn==1) a <- 0
	if(rn>1){
		avg <- mean(x)
		xsd <- sd(x)
		upp <- avg + 3 * xsd
		a <- sum( x >upp  )
	}
	return(a)
}

a <- ddply(.data = Q2_16[,c(1,4,6)],.variables = .(`start.number`,`end.number`),.fun = summarise,repear_num =repear_shop(duration),rn=length(duration) )

sum(a$repear_num)/nrow(Q2_16)*100
