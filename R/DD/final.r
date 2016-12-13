

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

ddply(.data = rt,.variables = .(size),
	.fun = summarise,bias_mean=mean(bias),bias_upp=quantile(bias,0.975),bias_low=quantile(bias,0.025))
