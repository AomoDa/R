
myf <- function(init,vec,cost_yesterday=1/3,bound=2,plot=TRUE) {
	cost_today <- 1- cost_yesterday
	n <- length(vec)
	rt <- numeric(n)
    # loop compute today value
	for (i in 1:n) {
		if(i==1){
			a <- init
		}else{
			a <- rt[i-1]
		}
		rt[i] <- a * cost_yesterday + vec[i] * cost_today
	}
	if(plot){
		plot(1:n,rt,type='b',pch=16,xlab='',ylab='')
		abline(h=bound,lty=2,lwd=1.5,col='red')		
	}
	out <- match(TRUE,rt<2)
	into <- match(TRUE,rt>=2)
	return(list(rt=rt,out=out,into=into))
}
