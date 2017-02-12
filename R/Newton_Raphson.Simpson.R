
#--------------------------------------------------------
#Q3
#--------------------------------------------------------

# first function 
normprob <- function(mu,sigmasq,a,b,n=100) {
 #a must be less than b
 try(if(a > b) stop("a must be less than b "))
 # exact probability
 p.exact <- pnorm(q = b,mean = mu,sd = sigmasq)- pnorm(q = a,mean = mu,sd = sigmasq)
 # Simpson approximation to the probability
 h <- (b-a) / n 
 x.vec <- seq(from = a,to = b,by = h)
 y.values <- dnorm(x = x.vec,mean = mu,sd = sigmasq)
 y.times <- c(1,rep(c(4,2), (n-2)/2 ),4,1)
 p.approx <- h/3 * (sum(y.values * y.times) )
 #the difference p.approx-p.exact
 p.error <- p.approx-p.exact
 # return list
 return(list(p.exact=p.exact,
 	         p.approx=p.approx,
 	         n=n,
 	         p.error=p.error
 	         )
        )

}


#  second function
SimpsonTest <- function(mu,sigmasq,a,b,n.grid){
 #a must be less than b
 try(if(a > b) stop("a must be less than b "))
 # use normprob()to obtain a vector of approximation errors for each value of n in n.grid
 normprob.rlt <- t(sapply(X =n.grid ,
 	                      FUN = normprob,
 	                      mu=mu,
 	                      sigmasq=sigmasq,
 	                      a=a,
 	                      b=b,
 	                      simplify = T
 	                      )
                    )
 # Coerce normprob.rlt to a Data Frame
 normprob.rlt <- as.data.frame(normprob.rlt)
 # a vector containing the absolute values of the approximation errors
 p.error <- abs(unlist(normprob.rlt$p.error))
 # use lm() to regress the 
 # log of the absolute approximation error against 
 # the log of n.grid, 
 # and take the estimated slope of the regression as an estimate of -alpha
 lm1 <- lm( log(p.error)~log(n.grid))
 # the slope of the regression line is an estimate of -alpha.
 alpha<- -lm1$coefficients[2]
 # return list
 return(list(n.grid=n.grid,
             p.error=p.error,
             alpha=alpha)
        )
}


##  test function 1
normprob(mu = 2,sigmasq = 1,a = -3,b = 3,n=10)
normprob(mu = 2,sigmasq = 1,a = -3,b = 3,n=100)

##  test function 2
SimpsonTest(mu = 2,sigmasq = 1,a = -3,b = 3,n.grid =seq(2,100,2) )


#--------------------------------------------------------
#Q4
#--------------------------------------------------------


polyopt <- function(a,x0,tol=1e-6,MaxIter=100){

 # create a function  called base_a_diff which calculate h(x) , h'(x)  by giving value x
 base_a_diff <- function(x){
    # primitive function exponential
    pe <- seq(from=0,to=length(a)-1,by = 1)
    # derivative function exponential
    de <- seq(from=0,to=length(a)-2,by = 1)
    # derivative function coefficients
    dc <- a[-1] * pe[-1]
    # the second derivative function exponential
    de2 <- seq(from=0,to=length(a)-3,by = 1)
    # the second  derivative function coefficients
    dc2 <- dc[-1] * de[-1]
    # primitive function value
    pv <- sum(a * x ** pe)
    # derivative function value
    gradient <- sum(dc * x ** de)
    # the second  derivative function value
    hessian <- sum(dc2 * x ** de2)
    return(list( pv=pv,gradient=gradient,hessian=hessian) )
 }

 # Initialization
 pv <- base_a_diff(x=x0)$pv
 gradient <- base_a_diff(x=x0)$gradient
 hessian <- base_a_diff(x=x0)$hessian
 odds <- Inf
 n <- 1 
 # run Newton-Raphson algorithm
 while( abs(gradient)>=tol  &&  abs(odds) >=tol && n <= MaxIter  ){
  x0 <- x0- gradient/hessian
  gradient <- base_a_diff(x=x0)$gradient
  hessian <- base_a_diff(x=x0)$hessian
  n <- n+1
  odds <- (pv - base_a_diff(x=x0)$pv) / base_a_diff(x=x0)$pv
  pv <- base_a_diff(x=x0)$pv
 }

 # return list
 return(list(x=x0,
 	         gradient =gradient,
 	         hessian =hessian,
 	         N.iter=n
 	         )
        )

}



# test function

# 1+x-2 x^2
polyopt(a = c(1,1,-2),x0 = 500)
