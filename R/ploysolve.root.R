ploysolve <- function(a,lower,upper,tol=1e-6){
 # create a function callled h which returned h(x) 
 # by giving x and known vector a 
  h <- function(x){
    # primitive function exponential
    pe <- seq(from=0,to=length(a)-1,by = 1)
    pv <- sum(a * x ** pe)
    return(pv)
  }
 # if h(L) * h(U) >0 then return NA
 hl <- h(x=lower)
 hu <- h(x=upper)
 if(hl*hu>0){
 	root <- NA
 }
 # if h(L) * h(U) <=0 then calculate root
 if(hl*hu <= 0){
 	hm <- Inf
 	while( abs(hm) >=tol & abs(lower-upper)>= tol ){
   hl <- h(x=lower)
   hu <- h(x=upper)
   m <- lower - ((upper-lower)*hl)/(hu-hl)
   hm <- h(x=m)
   if(hm*hl < 0 ){upper <- m }
   if(hm*hl >= 0 ){lower <- m}
 	}
 	# return
 	if(abs(hm)<tol){
 		root <- round(m,4)
 		} else if(abs(lower-upper)<tol) {
 			root <- paste0('root between ',lower,' and ', upper)
 		}
 }
 #return root
 return(root)
}

#---------------------------------
# test function 
# x^2 +2*x =0
## -1 <= root <= 10
ploysolve(a=c(0,2,1),lower=-1,upper=10)
## -10 <= root <= -1
ploysolve(a=c(0,2,1),lower=-10,upper=-1)

#---------------------------------
