

## get full data
get_full_data <- function(x) {
   x$date <- as.Date(unlist(str_extract_all(x$time,
                    pattern = '[0-9]{4}[-][0-9]{2}[-][0-9]{2}')))
   x$hour <- unlist(str_extract_all(x$time,
                    pattern = '[0-9]{2}$'))
   init <- nrow(x) +1 
   max_date <- max(x$date)
   unhour <- unique(x$hour)
   i <- min(x$date)
   while (i <=max_date) {    
        for (j in unhour) {
             a <- nrow(x[x$date==i & x$hour==j,])
             if(a==0) {
                	  x[init,1]  <- paste(i,j,sep=' ')
                    x[init,3]  <- i
                    x[init,4]  <- j
                    init <- init+1
              }
        }
      i <- i +1
    }
  x <- x[with(x,order(date,hour)),]
  return(x)
}



## Multivariate Imputation by Chained Equations (MICE)
mice_impute_pmm <- function(x) {
  require(mice)
  x$ind <- 1:nrow(x)
  mice_model <- mice(x[,c('amount','ind')],method='pmm',m=5)
  x$complete_data <- complete(mice_model)[,'amount']
  return(x[,-5])
  x$hour <- as.numeric(x$hour)
}
