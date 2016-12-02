

#-----------------------
#part1
#-----------------------

#load the data into R
lung<-read.csv("lung.csv",header=T)

##c
# compute the proportion of patients have 
# Physician's estimate of Karnofsky score 
# greater than/equal to/less than the patient's estimate?
summary(lung$ph.karno>=lung$pat.karno)

##d
# compute the rank of age
age_rank <- rank(lung$age,ties.method = 'first')
# Create a new variable which categorises age 
# into one of three categories, so that there are
# roughly the same number of individuals in each category.
lung$age_categorises <- cut(age_rank,breaks = 3,labels = c('A','B','C'))

#For each category, calculate the rows and age range
table(lung$age_categorises)
aggregate(age~age_categorises,data=lung,FUN =range)
#For each category, calculate the mean weight loss
aggregate(wt.loss~age_categorises,data=lung,FUN = mean)

#e
# define the range function 
range_value <- function(x){
  return(max(x)-min(x))
}

# Calculate the range, across indicator, of mean calories consumed
aggregate(meal.cal~status,data=lung,FUN =range_value)
#Calculate the range, across sex, of mean calories consumed
aggregate(meal.cal~sex,data=lung,FUN =range_value)

#f

#Plot a graph of calories consumed at meals VS survival time
ggplot(data=lung,aes(x=meal.cal,y=time))+geom_point(na.rm = T)+
   geom_smooth(method = 'lm',na.rm = T)+
   labs(title='calories consumed at meals VS survival time')


#-----------------------
#part2
#-----------------------

##a
# define g(x)=sin(x)^2
f <- function(x) {return(sin(x)^2)}

#Graph the function using plot
x <- seq(0,2*pi,length.out = 1000)
plot(x,f(x),type='l')



##b
#Write a function mcint that takes m as an argume
mcint <- function(m){
  a <- 0 # from
  b <- 2*pi # to 
  g <- replicate(m,f(runif(1,a,b))) #a random sample from a uniform distribution on the interval [a, b].
  rt <- (b-a) / m * sum(g) # approximation
  return(rt)
}

set.seed(1234)
#Use mcint function to approximate the integral using m = 100
mcint(100)

##c
# use integrate function  to calculate 
# the area under the curve between 0 and 2*pi
integrate(f = f,lower = 0,upper = 2*pi)

##d


##e
#generate a sequence from 1000 to 10000 by step 1 
mydat <- data.frame(m=seq(1000,10000,by = 1))

set.seed(2000)
# Use apply and mcint to approximate the area under the curve using m 
y <- apply(mydat,1,mcint)

#work out the ratio of your answer to the exact answer
ratio <- (y-pi) / pi *100

#Produce a line plot of this ratio against m.
plot(mydat$m,y,type='l')
