
##Q1

#a
myfun <- function(x) {
x <- as.vector(x)
a <- numeric(length(x))
for (i in 1:length(x)) {
 y <- x[i]
 a[i] <- ifelse(y<10,y^2-7*y-5,ifelse(y>=20,0.5*y+25,y+15)  )
}
return(a)
}

##test
myfun(x = 20)
myfun(x = 1:25)1

##b
a <- seq(1,40,0.01)
plot(a,myfun(a),type='l')


#Q3

##a

myfun_q3 <- function(n){
 try(if(n<0) stop('error'))
 y <- as.vector(n)
 y[1] <-1
 y[2] <-2
 if(n==1) y <-1
 if(n==2) y <-c(1,2)
 if(n>2){
  y <-c(1,2)
  for (i in 3:n) { y <- c(y,y[i-1]+2/y[i-1])   }
 }
 return(y)
}

##test
myfun_q3(10)


##b

set.seed(33)
x <- replicate(100,rbinom(1,size = 40,prob = 0.2))
mean(x)
var(x)


#Q4

###a
x <- read.csv('Baseball2010.txt',header = T,stringsAsFactors = F,sep='\t')

###b
size <- x$Size
mean(size)
sd(size)

###c
pbinom(81,size = 162,prob = 0.5,lower.tail = F)



###f
# the same 
with(x,t.test(Salary~League))



###g
hist(x$Salary)
median(x$Salary)
range(x$Salary)


###h
with(x,boxplot(Salary~League))


###i

with(x[x$League==0,],plot(Wins,Salary,col='red'))
with(x[x$League==1,],points(Wins,Salary,col='blue'))

###j
attendence.1 <- ifelse(x$Attendance<2000,'A',ifelse(x$Attendance>=3000,'C','B'))
pie(table(attendence.1))


###q5

x <- read.table('clipboard',header = T)
with(x,t.test(Before,After,paired = T))
