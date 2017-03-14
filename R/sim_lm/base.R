

#------------------------
#Part 1
#------------------------
# ... your comments on next line here ...
# loading data into R
tree = read.csv(file="trees91.csv",header=TRUE,sep=",")
median(tree$LFBM) # Compute the LFBM  median.
mean(tree$LFBM) # Compute the LFBM Arithmetic Mean .
quantile(tree$LFBM) # Compute the LFBM  Quantiles.
max(tree$LFBM) # Compute the LFBM  Maxima.
min(tree$LFBM) # Compute the LFBM  Minima.
sd(tree$LFBM) # Compute the LFBM  Standard Deviation.
var(tree$LFBM) #Compute the LFBM  Variance.


#------------------------
#Part 2
#------------------------

q2 <- matrix(data = 1:8,nrow = 2)
row.names(q2) <- c('row1','row2')
colnames(q2) <- c('col1','col2','col3','col4')


#------------------------
#Part 3
#------------------------

#Create a 3x4 matrix
q3 <- matrix(nrow = 3,ncol = 4)
# double loop
for (i in 1:3) {
  for (j in 1:4) {
    q3[i,j] <- i * j 
  }
}


#------------------------
#Part 4
#------------------------

# generate a set of 10 figures
q <- seq(from=-3,to=3,length.out = 10)
# Calculate the cumulative distribution
p <- pnorm(q = q)
# Plot ’p’ against ’q’.
plot(x = p,y = q,type = 'l')

#------------------------
#Part 5
#------------------------

## Three lines of R code
# generate 10000 values randomly drawn from anormal distribution with a mean of 5 and a standard deviation of 2
q5 <- rnorm(n = 10000,mean = 5,sd = 2)
# is the values between 4 and 6
a <- q5 >=4 & q5 <=6
# compute probability
mean(a)

## One line   of R code
pnorm(q = 6,mean = 5,sd = 2) - pnorm(q = 4,mean = 5,sd = 2)

#------------------------
#Part 6
#------------------------

## One line   of R code
pt(q = 2.5,df = 12)-pt(q = 2,df = 12)

#------------------------
#Part 7
#------------------------

#Draw 10,000 random numbers
q7 <- rt(n = 10000,df = 12)
#Make a histogram
hist(q7,breaks = 50)


#------------------------
#Part 8
#------------------------

X <- 1:14
Y <- c(-99.59, -95.33, -93.62, -90.84, -92.87, -78.96, -70.30, -48.20,
       -26.29, 20.98, 93.12, 218.08, 413.57, 729.16)
Z <- c(3.975368 , 3.49, 7.06, 5.10, 9.21, 14.24, 21.72, 27.94, 43.02,
       61.08, 84.60, 122.02, 181.75, 274.65)

plot(X,Y,col='red',type='l',ylab='')
points(X,Z,type='l',col='orange')
legend('topleft',legend = c('Y vs X','Z vs X'),col=c('red','orange'),lty=1)



#------------------------
#Part 9
#------------------------

#a
#Read in the data in the ’grades.csv’ file.
x <- read.csv('grades.csv',header=T)

#b


#c

#------------------------
#Part 10
#------------------------

# # generate 25 values randomly drawn from Exponential Distribution  with rate=4.2
x = rexp(25,rate=4.2)
#Make a histogram
hist(x,ylim=c(0,20),main="Distribution of X",xlab="X")
# Make a boxplot 
boxplot(x,at=19,horizontal=TRUE,add=TRUE)
# Add a Rug to a Plot
rug(x,side=1)
# Kernel Density Estimation with x 
d = density(x)
# add lines
points(d,type='l',col=3)
