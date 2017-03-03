
#Part 1 
# Description: Data from the 1970 military draft lottery. The lottery assigned numbers to potential draftees on the basis of birth date. 
# Variables are month, day of month, day of year, and draft number assigned to those born on that date. Men with lower draft numbers were drafted first.
# Number of cases: 366
# Variable Names:
# Day: Day of month (1-31)
# Month: Month of year
# Mo.Number: Month of year (1=January, 2=February, etc.)
# Day_of_Year: Day of year (1-366)
# Draft_No.: Draft number assigned to those born on this date
library(MASS)
x <- read.table('DraftLottery.txt',header = T,stringsAsFactors = F)
lo <- loess(Draft_No~Day_of_year,data=x,span = 0.2,degree = 1)
with(x,plot(Day_of_year,Draft_No,xlab='Day in Year',ylab='Draft Number'))
points(x$Day_of_year,lo$fitted,type='l',lwd=2)
abline(lm(Draft_No~Day_of_year,data=x),lty=2,lwd=2)



#Part 2
y <- read.csv('ResponseTime.txt',header = T)
y.cdf <- ecdf(y$ResponseTime)
plot(density(y$ResponseTime,width = 3,kernel = 'optcosine',bw = 0.05,adjust=0.1),main='',xlab='Response Time',ylab='')
a <- seq(0,500,0.1)
points(a,y.cdf(a),type='l',lty=2)
text(x=450,y = 0.8,labels = 'CDF')
text(x=450,y = 0.2,labels = 'KDE')
