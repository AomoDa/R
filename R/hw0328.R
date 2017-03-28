


x <- read.csv('Hubbard Brk valley_soil_chem1997-1998.csv',header = T)
mydata <- x[,c('PH','OM.pct','Al','Ni','EXCH_K','EXCH_Pb','VERYFINESAND')]

mydata$Ni[mydata$Ni<0] <- NA
mydata$Al[mydata$Al<0] <- NA
mydata$EXCH_K[mydata$EXCH_K<0] <- NA
mydata$EXCH_Pb[mydata$EXCH_Pb<0] <- NA




#---------


#PH
#"Shapiro-Wilk normality test for power-transformed PH : W:  0.9944 : p-value: 0.4562 : power term: 4.69"
shapiro.test(mydata$PH^4.69)

#OM.pct
# log
shapiro.test(log(mydata$OM.pct))

#Al
# -0.11
shapiro.test(mydata$Al^-0.11)

#Ni
# no
shapiro.test(mydata$Ni)

#EXCH_K
shapiro.test(mydata$EXCH_K^0.7)

#EXCH_Pb
qqPlot(mydata$EXCH_Pb^0.5)

#VERYFINESAND
#"Shapiro-Wilk normality test for power-transformed VERYFINESAND : W:  0.9928 : p-value: 0.2412 : power term: 0.272"





#----------------



# hw2

#Q1

x <- read.csv('q1.csv')

shapiro.test(x$X2012)
shapiro.test(x$X2015)
boxplot(x[,-1],main='F.1 BoxPlot of breeding Blue Swimmer Crabs \n in   2012 VS 2015')

with(x,t.test(X2012,X2015,paired = T,alternative = 'greater'))


#Q2
x <- read.csv('q2.csv',header = T,na.strings='null')

x$group <- ifelse(x$Year >=1945 & x$Year<=1960, 'Y1945-1960', ifelse(x$Year >=2000 & x$Year<=2015, 'Y2000-2015', NA))
x$winter_rainfall <- x$Jun+x$Jul+x$Aug
mydata <- na.omit(x[,c('Year','group','winter_rainfall')])

shapiro.test(log(mydata$winter_rainfall))

boxplot(log(winter_rainfall)~group,data=mydata,main='F.2 BoxPlot of log-transformed winter rainfall')

t.test(log(winter_rainfall)~group,data=mydata)
