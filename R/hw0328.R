


x <- read.csv('Hubbard Brk valley_soil_chem1997-1998.csv',header = T)
mydata <- x[,c('PH','OM.pct','Al','Ni','EXCH_K','EXCH_Pb','VERYFINESAND')]

mydata$Ni[mydata$Ni<0] <- NA
mydata$Al[mydata$Al<0] <- NA
mydata$EXCH_K[mydata$EXCH_K<0] <- NA
mydata$EXCH_Pb[mydata$EXCH_Pb<0] <- NA




#---------


#PH
#"Shapiro-Wilk normality test for power-transformed PH : W:  0.9944 : p-value: 0.4562 : power term: 4.69"




#VERYFINESAND
#"Shapiro-Wilk normality test for power-transformed VERYFINESAND : W:  0.9928 : p-value: 0.2412 : power term: 0.272"






qqnorm(x$PH)
qqline(x$PH)

require(car)
names.of.cols <- names(mydata)

	par(mfrow=c(3,3))
for (i in 1:7) {
	xx <- na.omit(mydata[,i])
	qqnorm(xx,main=names(mydata)[i])
    qqline(xx)
}



for ( i in c(1:7)){
print("--------------------------")
print(paste0("Normality tests for ",names.of.cols[i]," (column ",i,")"))
xx <- na.omit(mydata[,i])
if(min(xx,na.rm = T)>0) pt1 <- powerTransform(xx)
sw0 <- shapiro.test(xx)
if(min(xx,na.rm = T)>0) sw1 <- shapiro.test((xx)^as.vector(pt1$lambda))
if(min(xx,na.rm = T)>0) sw2 <- shapiro.test(log10(xx))
if(min(xx,na.rm = T)>0) sw3 <- shapiro.test(log(xx))
sw4 <- shapiro.test(sqrt(xx))
if(min(xx,na.rm = T)>0 &min(1-xx,na.rm = T)>0) sw5 <- shapiro.test( log(xx/(1-xx)) )
sw6 <- shapiro.test(scale(xx))

print(paste(sw0$method,"for",names.of.cols[i],": W:", round(sw0$statistic,4),
": p-value:", round(sw0$p.value,4)))
if(min(xx,na.rm = T)>0) print(paste(sw1$method,"for power-transformed", names.of.cols[i],": W: ", round(sw1$statistic,4),
": p-value:",round(sw1$p.value,4), ": power term:", round(as.vector(pt1$lambda),3)))
if(min(xx,na.rm = T)>0) print(paste(sw2$method,"for log10-transformed", names.of.cols[i],": W:", round(sw2$statistic,4),
": p‐value:", round(sw2$p.value,4)))
if(min(xx,na.rm = T)>0) print(paste(sw3$method,"for log-transformed", names.of.cols[i],": W:", round(sw3$statistic,4),
": p‐value:", round(sw3$p.value,4)))
print(paste(sw4$method,"for sqrt-transformed", names.of.cols[i],": W:", round(sw4$statistic,4),
": p‐value:", round(sw4$p.value,4)))
if(min(xx,na.rm = T)>0 &min(1-xx,na.rm = T)>0) print(paste(sw5$method,"for logit-transformed", names.of.cols[i],": W:", round(sw5$statistic,4),
": p‐value:", round(sw5$p.value,4)))
print(paste(sw6$method,"for scale-transformed", names.of.cols[i],": W:", round(sw6$statistic,4),
": p‐value:", round(sw6$p.value,4)))

}



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
