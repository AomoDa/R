
library(foreign); 
library(car)

##
## Standard Regression Diagnositcs
##
H2O <- read.spss("ConcordH2OTimeSeries.sav", use.value.labels = TRUE, to.data.frame = TRUE)

## Descriptive Plots
plot(H2O$time,H2O$H2Ouse, type="l", main="Monthly Water Use")
abline(v=seq(1,137,by=12),lty=3)

plot(H2O$time,H2O$temp, type="l", main="Monthly Temperature")
abline(v=seq(1,137,by=12),lty=3)

plot(H2O$time,H2O$rain,type="l", main="Monthly Rainfall Amount")
abline(v=seq(1,137,by=12),lty=3)

base.lm <- lm(H2Ouse~time+temp+rain+educ, data=H2O)
summary(base.lm, cor=T)
vif(base.lm)                            # VIF reports variances 1/(1-R^2) NOT standard deviations

ts.baseResid <- ts(residuals(base.lm),frequency=12,start=c(1970,1))
acf(ts.baseResid,xlab="lag in years")   # Hamilton Fig 4.6 (exclude time from lm)

## Full and half-year fourier cycles
H2O$y.cos <- cos(H2O$time/12*2*pi)                              # annual cycle
H2O$y.sin <- sin(H2O$time/12*2*pi)

H2O$h.cos <- cos(H2O$time/6*2*pi)                               # half year cycle
H2O$h.sin <- sin(H2O$time/6*2*pi)

## Full model
fullfourier.lm <- lm(H2Ouse~y.cos+y.sin+h.cos+h.sin+time+temp+rain+educ, data=H2O)
summary(fullfourier.lm)  
vif(fullfourier.lm)
anova(base.lm, fullfourier.lm)

## Reduced model
fourier.lm <- lm(H2Ouse~y.cos+y.sin+h.cos+h.sin+time+educ, data=H2O)
summary(fourier.lm,cor=T)                                       # Summary with correlation among Parameters

vif(fourier.lm)                                                 # Variance inflation factors
vcov(fourier.lm)                                                # covariance among estimated parameters
anova(fullfourier.lm, fourier.lm)

## Diagnostic plots
plot(fourier.lm, which = c(1:6), ask=T)                         # Diagnostic plots
avPlots(fourier.lm, id.n=4)                                     # Partial Effects Plots
qqPlot(fourier.lm, id.n=4)                                      # test residuals for normality
residualPlots(fourier.lm)                                       # check for adding squared independent variables
influenceIndexPlot(fourier.lm, id.n=3)                          # check for influential observations
                                                                # careful with scale Bonferroni p-values 

## Get residuals
resid <- residuals(fourier.lm)
std.resid <- rstandard(fourier.lm)
student.resid <- rstudent(fourier.lm)

## Other diagnositic measures
dfbeta.values <- dfbetas(fourier.lm)
cook.values <- cooks.distance(fourier.lm)
leverage.values <- hatvalues(fourier.lm)

## Leverage Plot not using residuals!!!
plot(H2O$time,H2O$H2Ouse,type="b",cex=abs(dfbeta.values[,6])*20)

## Evaluate standard errors using Delta method (see Fox p200) for annual and 6 month cycles
parms <- coefficients(fourier.lm)
ampY <- sqrt(parms[2]^2+parms[3]^2)
ampH <- sqrt(parms[4]^2+parms[5]^2)
ampYse <- deltaMethod(fourier.lm,"sqrt(y.cos^2+y.sin^2)")
ampHse <- deltaMethod(fourier.lm,"sqrt(h.cos^2+h.sin^2)")

fourierResult <- rbind(ampYse,ampHse)
row.names(fourierResult) <- c("Cycle 12 Months:","Cycle 6 Months:")
round(fourierResult,3)

##
## Time Series Analysis
##
ts.avg <- ts(H2O$H2Ouse, frequency = 12, start = c(1970, 1))
ts.avg <- ts(H2O$H2Ouse, deltat = 1/12, start = c(1970, 1))      # alternative but equivalent specification
plot(ts.avg)
ts.spec <- spectrum(ts.avg)          # spectral periodogram (cor^2 between series and sine/cosine for given period)
abline(v=c(1,2),lty=3)               # 1 and 2 waves per 12 month, i.e., annual and half-year cycles
plot(stl(ts.avg,"periodic"))         # decomposition into periodic, trend, remainder

## Durbin-Watson Test
durbinWatsonTest(fourier.lm, max.lag=5, simulate=TRUE, alternative="positive")

## Time Series original and moving average smoothed data
ts.res <- ts(residuals(fourier.lm), frequency = 12, start = c(1970, 1)) 
ts.res.ma <- filter(ts.res,filter=rep(1/3,3),sides=2)               # moving average with weights 1/3
ts.both <- ts.union(ts.res,ts.res.ma)                               # bind both series
plot(ts.both, plot.type = "single", ylab="Temperatur Residuals & Moving Average", xlab="Year",
     pch=20,type="b",lwd=c(1,1),col=c("red","blue"))
abline(h=0,lty=2,lwd=2)                                      
abline(v=c(1980+7/12))                                              # Intervention date

## Lags and Autcorrelation Functions
ts.lag1 <- lag(ts.res, k=1)                                         # First order lag
ts.lag2 <- lag(ts.res, k=2)                                         # second order lag
ts.union(ts.res,ts.lag1,ts.lag2)
plot(ts.res,ts.lag1,xy.labels=FALSE,xy.lines=TRUE)
cor(ts.res[2:120],ts.res[1:119])                                    # Correlation lag 1
acf(ts.res,xlab="lag in years")                                     # Autocorrelation function
pacf(ts.res,xlab="lag in years")                                    # Partial autocorrelation function

