library(foreign);library(car)
CloseVote <- read.spss("M:\\Lectures2016\\ECON6306\\Chapt07\\SchoolClosing.sav", to.data.frame=TRUE)

sapply(CloseVote,is.factor)

## Just intercept
GLM.00 <- glm(close ~ 1, family=binomial(logit), trace=TRUE, data=CloseVote)
summary(GLM.00)
logLik(GLM.00)

1/(1+exp(-(coef(GLM.00)[1])))            # predicted prob in favor of closing 1/(1+exp(-b_0)) 
mean(unclass(CloseVote$close)-1)         # same as 1/(1+exp(-b_0))

## Just variable "lived" and intercept
GLM.01 <- glm(close ~ lived, family=binomial(logit), trace=TRUE, data=CloseVote)
summary(GLM.01)
logLik(GLM.01)

## Likelihood Ratio Test
( LR <- -2*(logLik(GLM.00)-logLik(GLM.01)) )
( pchisq(LR[1], df=1, lower.tail=F) )
anova(GLM.00,GLM.01,test="LRT")

## The probit model 
GLM.probit <- glm(close ~ lived, family=binomial(link=probit), trace=TRUE, data=CloseVote)
summary(GLM.probit)
logLik(GLM.probit)

## Exploratory plots: barwidth proportionally to the count in interval 
plot(close~lived, data=CloseVote)
plot(close~educ, data=CloseVote)
plot(close~contam, data=CloseVote)
plot(close~hsc, data=CloseVote)
plot(close~female, data=CloseVote)
plot(close~nodad, data=CloseVote)

## Full model
GLM.02 <- glm(close ~ lived + educ + contam + hsc + nodad + female + kids ,
  family=binomial(logit), trace=TRUE, data=CloseVote)
summary(GLM.02, correlation=TRUE)
vif(GLM.02)
logLik(GLM.02)
confint(GLM.02, level=0.95, type="Wald")

## Restricted model without female and kids
GLM.03 <- glm(close ~ lived + educ + contam + hsc + nodad ,
             family=binomial(logit), data=CloseVote,
             control=list(epsilon=1e-15,maxit=50, trace=TRUE))
summary(GLM.03, correlation=TRUE)
logLik(GLM.03)

## Likelihood Ratio Test
( LR <- -2*(logLik(GLM.03)-logLik(GLM.02)) )
( pchisq(LR[1], df=2, lower.tail=F) )
anova(GLM.03,GLM.02,test="LRT")

##
## Effect plots (note: book uses average educ=12.95 for all models)
##
library(effects)     # Important: Use version 3.0-6 or newer

## all independent variables with the others at average
plot(allEffects(GLM.03), type="response", ylim=c(0,1), ask=FALSE)

## Group specifice effect plots
summary(CloseVote)





# Low prob respondent
eff.GLM.low <- effect("lived",GLM.03,xlevels=20,
                      given.values=c(educ=6,"contamyes"=0,"hscyes"=0,"nodadyes"=1))
plot(eff.GLM.low, type="response", ylim=c(0,1), ylab=expression(Pr(Y[i]=="Close")), 
     main="Low Probability Respondents")
# Average prob respondent
eff.GLM.average <- effect("lived",GLM.03,xlevels=20)
plot(eff.GLM.average, ylim=c(0,1), type="response", ylab=expression(Pr(Y[i]=="Close")),    # ylim is in terms of probs
     main="Average Probability Respondents")


# High prob respondent
eff.GLM.hi <- effect("lived",GLM.03,xlevels=20,
                     given.values=c(educ=20,"contamyes"=1,"hscyes"=1,"nodadyes"=0))
plot(eff.GLM.hi, type="response", ylim=c(0,1), ylab=expression(Pr(Y[i]=="Close")), 
     main="High Probability Respondents")





## Alternative generation of effect plots
invlogit <- function(x) 1/(1+exp(-x))   # inverse logit function

plot(CloseVote$lived, CloseVote$vote, type="n", ylim=c(0,1), xlim=c(0,80),                            # Define empty plot frame           
     ylab=expression(Pr(Y[i]=="close")), 
     xlab="Years Lived in Town", main="Conditional Effects Plot")

rug(CloseVote$lived[as.numeric(CloseVote$close)-1==0])                                  # "no"  obs on bottome
rug(CloseVote$lived[as.numeric(CloseVote$close)-1==1], side=3)                          # "yes" obs on top
abline(h=c(0,1),lty=3)                                                                  # 0,1 bounding limits
curve(invlogit(cbind(1,x,20,0,0,0) %*% coef(GLM.03)), col="red", lwd=2, add=TRUE)       # Low prob curve
curve(invlogit(cbind(1,x,6,1,1,1) %*% coef(GLM.03)), col="green", lwd=2, add=TRUE)      # High prob curve
legend("topright",legend=c("High Prob Respondent", "Low Prob Respondent"),              # legend
       title="Respondent's Profile",       lwd=2,col=c("green","red"))

## Residual Exploration of Response Residuals
resid.GLM.03 <- residuals(GLM.03, type="response")   # check ?residuals.glm
pred.GLM.03 <- predict(GLM.03, type="response")      # check ?predict.glm
plot(pred.GLM.03, resid.GLM.03, ylim=c(-1,1), xlim=c(0,1), 
     ylab="Response Residuals", xlab="Predicted Probabilities")
abline(h=0, lty=5)
lines(lowess(pred.GLM.03, resid.GLM.03),lwd=2)  # Smoothed function to see the residual behavior

## Residual Exploration of Pearson Residuals (standardized residuals)
resid.GLM.03 <- residuals(GLM.03, type="pearson")   
pred.GLM.03 <- predict(GLM.03, type="response")      
plot(pred.GLM.03, resid.GLM.03, xlim=c(0,1), 
     ylab="Pearson Residuals", xlab="Predicted Probabilities")
abline(h=0, lty=5)
lines(lowess(pred.GLM.03, resid.GLM.03),lwd=2)  # Smoothed function to see the residual behavior

##
## High Discrimination. Quasi-complete Separtion
##
(tab <- xtabs(~close+nodad, data=CloseVote))
RcmdrMisc::colPercents(tab) # Column Percentages
chisq.test(tab, correct=FALSE)

## removing the 4 nodad observations in favor of closing
CloseVoteSel <- subset(CloseVote, !(close=="close" & nodad=="yes"))
xtabs(~close+nodad, data=CloseVoteSel)

GLM.04 <- glm(close ~ lived + educ + contam + hsc + nodad ,
              family=binomial(logit), data=CloseVoteSel,
              control=list(epsilon=1e-15,maxit=50, trace=TRUE))
summary(GLM.04)
logLik(GLM.04)

## Another high discrimination example
y <- c(0,0,0,0,0,1,1,1,1,1)
x <- c(1,2,3,4,5,6,7,8,9,10)
glmHiDis <- glm(y~x, family=binomial(logit), control=list(epsilon=1e-15,maxit=50, trace=TRUE))
summary(glmHiDis)

invlogit <- function(x) 1/(1+exp(-x))   # inverse logit function

plot(x, y, type="n", ylim=c(0,1), 
     ylab="Prob(Y=1)", xlab="X", main="Conditional Effects Plot")
rug(x[y==0])
rug(x[y==1], side=3)
abline(h=c(0,1),lty=3)
curve(invlogit(cbind(1,x) %*% coef(glmHiDis)), lwd=2, add=TRUE)

##
## Logistic Regression for grouped data
##
insects <- data.frame(popDensity=c(1,4,10,22,55,121,210,444),
                      females=c(1,3,7,18,22,41,52,79),
                      males=c(0,1,3,4,33,80,158,365))
insects

## Weighted GLM specification
insects$totalPop <- insects$females+insects$males
insects$rateMale <- insects$males/insects$totalPop
glm.weight <- glm(rateMale~log(popDensity), weight=totalPop, data=insects, family=binomial)
summary(glm.weight)

## cbind GLM specification
bothSex <- cbind(insects$males,insects$females)
glm.cbind <- glm(bothSex~log(popDensity), data=insects, family=binomial)
summary(glm.cbind)

## Plot prediction
xAxis <- seq(0,7,by=0.001)                                                    # value range of log(popDensity)
probPred <- predict(glm.cbind,list(popDensity=exp(xAxis)), type="response")   # type="response" give predicted probabilities
plot(log(insects$popDensity), insects$rateMale, ylim=c(0,1), xlim=c(0,7), 
     ylab=expression(Pr(Y[i] =="Male")),xlab=expression(log("Population Density")), 
     pch=16, col="blue")
lines(xAxis,probPred, col="red")
abline(h=c(0,1),lty=3)

