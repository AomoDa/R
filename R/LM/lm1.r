

x <- read.csv('UsedCarPricingData.csv',header = T)
x$Leather <- as.factor(x$Leather)
x$Sound <- as.factor(x$Sound)
x$Cruise <- as.factor(x$Cruise)
x$Doors <- as.factor(x$Doors)

#--------------------------------------
# initial analysis,
#-------------------------------------

#summary
summary(x[,c('Price','Mileage','Liter')])

# Histogram
par(mfrow=c(1,3))
hist(x$Price,probability = T,breaks = 30,main='Histogram of Price',xlab='Price')
lines(density(x$Price),col='red')
hist(x$Mileage,probability = T,breaks = 30,main='Histogram of Mileage',xlab='Mileage')
lines(density(x$Mileage),col='blue')
hist(x$Liter,probability = T,breaks = 10,main='Histogram of Liter',xlab='Liter')
lines(density(x$Liter),col='orange')
par(mfrow=c(1,1))




# cor

# scatt plot
ggplot(data=x,aes(x=Mileage,y=Price))+geom_point()+geom_smooth(method = 'lm')

# lm

lm1 <- lm(Price~Mileage,data=x)
plot(allEffects(lm1))
summary(lm1)
coef(lm1)
confint(lm1)

#--------------------------------------
# improve model
#-------------------------------------


# boxplot
boxplot(Price~Model,data=x,main='boxplot of Model')
boxplot(Price~Trim,data=x,main='boxplot of Trim')

par(mfrow=c(1,3))
boxplot(Price~Make,data=x,main='boxplot of Make')
boxplot(Price~Type,data=x,main='boxplot of Type')
boxplot(Price~Cylinder,data=x,main='boxplot of Cylinder')
par(mfrow=c(1,1))


#variety of characteristics.
t.test(Price~Doors,data=x)
t.test(Price~Sound,data=x)
t.test(Price~Cruise,data=x)
t.test(Price~Leather,data=x)




#lm

# forward
stepAIC(object = lm1,scope = list(upper=~Mileage+Make+Type+Cylinder+Liter+Doors+Cruise+Sound+Leather,lower=~1),direction = 'forward')
# backward
lm2 <- lm(Price~Mileage+Make+Type+Cylinder+Liter+Doors+Cruise+Sound+Leather,data=x)
stepAIC(lm2,direction = 'backward')

#my model
lm3 <- lm(formula = Price ~ Mileage + Make + Liter + Type + Cylinder, data = x)
anova(lm2,lm3)


#residual analyses
par(mfrow=c(1,3))
plot(residuals(lm3),main='residuals plot ',xlab='',ylab='residuals',pch=16,col=gray(0.15),ylim=c(-8000,8000))
abline(h=0,col='orange',lty=1,lwd=2)
hist(residuals(lm3),breaks = 50,probability = T,xlim=c(-5000,5000))
lines(density(residuals(lm3)),col='blue')
qqPlot(lm3,main='Q-Q plot of residuals')
par(mfrow=c(1,1))

residualPlot(lm3,main=' residualPlot VS Fitted values')

#influencePlot
influencePlot(lm3,id.method = 'identyfy',main='influencePlot')

# multicollinearity
vif(lm3)



##
plot(allEffects(lm3))


relweights <- function(fit, ...) {
    R <- cor(fit$model)
    nvar <- ncol(R)
    rxx <- R[2:nvar, 2:nvar]
    rxy <- R[2:nvar, 1]
    svd <- eigen(rxx)
    evec <- svd$vectors
    ev <- svd$values
    delta <- diag(sqrt(ev))
 
    # correlations between original predictors and new orthogonal variables
    lambda <- evec %*% delta %*% t(evec)
    lambdasq <- lambda^2
 
    # regression coefficients of Y on orthogonal variables
    beta <- solve(lambda) %*% rxy
    rsquare <- colSums(beta^2)
    rawwgt <- lambdasq %*% beta^2
    import <- (rawwgt/rsquare) * 100
    lbls <- names(fit$model[2:nvar])
    rownames(import) <- lbls
    colnames(import) <- "Weights"
 
    # plot results
    barplot(t(import), names.arg = lbls, ylab = "% of R-Square",
        xlab = "Predictor Variables", main = "Relative Importance of Predictor Variables",
        sub = paste("R-Square = ", round(rsquare, digits = 3)),
        ...)
    return(import)
}
