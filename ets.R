require(fpp)

# Simple exponential smoothing
  oildata <- window(oil, start = 1996, end = 2007)
  plot(oildata, ylab = "Oil (millions of tonnes)", 
       xlab = "Year")
  fit <- ses(oildata,h = 3)
  plot(fit)  
  names(fit)
  
  # Not much difference between Naive and SES in this case
  # Notice the large alpha.  
  # alpha=0.89 89% of the weight on that last obs 
  # so little left for the previous obs
  # Notice the l0.
  fit$model
  lines(rwf(oildata,h=3)$mean,col='red')

  # Here are some diagnostics 
  accuracy(fit)
  sum(residuals(fit)^2)
  Acf(residuals(fit))

# Let's explore some values of alpha
  # and see their effect on the estimated level
  # and the fitted values
  fit <- ses(oildata,h = 3,alpha=0.8,initial="simple")
  plot(oildata,xlim=c(1995,2010))
  points(oildata)
  #Plot the estimated level 
  lines(fit$model$states,col='green', lty=3)  
  points(fit$model$states,col='green', lty=3)  
  #Plot the insample forecasts
  lines(fit$fitted,col='green')
  points(fit$fitted,col='green')
  lines(fit$mean,col='green') 
  points(fit$mean,col='green')  
  # Here the estimated level reacts a lot 
  # closely tracks the series
  
  fit <- ses(oildata,h = 3,alpha=0.2,initial="simple")
  lines(fit$model$states,col='red', lty=3)  
  points(fit$model$states,col='red', lty=3)  
  lines(fit$fitted,col='red')
  points(fit$fitted,col='red')
  lines(fit$mean,col='red')  
  points(fit$mean,col='red')  


# We've seen the following in the lecture slides
  # Here is the level estimate and forecasts
  # for some different values of alpha
  fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)
  fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
  fit3 <- ses(oildata, h=3)
  plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
  lines(fitted(fit1), col="blue", type="o")
  lines(fitted(fit2), col="red", type="o")
  lines(fitted(fit3), col="green", type="o")
  lines(fit1$mean, col="blue", type="o")
  lines(fit2$mean, col="red", type="o")
  lines(fit3$mean, col="green", type="o")
  legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)

# A plot of the strikes data
  plot(strikes, ylab = "No. of strikes in the US", 
     xlab = "Year")
  
  # Strikes data example
  # Forecasts look plausible
  # Notice the large prediction intervals
  plot(ses(strikes))

  # Note: we have not said anything yet on how
  # the predictions intervals are generated.


# # Strikes data
#   # I can print these out on screen and show 
#   # what happens as alpha decreases
# 
#   plot(strikes,ylab="No. strikes in US",xlab="Year",xlim=c(1950,1990))
#   
#   alpha <- seq(0.01,0.99,by=0.05)
#   mse <- numeric(length(alpha))
#   
#   for(i in 1:length(alpha))
#   {
#     fred <- ses(strikes,alpha=alpha[i])
#     mse[i] <- mean(fred$residuals^2)
#     plot(strikes,ylab="No. strikes in US",xlab="Year",xlim=c(1950,1990))
#     lines(fitted(fred),col=2)
#     lines(fred$mean,col=4)
#     text(1952,6000,bquote(alpha== .(format(alpha[i],digits=3,nsmall=2))))
#     text(1954,5700,paste("MSE=",round(mse[i])))
#   }
#   
#   # Let's have a look at the MSE in sample 
#   # when is it best=minimum
#   f1 <- ses(strikes)
#   f1$model
#   
#   plot(alpha,mse/1e3,type="l",ylab="MSE ('000)")
#   bestalpha <- f1$model$par[1]
#   lines(rep(bestalpha,2),c(0.0,1e6),col=2)
#   text(bestalpha+0.1,600,bquote(alpha==.(format(bestalpha,digits=4,nsmall=2))))
#   
#   f1$model$method = "SES"
#   plot(f1$model)
#   plot(f1$model,plot.type="single",col=1:2,ylab="No. strikes in US")
#   legend("topleft",lty=1,col=1:2,legend=c("data","level"))
#   lines(f1$fitted,col='green')

# Annual Air Transport Passengers Australia
  # This data has a trend
  # You still will get a set of forecasts
  # but they are flat

  plot(ausair)
  plot(ses(ausair))
  ses(ausair)$model

# Holt's, exp and additive damped

# Holt's additive method
  air <- window(ausair, start = 1990, end = 2004)
  plot(air)
  
  fit1 <- holt(air, h = 5)
  # Note the trending forecasts now
  plot(fit1)
  points(air)

  # See the output
  # The forecasts are given
  fit1
  names(fit1)
  fit1$model
  # beta=0 so trend not changing
  # alpha=1 level is equal to last observation 
  # i.e. Randnom walk with drift - drift method
  
  # Write this out to show it 
  
  plot(fit1$model)
  # A bit like a decomposition plots - no remainder BUT 
  # you have a slope here which is the first derivative 
  # of the trend. 
  # Notice the scale to see it is not changing much. 
  # It's changing in the 4th decimal place. 

  # For demonstration set alpha=0.8 beta=0.2
  fit1a <- holt(air, h = 5,alpha=0.8,beta=0.2)
  plot(fit1a)
  points(air)
  lines(fit1a$fitted,col='blue')
  points(fit1a$fitted,col='blue')
  # Comment: Not the same projection as a regression 
  # model with trend, this is locally linear not 
  # globally. The trend estimates allow for nonlinearities 
  # but the projection is linear 
  
  # Here are the linear trend forecasts
  lines(forecast(tslm(air~trend),h=5)$fitted,col='red')
  lines(forecast(tslm(air~trend),h=5)$mean,col='red')
  
  # Changing the initial estimated so we can get
  # the same as in Table 7.3
  fit1b <- holt(air, h = 5,alpha=0.8,beta=0.2,initial="simple")
  plot(fit1b$model)  
  fit1b$model$states
  
  # Plot this again
  # For demonstration set alpha=0.8 beta=0.2
  fit1a <- holt(air, h = 5,alpha=0.8,beta=0.2)
  plot(fit1a)
  points(air)
  lines(fit1a$fitted,col='blue')
  points(fit1a$fitted,col='blue')

  # If I make beta higher so slope reacts much more
  # see what happens
  fit1c<-holt(air, h = 5,alpha=0.8,beta=0.8)
  lines(fit1c$fitted,col='green')
  points(fit1c$fitted,col='green')
  lines(fit1c$mean,col='green')

  # To show where your forecasts start from
  fit1a$model$states
  #abline(h=fit1a$model$states[nrow(fit1a$model$states),1]+
  #         fit1a$model$states[nrow(fit1a$model$states),2],col="blue")
  abline(h=41.93+1.83,col="blue")

  fit1c$model$states
  #abline(h=fit1c$model$states[nrow(fit1a$model$states),1]+
  #         fit1c$model$states[nrow(fit1a$model$states),2],col="green")
  abline(h=42.31+0.65,col="green")


# Holt's additive method
# Exampe above simply repeated to compare with below
  air <- window(ausair, start = 1990, end = 2004)
  fit1 <- holt(air, h = 5)
  plot(fit1)
  fit1$model

  # beta=0 so trend not changing
  # alpha=1 level is equal to last observation
  plot(fit1$model)
  # i.e. Randnom walk with drift - drift method
  
  # Write this out to show it 

# Holt's with exponential trend
  fit2 <- holt(air, exponential = TRUE, h = 5)
  fit2$model
  # Notice, alpha, beta and l0 much the same
  # b0 now in a growth rate - multiplicative
  plot(fit2)
  # plot a bit more so you can see the exponential trend
  plot(holt(air, exponential = TRUE, h = 55))

#Damped trend
  fit3 <- holt(air, damped = TRUE,  h = 5)
  # An extra parameter now is estimated - phi
  fit3$model
    # with every h only 94% of the trend is carried over
  plot(fit3)
  plot(holt(air, damped = TRUE,  h = 55))

  # Reproducing the figure in the textbook
  # So smoothing parameters are set to 
  #     be the same - not estimated 
  # Notice the insample forecasts - damped below Holt's
  #     below exponential
  air <- window(ausair,start=1990,end=2004)
  fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5) 
  fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", 
               exponential=TRUE, h=5) 
  fit3 <- holt(air, alpha=0.8, beta=0.2, initial="simple", 
               damped=TRUE, h=5) 
  plot(fit2, type="o", ylab="Air passengers in Australia (millions)", xlab="Year", 
       fcol="white", plot.conf=FALSE)
  lines(fitted(fit1), col="blue") 
  lines(fitted(fit2), col="red")
  lines(fitted(fit3), col="green")
  lines(fit1$mean, col="blue", type="o") 
  lines(fit2$mean, col="red", type="o")
  lines(fit3$mean, col="green", type="o")
  legend("topleft", lty=1, col=c("black","blue","red","green"), 
         c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))


      # # Experimenting with multiplicative damped trend
      #   p=0.98
      #   
      #   b=15 #slope which I convert to growth rate below
      #   bm=1+(b/100)
      #   
      #   h=2000
      #   yhat=matrix(0,h,3)
      #   for(i in 1:h)
      #   {
      #     sump=0
      #     for(j in 1:i)
      #       sump=sump+p^j
      #     
      #     yhat[i,1]=b*sump
      #     yhat[i,2]=bm^sump
      #     yhat[i,3]=b*i
      #   }
      #   
      #   plot.ts(yhat[,1],ylim=range(yhat,max((yhat[,2]-1)*100))) #damped
      #   lines((yhat[,2]-1)*100,col="red") #exp
      #   lines(yhat[,3], col="green") #linear
      #   
      #   #Multiplicative asymptotes here  
      #   plot.ts(yhat[,2])
      #   abline(h=bm^(p/(1-p)),col="red")
  
# Example: Livestock - Annual no. sheep in Asia
  # Showing longer projections to see the differences
  livestock2 <- window(livestock, start = 1970, end = 2000)
  plot(livestock2,ylab="No. livestock in US",xlab="Year")
    
  fit1 <- ses(livestock2,h=50)
  fit2 <- holt(livestock2,h=50)
  fit3 <- holt(livestock2,exponential=TRUE,h=50)
  fit4 <- holt(livestock2,damped=TRUE,h=50)
  fit5 <- holt(livestock2,exponential=TRUE,damped=TRUE,h=50)
  
  plot(fit3, type="o", ylab="Livestock, sheep in Asia (millions)", 
       flwd=1, plot.conf=FALSE)
  lines(window(livestock,start=2001),type="o")
  lines(fit1$mean,col=2)
  lines(fit2$mean,col=3)
  lines(fit4$mean,col=5)
  lines(fit5$mean,col=6)
  legend("topleft", lty=1, pch=1, col=1:6,
         c("Data","SES","Holt's","Exponential",
           "Additive Damped","Multiplicative Damped"))

# Looking at the components
  
  plot(fit2$model)
  # Notice the scale of b - so not changing  
  plot(fit2$model$state, main="Holt's linear trend")
  
  plot(fit4$model)
  plot(fit4$model$state, main="Additive damped trend")

  plot(fit3$model)
  plot(fit3$model$state, main="Exponential trend")

  plot(fit5$model)
  plot(fit5$model$state, main="Multi damped trend")

  # Notice the differences in the scale of the slope
  # between additive and multiplicative methods

  "Holt's linear trend"
  fit2$model$state[,1]
  fit2$model$state[,2]

  "Additive damped trend"
  fit4$model$state[,1]
  fit4$model$state[,2]

  "Exponential trend"
  fit3$model$state[,1]
  fit3$model$state[,2]
  
  "Multi damped trend"
  fit5$model$state[,1]
  fit5$model$state[,2]



  # Haven't used this
  #   alpha <- seq(0.01,0.99,by=0.02)
  #   mse <- numeric(length(alpha))
  #   
  #   for(i in 1:length(alpha))
  #   {
  #     fred <- ses(livestock2,alpha=alpha[i])
  #     mse[i] <- mean(fred$residuals^2)
  #     plot(livestock2,ylab="No. livestock2 in US",xlab="Year",xlim=c(1970,2010))
  #     lines(fitted(fred),col=2)
  #     lines(fred$mean,col=4)
  #     text(1972,400,bquote(alpha== .(format(alpha[i],digits=3,nsmall=2))))
  #     text(1972,380,paste("MSE=",round(mse[i])))
  #   }

    # # SES
    # fit1 <- ses(livestock2)
    # fit1$model
    # 
    # plot(alpha,mse/1e3,type="l",ylab="MSE ('000)")
    # bestalpha <- fit1$model$par[1]
    # lines(rep(bestalpha,2),c(0.0,1e6),col=2)
    # text(bestalpha+0.1,600,bquote(alpha==.(format(bestalpha,digits=4,nsmall=2))))
    # 
    # fit1$model$method = "SES"
    # plot(fit1$model)
    # plot(fit1$model,plot.type="single",col=1:2,ylab="No. livestock2 in US")
    # legend("topleft",lty=1,col=1:2,legend=c("data","level"))
    # 
    # # HOLT
    # fit2 <- holt(livestock2)
    # fit2$model
    # 
    # plot(fit2,plot.conf=FALSE,ylab="No. livestock2 in US",xlab="Year")
    # lines(fitted(fit2),col=2)
    # text(1972,450,bquote(alpha==.(format(fit2$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1972,430,bquote(beta==.(format(fit2$model$par[2],digits=4,nsmall=2))),adj=0)
    # 
    # fit2$model$method <- "Holt's"
    # plot(fit2$model)
    # 
    # # Exponential HOLT
    # fit3 <- holt(livestock2, exponential=TRUE)
    # 
    # plot(fit3,plot.conf=FALSE,ylab="No. livestock2 in US",xlab="Year")
    # lines(fitted(fit3),col=2)
    # text(1970,450,bquote(alpha==.(format(fit3$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1970,430,bquote(beta==.(format(fit3$model$par[2],digits=4,nsmall=2))),adj=0)
    # 
    # fit3$model$method <- "Exponential trend"
    # plot(fit3$model)
    # 
    # # Damped trend
    # fit4 <- holt(livestock2, damped=TRUE)
    # 
    # plot(fit4,plot.conf=FALSE,ylab="No. livestock2 in US",xlab="Year")
    # lines(fitted(fit4),col=2)
    # text(1972,430,bquote(alpha==.(format(fit4$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1972,410,bquote(beta==.(format(fit4$model$par[2],digits=4,nsmall=2))),adj=0)
    # text(1972,390,bquote(phi==.(format(fit4$model$par[3],digits=4,nsmall=2))),adj=0)
    # 
    # fit4$model$method <- "Damped trend"
    # plot(fit4$model)
    # 
    # # Damped Exponential trend
    # fit5 <- holt(livestock2, exponential=TRUE, damped=TRUE)
    # 
    # plot(fit5,plot.conf=FALSE,ylab="No. livestock2 in US",xlab="Year")
    # lines(fitted(fit5),col=2)
    # text(1972,450,bquote(alpha==.(format(fit5$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1972,430,bquote(beta==.(format(fit5$model$par[2],digits=4,nsmall=2))),adj=0)
    # text(1972,410,bquote(phi==.(format(fit5$model$par[3],digits=4,nsmall=2))),adj=0)
    # 
    # fit5$model$method <- "Damped Exponential trend"
    # plot(fit5$model)
    # 
    # livestock3 <- window(livestock, start=2001, end=2005)
    # accuracy(fit1, livestock3)
    # accuracy(fit2, livestock3)
    # accuracy(fit3, livestock3)
    # accuracy(fit4, livestock3)
    # accuracy(fit5, livestock3)
    # 
    # plot(fit2$model)
    # plot(fit4$model)
    # 
    # # Figure 7.5
    # plot(fit3, type="o", ylab="Livestock, sheep in Asia (millions)", 
    #      flwd=1, plot.conf=FALSE)
    # lines(window(livestock,start=2001),type="o")
    # lines(fit1$mean,col=2)
    # lines(fit2$mean,col=3)
    # lines(fit4$mean,col=5)
    # lines(fit5$mean,col=6)
    # legend("topleft", lty=1, pch=1, col=1:6,
    #        c("Data","SES","Holt's","Exponential","Additive Damped","Multiplicative Damped"))
    # 
    # 



     # # HOLT
    # f2 <- holt(strikes)
    # f2$model
    # 
    # plot(f2,plot.conf=FALSE,ylab="No. strikes in US",xlab="Year")
    # lines(fitted(f2),col=2)
    # text(1950,1000,bquote(alpha==.(format(f2$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1950,500,bquote(beta==.(format(f2$model$par[2],digits=4,nsmall=2))),adj=0)
    # 
    # f2$model$method <- "Holt's"
    # plot(f2$model)
    # 
    # # DAMPED HOLT
    # f3 <- holt(strikes, damped=TRUE)
    # 
    # plot(f3,plot.conf=FALSE,ylab="No. strikes in US",xlab="Year")
    # lines(fitted(f3),col=2)
    # text(1950,2900,bquote(alpha==.(format(f3$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1950,2600,bquote(beta==.(format(f3$model$par[2],digits=4,nsmall=2))),adj=0)
    # 
    # f3$model$method <- "Damped Holt's"
    # plot(f3$model)
    # 
    # # Exponential trend
    # f4 <- holt(strikes, exponential=TRUE)
    # 
    # plot(f4,plot.conf=FALSE,ylab="No. strikes in US",xlab="Year")
    # lines(fitted(f4),col=2)
    # text(1950,2900,bquote(alpha==.(format(f4$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1950,2600,bquote(beta==.(format(f4$model$par[2],digits=4,nsmall=2))),adj=0)
    # 
    # f4$model$method <- "Exponential trend"
    # plot(f4$model)
    # 
    # # Damped Exponential trend
    # f5 <- holt(strikes, exponential=TRUE, damped=TRUE)
    # 
    # plot(f5,plot.conf=FALSE,ylab="No. strikes in US",xlab="Year")
    # lines(fitted(f5),col=2)
    # text(1985,4900,bquote(alpha==.(format(f5$model$par[1],digits=4,nsmall=2))),adj=0)
    # text(1985,4600,bquote(beta==.(format(f5$model$par[2],digits=4,nsmall=2))),adj=0)
    # 
    # f5$model$method <- "Damped Exponential trend"
    # plot(f5$model)
    # 
    # 
    # accuracy(f1)
    # accuracy(f2)
    # accuracy(f3)
    # accuracy(f4)
    # accuracy(f5)



# Holt Winters with a seasonal component
  # austourists <-Quarterly visitor nights spent 
  # by international tourists to Australia. 1999-2010.
  
aust <- window(austourists,start=2005)
  plot(aust)

  fit1 <- hw(aust,seasonal="additive")
  plot(fit1,ylim=c(20,70))
  # It gerenated good forecasts
  # Remember all components are changing and adjusting
  # gamma determines how quickly seasonality changes
  # a little like regression with dummies with time 
  # varying parameters - i.e., not fixed parameters
  # Remember the electricity example where seasonal 
  # pattern changed because of the prominent use of 
  # airconditioners. This method will cope.

  fit2 <- hw(aust,seasonal="multiplicative")
  plot(fit2,ylim=c(20,70))
  # Quite a noticeable  difference between the two
  # However dataset too small to do proper 
  # test-sample comparisons - at best maybe a year or so
  # We will learn about how to decide this soon.

  fit1$model
  # gamma is zero - the other two are fairly low
  # so components not changing much
  plot(fit1$model)
  
  fit1a <- hw(aust,seasonal="additive", alpha=0.001, 
              beta=0.0001, gamma=0.0001)
  plot(fit1a$model)
  # A bit like a decomposition plots BUT the slope here 
  # is the first derivative of the trend. Notice the scale 
  # to see it is not changing much. It's changing in the 4th 
  # decimal place. In this case it is like we have set the 
  # parameters to be like a regression with dummies.

  # Here is a model specification with all three 
  # components changing lots
  fit1a <- hw(aust,seasonal="additive", alpha=0.6,
              beta=0.4, gamma=0.4)
  plot(fit1a$model)

 
# Example 7.4 Holt Winters
  plot(fit2,ylab="International visitor night in Australia 
       (millions)",
     plot.conf=FALSE,type="o",fcol="white",xlab="Year")
  lines(fitted(fit1),col="red",lty=2)
  lines(fitted(fit2),col="green",lty=2)
  lines(fit1$mean,type="o",col="red")
  lines(fit2$mean,type="o",col="green")
  legend("topleft",lty=1,pch=1,col=c(1,2,3),
         c("data","Holt Winters' Additive",
           "Holt Winters' Multiplicative"))

  fit3 <- hw(aust, seasonal="multiplicative", damped=TRUE, h=50)
  plot(fit3)


#
# Some examples in practice
#

#
# Example 1
#
  # Say I look at the oil data and decide to use ses
  fit <- ses(oil)
  plot(fit)
  # how do I decide if this is good?
  # Look if residuals are WN 
  plot(residuals(fit))
  abline(h=0)
  Acf(residuals(fit))
  # Looks ok 
  # Do a Ljung Box test which works better on small samples
  fit$model
  # See the nuimber of parameters is 2 for dof
  Box.test(residuals(fit),fitdf=2,type="Ljung-Box",lag=10)
  # pvalue is not significant but just 
  # Cannot reject H0:WN
  summary(fit) # you don't know about many of these yet 

  # I could try a Holt's methods
  fit2<-holt(oil)
  plot(fit2)
  # the adaptive trend picks up a bit of a positive 
  # Remember this is allowed to adapt throughout the sample
  fit2$model
  # alpha close to 1 and beta close to 0 
  # so trend is not changing much 
  # The forecast slope shows that it responds a little too 
  # much to the first bit if the sample
  plot(residuals(fit2))
  abline(h=0)
  Acf(residuals(fit2))
  Box.test(residuals(fit2),fitdf=4,type="Ljung-Box",lag=10)
  summary(fit2)
  # Compare the RMSE 
  # fit RMSE is 48.7273 using 2 parameters
  # fit2 RMSE is 49.267 it's increased although we are using 4 parameters
  # So really we are giving all advantage to Holt's but it does 
  # not do any better
  # Colnclusion probably use SES - it's simpler 

  # The more objective way of choosing will be provided
  # when we introduce statistical models for these


#
# Example 2: ausair
# Total annual air passengers including domestic and 
# international aircraft passengers of air carriers 
# registered in Australia. 1970-2009
#
  plot(ausair)
  
  # Strong trend let's use an adaptive trend -- use Holt's
  fit <- holt(ausair)
  plot(fit)
  fit$model
  # Look at the residuals
  plot(residuals(fit))
  abline(h=0)
  Acf(residuals(fit))
  Box.test(residuals(fit),fitdf=4,type="Ljung-Box",lag=10)
  # Easily passes test 

  summary(fit)
  # Again large alpha i.e., forecasts start very close to the 
  # last observation but low beta again so trend more of a 
  # global trend not adapting 

  # Let's try an exponential trend
  fit2 <- holt(ausair, exponential=TRUE)
  plot(fit2)
  # So forecasts now curved up abit
  # Look at the residuals
  plot(residuals(fit2))
  abline(h=0)
  fit2$model
  Acf(residuals(fit2))
  Box.test(residuals(fit2),fitdf=4,type="Ljung-Box",lag=10)
  # Oops we have now almost failed the 
  # test which is worrying
  summary(fit2)
  
  # Now fit and fit2 error measures are direclty comparable 
  # because both models use the same number of parameters
  # All insample measures show second method does better.

#
# Example 3:
# A seasonal example - data set came from my work with TA
# Quarterly visitor nights spent by international tourists 
# to Australia. 1999-2010.
#

  plot(austourists)
  # A bit of a hint for a multiplicative method 
  # remember for multiplicative method seasonality 
  # multiplies on top of trend
  fit <- hw(austourists,seas="multi")
  summary(fit)
  # a few parameters get estimated now
  # alpha how quickly the level changes
  # beta how quickly the trend chenges
  # gamma how quickly the seasonality changes -  
  # we expect gamma to be small - seasonality should not 
  # change much, by definition seasonality stays 
  # the same over time
  
  # Useful to plot the model i.e. the states evolving 
  # over time as we said last time a bit like a 
  # decomposition
  plot(fit$model)
  # see the slope and seasonal not changing much
  # just the level changing
  plot(fit)
  # Generally do an eyeball check
  # point forecast may look a little lower 
  # than what I expected them maybe prediction 
  # intervals look ok
  
  # May be we can try an exponential trend
  fit2 <- hw(austourists,seas="multi",exponential=TRUE)
  plot(fit2)
  # Made the slightest of differences
  # Let's look at their fit - RMSE/MAE is fine the same 
  # number of coeff
  summary(fit2)
  # Very similar - probably first one additive better
  
  # How about Ljung-Box tests
  Box.test(residuals(fit),lag=12, fitdf=8, type="Ljung-Box")
  # Note the number of parameters
  # 3 smoothing coefficients
  # l0, b0, s0,s1,s2 - no need to count s3 because they 
  # are constrained to add up to m 
  # if additive add up to 0 - just like seas dummies
  # lag=8 is two years worth of data - but dof=lag-fitdf>0, 
  # hence use lag=12
  Box.test(residuals(fit2),lag=12,fitdf=8, type="Ljung-Box")
  # Hmm. This one just passes the test. 
  # So maybe the first fits better. 
  # Maybe choose the one with the smallest RMSE
  # Be careful with projecting the exponential trend - so 
  # maybe choose the first one in this case

#
# Example 3
# Monthly cortecosteroid drug sales in Australia 
# from 1992 to 2008.
#   

  # Total monthly scripts for pharmaceutical products falling 
  # under ATC code H02, as recorded by the Australian Health 
  # Insurance Commission. Measured in millions of scripts.
  
  plot(h02)
  # Seasonality increasing in size
  # trend seems to flatten out - so maybe damped at this stage 
  # start simple
  fit <- hw(h02,season="multi")
  plot(fit)
  
  summary(fit)
  # See the estimated parameters - 17 of them
  
  tsdisplay(residuals(fit))
  # Big spikes in the ACF - we have not covered the PACF yet
  # Almost surely will fail the Ljung-Box test
  Box.test(residuals(fit,fitdf=16),lag=24, type="L")
  # Confirmed a bad fail - reject Null:WN
  
  # So what do we do? It's the seasonal compenent that is not
  # working for us. I would think additive seas would be 
  # even worse
  fit2 <- hw(h02,season="additive")
  plot(fit2)
  # Point forecasts now definitely look dodgy
  tsdisplay(residuals(fit2))
  # Even more spikes in the ACF
  Box.test(residuals(fit2,fitdf=16),lag=24, type="L")
  # Again failed the Ljung-Box test
  # So this is even worse
  
  # Back to the first model which was better
  tsdisplay(residuals(fit))
  # 
  # Comments: 
  #   1. Longish series so likely to get significant 
  #      autocorrelations although they are not that 
  #      large
  #   2. At the moment you don't have the tools we will 
  #      develop them
  #   3. There are many times that you will not be able 
  #      to capture all dynamics. Models are approximations
  #      and you have to live with that. F0recasts are not 
  #      aweful they are Ok.


#
# ETS examples
#
  plot(oil)
  ets(oil,model="AAN",damped=FALSE)
  # See the output returns estimates of 
  # alpha, beta, l0, b0 and sigma - st dev of the errors

  # before we did
  holt(oil)$model
  # This actually used the ETS function to do the fitting
  
  # If you do not specify damped=FALSE it will choose 
  # whether damping is required. The choice is based on
  # whether the AIC is improved
  ets(oil,model="AAN")

  # In fact if you leave the model argument empty 
  # see what happens
  fit<-ets(oil)
  fit
  # Fits all 30 models and returns the one with the
  # minimum AIC
  plot(forecast(fit))
  # Prediction intervals - we'll talk about these 
  # in detail 

  # A useful function - returns in-sample error measures
  summary(fit)

#
# Another example
#
 
  # Note: I am not repeating the residuals tests that we 
  # did above. But you would look at the residuals 
  # as you did above for each step of the way

  plot(ausair)
  # Strong trend so we may for a model for Holt's method
  fit1 <- ets(ausair,model="AAN")
  summary(fit1)
  
  # Let R choose 
  fit2 <- ets(ausair)
  summary(fit2)
  # R chooses multiplicative errors and trend
  # Notice the AIC for the second one is much lower
  plot(forecast(fit2))
  lines(forecast(fit1)$mean,col="red",lwd=2)
  # I expect prediction intervals from additive 
  # to be narrower. Let's have a look
  lines(ts(forecast(fit1)$upper[,2],start=2010),col="red",lwd=1)
  lines(ts(forecast(fit1)$lower[,2],start=2010),col="red",lwd=1)

# A third example
  plot(austourists)
  # Seasonal and trended - but in what combination is hard to tell
  # Here is a guess ETS(M,A,M)
  # Multi seasonality because it increases with level
  # Trend looks linear so additive trend
  # Multi errors fit better with Multi seasonality
  fit1 <- ets(austourists,model="MAM")
  summary(fit1)
  # Let R choose
  fit2 <- ets(austourists)
  summary(fit2)
  # Pretty good choice in this case

# Last example
  plot(h02)
  fit<-ets(h02) # Takes a little longer this time
  summary(fit)
  Acf(residuals(fit))
  Box.test(residuals(fit,fitdf=17),lag=24, type="L")
  plot(forecast(fit)) 
    # So can handle a large range of data
  # in a completely automatic way.
  # Many companies use the ets package 
  # in this automatic way - Nestle, Lego, etc.







