


#------------------------------------------------------------
# http://www.bbc.com/news/uk-politics-38762034
# http://www.stat.illinois.edu/courses/stat100/Notes/Chap9.pdf
# http://blog.csdn.net/textboy/article/details/47277131
# http://plantecology.syr.edu/fridley/bio793/gam.html 
#------------------------------------------------------------

library(ggplot2)
library(boot)
library(leaps)
library(caret)
library(mgcv)

x <- read.csv('ReferendumResults.csv',header = T)
# create a variable which is the proportion of 'Leave' votes
x$Leava_Prob <- ifelse(x$Leave!=-1,x$Leave/x$NVotes,NA)

# demographic characteristics
# age
# x$Underage <- x$Age_0to4 + x$Age_5to7+x$Age_8to9+x$Age_10to14+x$Age_15+x$Age_16to17
x$Young_age <- x$Age_18to19+x$Age_20to24+x$Age_25to29
x$Working_age <- x$Age_30to44+x$Age_45to59+x$Age_60to64
x$Retirement_age <- x$Age_65to74+x$Age_75to84+x$Age_85to89+x$Age_90plus
#ethnicity 
x$Yellow <- x$Asian

#household and economic
x$Owned_Yes <- x$Owned
x$Owned_No <- x$SocialRent + x$PrivateRent
x$Unemp_All <- x$Unemp + x$UnempRate_EA
x$Deprived_All <- x$Deprived + x$MultiDepriv

#social grades
x$C2 <- x$C2DE - x$DE
x$C1 <- x$C1C2DE - x$C2DE


# mydata

mydata <- x[,c('NVotes','Leave','Leava_Prob','AreaType','RegionName','Residents',
	            'Households','MeanAge','AdultMeanAge','Black',
	            'White','Yellow','Owned_Yes','Owned_No',
	            'NoQuals','L1Quals','L4Quals_plus','Students',
	            'Unemp_All','HigherOccup','RoutineOccupOrLTU',
	            'Density','Deprived_All','C2','C1','DE',
	            'Young_age','Working_age','Retirement_age'
	            )]

#Determine highly correlated variables
highcor_name <- names(mydata)[-c(1:5)][findCorrelation(cor(mydata[,-(1:5)]))]
mydata <- mydata[,-c(findCorrelation(cor(mydata[,-(1:5)]))+5)]




# Subset Selection in Regression
r1 <- regsubsets(Leava_Prob~.,data=na.omit(mydata[,-c(1,2,4,5)]),nvmax = 26)
r1.summ <- summary(r1)
par(mfrow=c(1,3))
plot(r1.summ$cp,type = 'b',ylab='Mallows\' Cp',
     main=paste0('Best Subset Selection with ', 'Mallows\' Cp ' ),xlab='Index')
abline(h=min(r1.summ$cp),col='red',lty=2)
plot(r1.summ$bic,type = 'b',ylab='BIC',
     main=paste0('Best Subset Selection with ', 'BIC'),xlab='Index')
abline(h=min(r1.summ$bic),col='red',lty=2)
plot(r1.summ$adjr2,type = 'b',ylab='adj R^2',
     main=paste0('Best Subset Selection with ','adj R^2 ' ),xlab='Index')
abline(h=max(r1.summ$adjr2),col='red',lty=2)
par(mfrow=c(1,1))

choose_p <- c(names(mydata[,1:5]),names(coef(r1,id = 13))[-1])
mydata <- subset(mydata,select =choose_p )



#------

#split data 

set.seed(200)
train_data <- mydata[x$Leave!=-1,]
pred_data <- mydata[x$Leave==-1,]
ind <- sample(x = 2,size = nrow(train_data),replace = T,prob = c(0.75,0.25))
t1_data <- train_data[ind==1,]
t2_data <- train_data[ind==2,]

#------

# stepwise regression with backward algorithm.
lm1 <- lm(formula = Leava_Prob ~ ., data = train_data[,-c(1:2)])

step(lm1,direction='backward')

lm2 <- lm(formula = Leava_Prob ~ AreaType + RegionName + White + Yellow + 
    L1Quals + L4Quals_plus + Students + Unemp_All + Density + 
    C2 + C1 + Young_age + Retirement_age, data = train_data[, 
    -c(1:2)])

lm3 <- lm(formula = Leava_Prob ~ AreaType + RegionName + White + Yellow + 
    L1Quals + L4Quals_plus + Students  + Density + 
    C2  + Young_age + Retirement_age, data = train_data[, 
    -c(1:2)])

anova(lm2,lm3)


#--------
par(mfrow=c(1,3),las=2)
hist(train_data$Leava_Prob,main='Leava_Prob',xlab='')
boxplot(Leava_Prob~AreaType,data=train_data,main='Leava_Prob Vs AreaType')
boxplot(Leava_Prob~RegionName,data=train_data,main='Leava_Prob Vs RegionName')
par(mfrow=c(1,1),las=1)

#------

#############################################
#                 GLM                       #
#############################################

glm1 <- glm(formula = Leava_Prob ~ AreaType + RegionName + White + Yellow + 
              L1Quals + L4Quals_plus + Students + Density + C2 + Young_age + 
              Retirement_age, data = t1_data,family=gaussian)

glm2 <- glm(formula = cbind(Leave,NVotes-Leave) ~ AreaType +RegionName + White + Yellow + 
              L1Quals + L4Quals_plus + Students + Density + C2 + Young_age + 
              Retirement_age, data = t1_data,family=binomial)

glm3 <- glm(formula = cbind(Leave,NVotes-Leave)~ AreaType+RegionName + White + Yellow + 
              L1Quals + L4Quals_plus + Students + Density + C2 + Young_age + 
              Retirement_age, data = t1_data,family=quasibinomial(link='logit'))

glm4 <- glm(formula = cbind(Leave,NVotes-Leave) ~AreaType+ RegionName + White + Yellow + 
              L1Quals + L4Quals_plus + Students + Density + C2 + Young_age + 
              Retirement_age+Retirement_age:RegionName, data = t1_data,family=quasibinomial(link='logit'))

#------
#############################################
#                GAM                        #
#############################################

gam1 <- gam(formula = Leava_Prob ~ AreaType + RegionName + s(White,k=3) + s(Yellow,k=5) + 
              s(L1Quals,k=3) + s(L4Quals_plus,k=3) + s(Students) + s(Density,k=3) + C2 + s(Young_age,k=5) + 
              s(Retirement_age,k=5,by=RegionName), data = t1_data,family=gaussian)

gam2 <- gam(formula = cbind(Leave,NVotes-Leave) ~ AreaType + RegionName + s(White) + s(Yellow) + 
              s(L1Quals) + s(L4Quals_plus) + s(Students) + s(Density) + s(C2) + s(Young_age) + 
              s(Retirement_age), data = t1_data,family=binomial)

gam3 <- gam(formula = cbind(Leave,NVotes-Leave) ~ AreaType + RegionName + s(White) + s(Yellow) + 
              s(L1Quals) + s(L4Quals_plus) + s(Students) + s(Density) + s(C2) + s(Young_age) + 
              s(Retirement_age), data = t1_data,family=quasibinomial(link='logit'))

gam4 <- gam(formula = cbind(Leave,NVotes-Leave) ~ AreaType + RegionName + s(White) + s(Yellow) + 
              s(L1Quals) + s(L4Quals_plus) + s(Students) + s(Density) + s(C2) + s(Young_age,by=RegionName) + 
              s(Retirement_age), data = t1_data,family=quasibinomial(link='logit'))



comp <- function(obj,data,type='lm'){
	pred <- predict(obj,newdata = data,se.fit = T,type = 'response')
	real <- data$Leava_Prob
	rmse <- sqrt(round(mean( (pred$fit-real)^2),5))
	rss <- round(sum( (pred$fit-real)^2),5)
	tss <- sum((real-mean(real))^2)
	r2 <- 1-rss/tss
    if(type=='lm'){
    	sigma <- sqrt(pred$se.fit^2 + pred$residual.scale^2)
    	} else { 
        dispersion <- summary(obj)$dispersion
        p <- pred$fit
    		sigma <- sqrt( (pred$se.fit^2 + dispersion* p* (1-p)/data$NVotes) )
    	}

    s <- sum( log(sigma) + (real-pred$fit)^2 / (2*sigma^2) )
    return(data.frame(rmse=rmse,rss=rss,r2=r2,s=s))
}


#-------

comp(obj = glm1,data = t1_data,type = 'glm')
comp(obj = glm2,data = t1_data,type = 'glm')
comp(obj = glm3,data = t1_data,type = 'glm')
comp(obj = glm4,data = t1_data,type = 'glm')

comp(obj = gam1,data = t1_data,type = 'gam')
comp(obj = gam2,data = t1_data,type = 'gam')
comp(obj = gam3,data = t1_data,type = 'gam')
comp(obj = gam4,data = t1_data,type = 'gam')

#-------

comp(obj = glm1,data = t2_data,type = 'glm')
comp(obj = glm2,data = t2_data,type = 'glm')
comp(obj = glm3,data = t2_data,type = 'glm')
comp(obj = glm4,data = t2_data,type = 'glm')

comp(obj = gam1,data = t2_data,type = 'gam')
comp(obj = gam2,data = t2_data,type = 'gam')
comp(obj = gam3,data = t2_data,type = 'gam')
comp(obj = gam4,data = t2_data,type = 'gam')
#-------


#ggplot(data=train_data,aes(x=Young_age,y=Leava_Prob,col=RegionName))+geom_point()+
#    geom_smooth(se=F,method = 'gam',formula = y ~ s(x))
#ggplot(data=train_data,aes(x=Retirement_age,y=Leava_Prob))+geom_point()+
#    geom_smooth(se=F,method = 'gam',formula = y ~ s(x))


best_model <- gam(formula = cbind(Leave,NVotes-Leave) ~ AreaType + RegionName + s(White) + s(Yellow) + 
              s(L1Quals) + s(L4Quals_plus) + s(Students) + s(Density) + s(C2) + s(Young_age,by=RegionName) + 
              s(Retirement_age), data = train_data,family=quasibinomial(link='logit'))

plot(best_model,pages = 2,rug = T,shade = T)

# write data
dispersion <- summary(best_model)$dispersion
pred <- predict.bam(object = best_model,newdata = pred_data,se.fit = T,type = 'response')
pred_porb <- pred$fit
pred_sigma <- sqrt( (pred$se.fit^2 + dispersion* pred_porb* (1-pred_porb)/pred_data$NVotes))
pred_result <- data.frame(id=row.names(pred_data),leave_prob=round(pred_porb,4),se=round(pred_sigma,4))




# write data
write.table(x = pred_result,
  file = '13020522_pred.dat',
  row.names = F,
  col.names = F,
  sep = ' ',
  quote = F)


#=========
#END

