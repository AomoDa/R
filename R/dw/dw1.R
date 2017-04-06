

#http://www.stat.illinois.edu/courses/stat100/Notes/Chap9.pdf
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
x$Yellow <- x$Asian + x$Indian + x$Pakistani

#household and economic
x$Owned_Yes <- x$Owned + x$OwnedOutright
x$Owned_No <- x$SocialRent + x$PrivateRent
x$Unemp_All <- x$Unemp + x$UnempRate_EA
x$Deprived_All <- x$Deprived + x$MultiDepriv

#social grades
x$C2 <- x$C2DE - x$DE
x$C1 <- x$C1C2DE - x$C2DE


# mydata

mydata <- x[,c(   'NVotes','Leave','Leava_Prob','AreaType','RegionName','Residents',
	            'Households','MeanAge','AdultMeanAge','White',
	            'Black','Yellow','Owned_Yes','Owned_No',
	            'NoQuals','L1Quals','L4Quals_plus','Students',
	            'Unemp_All','HigherOccup','RoutineOccupOrLTU',
	            'Density','Deprived_All','C2','C1','DE',
	            'Young_age','Working_age','Retirement_age'
	            )]

#Determine highly correlated variables
highcor_name <- names(mydata)[-c(1:5)][findCorrelation(cor(mydata[,-(1:5)]))]
mydata <- mydata[,-c(findCorrelation(cor(mydata[,-(1:5)]))+5)]



# Subset Selection in Regression
r1 <- regsubsets(Leava_Prob~.,data=mydata[,-c(1,2,4,5)],nvmax = 26)
r1.summ <- summary(r1)
par(mfrow=c(1,3))
plot(r1.summ$cp,type = 'b',ylab='Mallows\' Cp',main=paste0('Best Model P is ', which.min(r1.summ$cp) ),xlab='Number of Predictor')
abline(h=min(r1.summ$cp),col='red',lty=2)
plot(r1.summ$bic,type = 'b',ylab='BIC',main=paste0('Best Model P is', which.min(r1.summ$bic)),xlab='Number of Predictor')
abline(h=min(r1.summ$bic),col='red',lty=2)
plot(r1.summ$adjr2,type = 'b',ylab='adj R^2',main=paste0('Best Model P is', which.max(r1.summ$adjr2)),xlab='Number of Predictor')
abline(h=max(r1.summ$adjr2),col='red',lty=2)
par(mfrow=c(1,1))

choose_p <- c(names(mydata[,1:5]),names(coef(r1,id = 13))[-1])
mydata <- subset(mydata,select =choose_p )


#split data 

set.seed(200)
train_data <- mydata[x$Leave!=-1,]
pred_data <- mydata[x$Leave==-1,]
ind <- sample(x = 2,size = nrow(train_data),replace = T,prob = c(0.9,0.1))
t1_data <- train_data[ind==1,]
t2_data <- train_data[ind==2,]


#--------------------------------------------------------------
#PCA
#library(psych)
#Parallel analysis suggests that  the number of components =  5
#fa.parallel(mydata[,-c(1:3)],fa='pc')
#pc1 <- principal(mydata[,-c(1:3)],nfactors = 5,rotate = 'varimax',scores = T)
#dd <- cbind(mydata[,1:3],as.data.frame(pc1$scores))

# train dataset and predict dataset
#model_data <- dd[x$Leave!=-1,]
#pred_data <- dd[x$Leave==-1,]

#
#set.seed(1)
#ind <- sample(x = 2,size = nrow(model_data),replace = T,prob = c(0.9,0.1))
#train_data <- model_data[ind==1,]
#test_data <- model_data[ind==2,]
#--------------------------------------------------------------


lm1 <- lm(formula = Leava_Prob ~ ., data = t1_data[,-c(1:2)])
step(lm1)
lm2 <- lm(formula = Leava_Prob ~ AreaType + RegionName + Black + Yellow + 
              L1Quals + L4Quals_plus + Students + Density + C2 + Young_age + 
              Retirement_age, data = t1_data)

anova(lm1,lm2)

#------



glm1 <- glm(formula = Leava_Prob ~ AreaType + RegionName + Black + Yellow + 
              L1Quals + L4Quals_plus + Students + Density + C2 + Young_age + 
              Retirement_age, data = t1_data,family=gaussian)

glm2 <- glm(formula = cbind(Leave,NVotes-Leave) ~ AreaType +RegionName + Black + Yellow + 
              L1Quals + L4Quals_plus + Students + Density + C2 + Young_age + 
              Retirement_age, data = t1_data,family=binomial)


#------


gam2 <- gam(formula = Leava_Prob ~ AreaType + RegionName + s(Black,k=3) + s(Yellow,k=5) + 
              s(L1Quals,k=3) + s(L4Quals_plus,k=3) + s(Students) + s(Density,k=3) + C2 + s(Young_age,k=5) + 
              s(Retirement_age,k=5,by=RegionName), data = t1_data,family=gaussian)

gam3 <- gam(formula = cbind(Leave,NVotes-Leave) ~ AreaType + RegionName + s(Black) + s(Yellow) + 
              s(L1Quals) + s(L4Quals_plus) + s(Students) + s(Density) + s(C2) + s(Young_age) + 
              s(Retirement_age), data = t1_data,family=binomial)


summary(gam3)
summary(gam2)
anova.gam(gam2,gam3,test = 'Chisq')


comp <- function(obj,data,type='g'){
	pred <- predict(obj,newdata = data,se.fit = T,type = 'response')
	real <- data$Leava_Prob
	rmse <- round(mean( (pred$fit-real)^2),5)
	sse <- round(sum( (pred$fit-real)^2),5)
	tss <- sum((real-mean(real))^2)
	ess <- sum((pred$fit-mean(real))^2)
	r2 <- ess/tss
	# sigma 这里有问题
    if(type=='g'){
    	sigma <- pred$se.fit} else{
    		sigma <- sqrt( pred$se.fit^2 + pred$fit * (1-pred$fit)/data$NVotes)
    	}
    s <- sum( log(sigma) + (real-pred$fit)^2 / (2*sigma^2) )
    return(data.frame(rmse=rmse,sse=sse,r2=r2,s=s))
}

comp(gam2,t2_data)


ggplot(data=train_data,aes(x=Retirement_age,y=Leava_Prob,col=RegionName))+geom_point()+
    geom_smooth(se=F,method = 'gam',formula = y ~ s(x))
