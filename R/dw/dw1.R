
#http://www.stat.illinois.edu/courses/stat100/Notes/Chap9.pdf

x <- read.csv('ReferendumResults.csv',header = T,stringsAsFactors = F)

# create a variable which is the proportion of 'Leave' votes
x$Leava_Prob <- 100* x$Leave/x$NVotes

# age
x$Underage <- x$Age_0to4 + x$Age_5to7+x$Age_8to9+x$Age_10to14+x$Age_15+x$Age_16to17
x$Young_age <- x$Age_18to19+x$Age_20to24+x$Age_25to29
x$Working_age <- x$Age_30to44+x$Age_45to59+x$Age_60to64
x$Retirement_age <- x$Age_65to74+x$Age_75to84+x$Age_85to89+x$Age_90plus

#ethnicity 
x$Yellow <- x$Asian + x$Indian + x$Pakistani

#household
x$Owned_Yes <- x$Owned + x$OwnedOutright
x$Owned_No <- x$SocialRent + x$PrivateRent

#social grades
x$C2 <- x$C2DE - x$DE
x$C1 <- x$C1C2DE - x$C2DE

# mydata

mydata <- x[,c('Leava_Prob','AreaType','RegionName','Residents',
	            'Households','MeanAge','AdultMeanAge','White',
	            'Black','Yellow','Owned_Yes','Owned_No',
	            'NoQuals','L1Quals','L4Quals_plus','Students',
	            'Unemp','UnempRate_EA','HigherOccup','RoutineOccupOrLTU',
	            'Density','Deprived','MultiDepriv','C2','C1','DE',
	            'Underage','Young_age','Working_age','Retirement_age'
	            )]





#Postals

TukeyHSD(aov(Leava_Prob~AreaType,data=model_data))


#PCA
library(psych)
#Parallel analysis suggests that  the number of components =  5
fa.parallel(mydata[,-c(1:3)],fa='pc')
pc1 <- principal(mydata[,-c(1:3)],nfactors = 5,rotate = 'varimax',scores = T)
dd <- cbind(mydata[,1:2],as.data.frame(pc1$scores))

# train dataset and predict dataset
model_data <- dd[x$Leave!=-1,]
pred_data <- dd[x$Leave==-1,]

#
set.seed(1)
ind <- sample(x = 2,size = nrow(model_data),replace = T,prob = c(0.9,0.1))
train_data <- model_data[ind==1,]
test_data <- model_data[ind==2,]

# lasso
library(glmnet)

x <- model.matrix(Leava_Prob~.,data=train_data)[,-1]
y <- train_data$Leava_Prob
xx <- cv.glmnet(x,y,alpha=1)


#----------------------------------------
#gam
#-----------------------------------------

library(mgcv)

gam1 <- gam(Leava_Prob~AreaType+s(RC1)+s(RC2)+s(RC3)+s(RC4)+s(RC5),data = train_data)

summary(gam1)
