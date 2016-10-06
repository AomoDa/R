

######################################################
# 0. load and explore the tips data
######################################################
library("reshape2")
?tips
head(tips)
summary(tips)
pairs(tips)


######################################################
# 1. simple linear regression: tip ~ total_bill
######################################################
plot(tips$total_bill, tips$tip)

## run regression
lm1 <- lm(formula= tip ~ total_bill, data=tips)
summary(lm1)
# plot the regression line
abline(lm1, col="red")

# alternatively, manual OLS
b1.hat <- cov(tips$tip, tips$total_bill) / var(tips$total_bill)
b0.hat <- mean(tips$tip) - b1.hat * mean(tips$total_bill)

# regression line always goes through x_bar, y_bar
abline(h=mean(tips$tip), lty=2)
abline(v=mean(tips$total_bill), lty=2)


## interpret regression output
str(lm1)
names(lm1)
str(summary(lm1))

# coefficient estimates
coef(lm1)  # or lm1$coefficients
# coefficient confidence intervals
confint(lm1)
# y_hat
fitted(lm1)  # or lm1$fitted.values
# R^2
summary(lm1)$r.squared

# alternative way to calculate R^2
cor(tips$tip, tips$total_bill)^2
cor(tips$tip, fitted(lm1))^2

# prediction based on x_new
predict(lm1, data.frame(total_bill=(c(5, 10, 15))), interval="confidence")

# more statistics
par(mfrow=c(2, 2))  # setup 2x2 plot mode
plot(lm1)
par(mfrow=c(1, 1))  # reset back to default single plot mode


######################################################
# 2. multiple linear regression
######################################################
## multiple regression
lm2 <- lm(formula= tip ~ total_bill + size, data=tips)
summary(lm2)


## multiple regression with dummy variables
lm3 <- lm(formula= tip ~ total_bill + size + sex, data=tips)
summary(lm3)
# sexMale is not significant, we have a slightly higher R^2 but lower adjusted R^2 compared to model 2

# two separated regressions
lm3m <- lm(formula= tip ~ total_bill + size, data=tips, subset=(sex=="Male"))
summary(lm3m)
lm3f <- lm(formula= tip ~ total_bill + size, data=tips, subset=(sex=="Female"))
summary(lm3f)


## multiple regression with more dummy variables
lm4 <- lm(formula= tip ~ total_bill + size + day, data=tips)
summary(lm4)

# check the dummy variable coding
contrasts(tips$day)
# modify dummy variable coding to use Thur as the baseline
tips2 <- tips
tips2$day <- relevel(tips$day, ref="Thur")
contrasts(tips2$day)
# regression with the releveled dataset
lm4b <- lm(formula= tip ~ total_bill + size + day, data=tips2)
summary(lm4b)


## multiple regression with interactions
# without interaction
lm5a <- lm(formula= tip ~ total_bill + size + smoker, data=tips)
summary(lm5a)
# with interaction
lm5b <- lm(formula= tip ~ total_bill * smoker + size, data=tips)
summary(lm5b)
# alternatively, we can use update() function to add the interaction term
# or manually: lm5c <- lm(formula= tip ~ total_bill + size + smoker + total_bill:smoker, data=tips)
lm5c <- update(lm5a, formula= . ~ . + total_bill:smoker)
summary(lm5c)


## multiple regression with polynomial terms
# quadratic
lm6a <- lm(formula= tip ~ total_bill + I(total_bill^2) + size, data=tips)
summary(lm6a)

# polynomial
lm6b <- lm(formula= tip ~ poly(total_bill, 4, raw=TRUE), data=tips)
summary(lm6b)

# visualize polynomial fit
tips_new <- data.frame(total_bill=seq(1, 60, 0.5))
tips_new$pred6b <- predict(lm6b, tips_new)
plot(tips$total_bill, tips$tip)
lines(tips_new$total_bill, tips_new$pred6b)

# log
lm6c <- lm(formula= tip ~ log(total_bill) + size, data=tips)
summary(lm6c)

# more log
lm6d <- lm(formula= log(tip) ~ log(total_bill) + log(size), data=tips)
summary(lm6d)


######################################################
# 3. variable selection using stepAIC()
######################################################
library(MASS)
lm.all <- lm(formula= tip ~ ., data=tips)
summary(lm.all)

# forward selection
step.f <- stepAIC(lm1, scope=list(upper= ~ total_bill + size + sex + smoker + day + time, lower= ~ 1), direction="forward")
step.f$anova # display results

# backward selection
step.b <- stepAIC(lm.all, direction="backward")
step.b$anova # display results

# could potentially include all interactions ...
lm.allint <- lm(formula= tip ~ total_bill * size * sex * smoker * day * time, data=tips)
summary(lm.allint)
step.allint <- stepAIC(lm.allint, direction="backward")


######################################################
# 4. practical issues
######################################################
## multicollinearity
tips$total_bill2 <- (10 + rnorm(nrow(tips))) * tips$total_bill
cor(tips$total_bill, tips$total_bill2)
lm7 <- lm(formula= tip ~ total_bill + total_bill2, data=tips)
summary(lm7)
summary(lm1)


######################################################
# 5. tricks
######################################################

## discretize continuous variable into categorial in order to capture more nonlinearlity
tips$total_bill_d <- ifelse(tips$total_bill < 10, "0--10", ifelse(tips$total_bill < 20, "10--20", ifelse(tips$total_bill < 30, "20--30", ifelse(tips$total_bill < 40, "30--40", ifelse(tips$total_bill < 50, "40--50", "50+")))))

lm8 <- lm(formula= tip ~ total_bill_d * smoker, data=tips)
summary(lm8)

# visualize nonlinear fit
tips_new$smoker <- sample(c("Yes", "No"), nrow(tips_new), replace=TRUE)
tips_new$total_bill_d  <- ifelse(tips_new$total_bill < 10, "0--10", ifelse(tips_new$total_bill < 20, "10--20", ifelse(tips_new$total_bill < 30, "20--30", ifelse(tips_new$total_bill < 40, "30--40", ifelse(tips_new$total_bill < 50, "40--50", "50+")))))
tips_new$pred8 <- predict(lm8, tips_new)

plot(tips$total_bill, tips$tip)
lines(tips_new[tips_new$smoker == "No", "total_bill"], tips_new[tips_new$smoker == "No", "pred8"], col="blue")
lines(tips_new[tips_new$smoker == "Yes", "total_bill"], tips_new[tips_new$smoker == "Yes", "pred8"], col="red")


## alternative formulation 
plot(tips$total_bill, tips$tip / tips$total_bill)
tips$percent <- tips$tip / tips$total_bill

lm0 <- lm(percent ~ total_bill * smoker + size, data=tips)
summary(lm0)



