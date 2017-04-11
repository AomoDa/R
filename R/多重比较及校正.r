data(airquality)
str(airquality)

pairwise.t.test(x =airquality$Ozone,g = airquality$Month,p.adjust.method = 'none')
#Bonferroni
pairwise.t.test(x =airquality$Ozone,g = airquality$Month,p.adjust.method = 'bonferroni')
# fdr
pairwise.t.test(x =airquality$Ozone,g = airquality$Month,p.adjust.method = 'fdr')
# Tukey
TukeyHSD(aov(Ozone~as.factor(Month),data=airquality),conf.level = 0.95)
