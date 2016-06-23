# load package 
library(spatstat)

# load data
larynx.can <- read.csv('LARYNX.csv',header = T,sep=',')
lung.can <- read.csv('LUNG.csv',header = T,sep=',')
incin.xy <- read.csv('INCIN.csv',header = T,sep=',')


summary(larynx.can)


larynx.ppp <- ppp(x = larynx.can$Easting,y = larynx.can$Northing,
	window = owin(xrange = c(348273,360779) , yrange = c(413295,428827)))
plot(larynx.ppp,pch=16)

# Add in the location of the incinerator (incin.xy) as a cross in a different colour
incin.ppp <- ppp(x = incin.xy$Easting,y = incin.xy$Northing,
	window = owin(xrange = c(348273,360779) , yrange = c(413295,428827)))
points(incin.ppp,col='red',pch=3,lwd=2)


# Q1

bins <- quadratcount(X = larynx.ppp,nx = 10,ny = 10)
plot(larynx.ppp,pch='+')
plot(bins,add=T,col='red',cex=1.5, lty=2)


# density
par(mfrow=c(2,2))
#sigma=300
lar_den  <- density.ppp(x = larynx.ppp,sigma = 300)
plot(lar_den,main='sigma = 300')
points(larynx.ppp,pch=3,col='white')
# sigma=3000
lar_den  <- density.ppp(x = larynx.ppp,sigma = 3000)
plot(lar_den,main='sigma = 3000')
points(larynx.ppp,pch=3,col='white')
#
sig_est <- bw.ppl(X = larynx.ppp)
sig_est
lar_den  <- density.ppp(x = larynx.ppp,sigma =round(sig_est,0) ) 
plot(lar_den,main=
	paste( 'sigma = ' ,round(sig_est,0) ,'and sig_est = ', round(sig_est,0),sep='' ) )
points(larynx.ppp,pch=3,col='white')
par(mfrow=c(1,1))


#persp
par(mfrow=c(2,2))
persp(lar_den,col='red',theta=50,main='theta=50')
persp(lar_den,col='red',theta=90,main='theta=90')
persp(lar_den,col='red',theta=180,main='theta=180')
persp(lar_den,col='red',theta=270,main='theta=270')
par(mfrow=c(1,1))



# Q2
par(mfrow=c(2,2))
Gs <- Gest(larynx.ppp)
plot(Gs,main='Gs')
Fs <- Fest(X = larynx.ppp)
plot(Fs,main='Fs')
# Q3
Ks <- Kest(X = larynx.ppp)
plot(Ks,main='Ks')
par(mfrow=c(1,1))


#Q4
par(mfrow=c(2,2))
kenv <- envelope(Y = larynx.ppp,fun = Gest,nsim = 99,verbose = F)
plot(kenv,main='envelope of Gest ')
kenv <- envelope(Y = larynx.ppp,fun = Fest,nsim = 99,verbose = F)
plot(kenv,main='envelope of Fest ')
kenv <- envelope(Y = larynx.ppp,fun = Kest,nsim = 99,verbose = F)
plot(kenv,main='envelope of Kest ')
par(mfrow=c(1,1))

# Q5


# Q6

summary(lung.can)

lung.ppp <- ppp(x = lung.can$Easting,y = lung.can$Northing,window = owin(xrange = c(346475,364435) , yrange = c(412437,428987)))
plot(lung.ppp,pch=16)
points(incin.ppp,col='red',pch=3)


lung.bins <- quadratcount(X = lung.ppp,nx = 10,ny = 10)
plot(lung.ppp,pch='+')
plot(lung.bins,add=T,col='red',cex=1.5, lty=2)



# density
par(mfrow=c(2,2))
#sigma=300
lung_den  <- density.ppp(x = lung.ppp,sigma = 300)
plot(lung_den,main='sigma = 300')
points(lung.ppp,pch='+',col='white')
# sigma=3000
lung_den  <- density.ppp(x = lung.ppp,sigma = 3000)
plot(lung_den,main='sigma = 3000')
points(lung.ppp,pch='+',col='white')
#
sig_est <- bw.ppl(X = lung.ppp)
sig_est
lung_den  <- density.ppp(x = lung.ppp,sigma =round(sig_est,0) ) 
plot(lung_den,main=
	paste( 'sigma = ' ,round(sig_est,0) ,'and sig_est = ', round(sig_est,0),sep='' ) )
points(lung.ppp,pch='+',col='white')
par(mfrow=c(1,1))



#persp
par(mfrow=c(2,2))
persp(lung_den,col='red',theta=50,main='theta=50')
persp(lung_den,col='red',theta=90,main='theta=90')
persp(lung_den,col='red',theta=180,main='theta=180')
persp(lung_den,col='red',theta=270,main='theta=270')
par(mfrow=c(1,1))

# 





par(mfrow=c(2,2))
Gs <- Gest(lung.ppp)
plot(Gs,main='Gs')
Fs <- Fest(X = lung.ppp)
plot(Fs,main='Fs')
Ks <- Kest(X = lung.ppp)
plot(Ks,main='Ks')
par(mfrow=c(1,1))



par(mfrow=c(2,2))
kenv <- envelope(Y = lung.ppp,fun = Gest,nsim = 99,verbose = F)
plot(kenv,main='envelope of Gest ')
kenv <- envelope(Y = lung.ppp,fun = Fest,nsim = 99,verbose = F)
plot(kenv,main='envelope of Fest ')
kenv <- envelope(Y = lung.ppp,fun = Kest,nsim = 99,verbose = F)
plot(kenv,main='envelope of Kest ')
par(mfrow=c(1,1))





# Q7
plot(lung.ppp)
points(larynx.ppp,col='red',pch=16)
