rm(list=ls())
library(fda)
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("power.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
Xobs0 <- d$power
abscissa <- 1:365
NT <- length(abscissa) # number of locations of observations

x11()
plot(abscissa,t(Xobs0),xlab="t",ylab="observed data")
plot(abscissa,t(Xobs0),xlab="t",ylab="observed data", type = "l")


# compute the central finite differences
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincX2 <- ((Xobs0[3:NT]-Xobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Xobs0[2:(NT-1)]-Xobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])


nbasis <- 6:50
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
    basis <- create.fourier.basis(range(abscissa), nbasis[i])
    gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}

par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v=nbasis[which.min(gcv)],col='red')

# nbasis = 12

basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis[which.min(gcv)])
plot(basis) # plot basis system

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)
# smoothed data


##### PUNTO B #####
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative

plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1bis,type='l',col="orange",lwd=3)

# first increment very noisy bust still carries information
# for smoothed data





##### PUNTO C/D #####
### Overfitting
nbasis <- 50
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)




### Oversmoothing
nbasis <- 2
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)






