rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("tide.txt",header=T)
n <- nrow(d)

data_W <- d[,2]


##### PUNTO A #####
matplot(data_W,type='l',main='Canadian temperature',xlab='Day',ylab='Temperature')
m <- 4           # spline order 
degree <- m-1    # spline degree 


time <- 1:48
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
    basis <- create.bspline.basis(range(time), nbasis[i], m)
    gcv[i] <- smooth.basis(time, data_W, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

# 12 basis
nbasis <- 12
basis <- create.bspline.basis(rangeval=range(time), nbasis=nbasis, norder=m)
basismat <- eval.basis(time, basis)

est_coef = lsfit(basismat, data_W, intercept=FALSE)$coef

Xsp0 <- basismat %*% est_coef
basismat1<- eval.basis(time, basis, Lfdobj=1)
Xsp1 <- basismat1 %*% est_coef
basismat2<- eval.basis(time, basis, Lfdobj=2)
Xsp2 <- basismat2 %*% est_coef

par(mfrow=c(1,1))
plot(time,Xobs0,xlab="t",ylab="observed data")
points(time,Xsp0 ,type="l",col="blue",lwd=2)
abline(v=basis$params)

##### PUNTO B #####

#### Approximate pointwise confidence intervals
# As in linear models, we can estimate the variance of x(t) as
# sigma^2*diag[phi*(phi'phi)^{-1}(phi)']
abscissa <- time
NT <- length(abscissa)
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
sum(diag(S))
sigmahat <- sqrt(sum((Xsp0-data_W)^2)/(NT-df)) #estimate of sigma
lb <- Xsp0-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0+qnorm(0.975)*sigmahat*sqrt(diag(S))


x11()
plot(abscissa,Xsp0,type="l",col="blue",lwd=2,ylab="")
points(abscissa,lb,type="l",col="blue",lty="dashed")
points(abscissa,ub,type="l",col="blue",lty="dashed")
points(abscissa,d[,2],type="l")




##### PUNTO C #####
Xobs0 <- data_W
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincX2 <- ((Xobs0[3:NT]-Xobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Xobs0[2:(NT-1)]-Xobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])


x11(width = 14)
par(mfrow=c(1,2))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,truecurve$X0vera ,type="l",col="orange",lwd=3)
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
legend("topleft", legend = c("noisy data","true curve","estimated curve"), col = c("black", "orange","blue"), lwd = c(1,3,2))
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(truecurve$Abscissa,truecurve$X1vera,type='l',col="orange",lwd=3)
points(abscissa,Xsp1 ,type="l",col="blue",lwd=2)




##### PUNTO D #####

# Fourier, è periodico


