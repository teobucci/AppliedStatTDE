rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')
library(fda)
d <- read.table("spectra.txt",header=T)
n <- nrow(d)

abscissa <- 1:80
time <- 1:80

##### PUNTO A #####

Xobs0 <- d[1,]
NT <- length(abscissa) # number of locations of observations

x11()
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
plot(abscissa,Xobs0,xlab="t",ylab="observed data", type = "l")


m <- 4          # spline order 
degree <- m-1    # spline degree 

nbasis <- 6:50
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
    basis <- create.bspline.basis(range(abscissa), nbasis=nbasis[i], norder=m)
    gcv[i] <- smooth.basis(abscissa, t(Xobs0), basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)] # 11
abline(v=nbasis[which.min(gcv)],col='red')


nbasis <- 11
basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=11, norder=m)
basismat <- eval.basis(abscissa, basis)
est_coef = lsfit(basismat, t(Xobs0), intercept=FALSE)$coef
basismat <- eval.basis(abscissa, basis)
Xsp0 <- basismat %*% est_coef

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
abline(v=basis$params)


# coefficients
est_coef[1:3] # 0.02401024 0.10070402 0.33763038 

##### PUNTO B #####

data_W <- t(d)
matplot(data_W,type='l',main='Canadian temperature',xlab='Day',ylab='Temperature')

basis.1 <- create.bspline.basis(rangeval=c(1,80),nbasis=11, norder=m)
data_W.fd.1 <- Data2fd(y = data_W,argvals = time,basisobj = basis.1)
plot.fd(data_W.fd.1)


##### PUNTO C #####

library(fdakma)
x <- abscissa  # abscissas
y0 <- d # evaluations of original functions

rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
y1 <- rappincX1# evaluations of original functions' first derivatives

set.seed(1)
fdakma_example <- kma(
    x=x, y0=y0, n.clust = 3, 
    warping.method = 'affine', 
    similarity.method = 'd0.pearson',  # similarity computed as the cosine
    # between the first derivatives 
    # (correlation)
    center.method = 'k-means'
    #seeds = c(1,21) # you can give a little help to the algorithm...
)

kma.show.results(fdakma_example)
# Don't know how to comment warping function, maybe one cluster has higher phase variability
# than the others, and one basically zero


