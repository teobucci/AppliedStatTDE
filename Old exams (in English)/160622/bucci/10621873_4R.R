# -----------
# EXERCISE 4
# -----------

rm(list=ls())
d <- read.table('listening.txt', header=TRUE)
load('mcshapiro.test.RData')
head(d)
names(d)

n <- dim(d)[1]
p <- dim(d)[2]

# ------------------------- point a

library(fda)
library(fields)

data_W <- d
head(data_W)
dim(data_W)
par(mfrow=c(1,1))
matplot(t(data_W),type='l')
# or t(data_W)

#### BSPLINE
# set the parameters
norder <- 3         # spline order (4th order polynomials)
degree <- norder-1  # spline degree
nbasis <- 30         # how many basis we want

time <- 1:365
abscissa <- time

breaks <- abscissa

#functionalPar <- fdPar(fdobj=basis, Lfdobj=3, lambda=100)
#Xsster <- smooth.basis(abscissa, Xobs0, functionalParter)

basis <- create.bspline.basis(rangeval=range(abscissa), # 
                              nbasis=nbasis,
                              norder=norder)

data_W.bspline <- Data2fd(y = t(data_W), argvals = time, basisobj = basis)
plot.fd(data_W.bspline)


data_W.bspline$coefs[1:3,1]
#bspl3.1  bspl3.2  bspl3.3 
#21.13833 12.94600 19.04748 

# ------------------------- point b

#plot.fd(data_W.bspline)

pca_W.1 <- pca.fd(data_W.bspline,nharm=5,centerfns=TRUE)
pca_W.1$varprop

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=34 are non-null
plot(pca_W.1$values[1:3],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0,1))
abline(h=0.8)
# how much explained by the first 5
cumsum(pca_W.1$values)[2]/sum(pca_W.1$values)



# ------------------------- point c


par(mfrow = c(1,2))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1',ylim=c(-0.1,0.08))
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2',ylim=c(-0.1,0.08))


# ------------------------- point d


howmany <- c(1,2)
par(mfrow=howmany) # c(1,2) 
plot(pca_W.1, nx=100, pointplot=TRUE, harm=howmany, expand=0, cycle=FALSE)


# ------------------------- point e

par(mfrow=c(1,1))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
#points(pca_W.1$scores[35,1],pca_W.1$scores[35,2],col=2, lwd=4) # higlightin a particular object (35)






basis2 <- create.bspline.basis(breaks, norder=norder)
functionalPar <- fdPar(fdobj=basis2, Lfdobj=3, lambda=100)


































































































































