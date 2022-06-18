rm(list=ls())
data <- read.table('wind.txt', header=TRUE)
head(data)

# ------------------------- point 1

library(fda)

norder <- 3           # spline order (4th order polynomials)
degree <- norder-1    # spline degree
nbasis <- 12          # how many basis we want

basis <- create.bspline.basis(rangeval=c(1, 24),
                              nbasis=nbasis,
                              norder=norder)

par(mfrow=c(1,1))
matplot(t(data), type = 'l')

time <- 1:24

data_W.fd.1 <- Data2fd(y=t(data), argvals=time, basisobj=basis)
plot.fd(data_W.fd.1)
data_W.fd.1$coefs[1:3,1]
#  bspl3.1  bspl3.2  bspl3.3 
# 17.04582 17.63498 31.07959 

# ------------------------- point 2

# pca
pca_W.1 <- pca.fd(data_W.fd.1, nharm=5, centerfns=TRUE)

# scree plot
par(mfrow=c(1,2))
plot(pca_W.1$values[1:15], xlab='j', ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:15]/sum(pca_W.1$values), xlab='j', ylab='CPV', ylim=c(0.8, 1))

cumsum(pca_W.1$values)[3]/sum(pca_W.1$values)
# 0.9585892
sum(pca_W.1$varprop[1:3])

# eigenfunctions
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')

# ------------------------- point 3

# we keep the first 2 PC

media <- mean.fd(data_W.fd.1)

par(mfrow=c(1,2))
plot(media,lwd=2,ylim=c(5,35),main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)
# first - weighted average with heavier weights in the central part of the day 

plot(media,lwd=2,ylim=c(5,35),main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
# second - contrast between first 15 hour of the day and the rest of the evening  

# plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

# ------------------------- point 4

par(mfrow=c(1, 1))
plot(pca_W.1$scores[, 1], pca_W.1$scores[, 2], xlab="Scores FPC1", ylab="Scores FPC2", lwd=2)
points(pca_W.1$scores[1, 1], pca_W.1$scores[1, 2], col=3, lwd=4) # Day1

# Day1
# FCP1 positive: above average intensity throughout all day
# FCP2 positive: slighly above average in the first part of the day until 15 and then slighly below average

day1 <- t(as.matrix(data[1,]))
plot.fd(data_W.fd.1[1,])
points(time,day1)
lines(media,lwd=2)


































