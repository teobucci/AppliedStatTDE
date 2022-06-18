rm(list=ls())
data <- read.table('traffic.txt', header=TRUE)
head(data)

# ------------------------- point 1

library(fda)

norder <- 4           # spline order (4th order polynomials)
degree <- norder-1    # spline degree
nbasis <- 15          # how many basis we want

basis <- create.bspline.basis(rangeval=c(1, 24),
                              nbasis=nbasis,
                              norder=norder)

matplot(t(data), type = 'l')

time <- 1:24

data_W.fd.1 <- Data2fd(y=t(data), argvals=time, basisobj=basis)
plot.fd(data_W.fd.1)
data_W.fd.1$coefs[1:3,1]
# bspl4.1  bspl4.2  bspl4.3 
# 616.5454 354.9235 463.4883 

# ------------------------- point 2

# pca
pca_W.1 <- pca.fd(data_W.fd.1, nharm=5, centerfns=TRUE)

# scree plot
par(mfrow=c(1,2))
plot(pca_W.1$values[1:15], xlab='j', ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:15]/sum(pca_W.1$values), xlab='j', ylab='CPV', ylim=c(0.8, 1))

cumsum(pca_W.1$values)[3]/sum(pca_W.1$values)
# 0.9795244
sum(pca_W.1$varprop[1:3])

# eigenfunctions
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')

# ------------------------- point 3

# we keep the first PC associated to the morning peak hour and late afternoon peak hour

# ------------------------- point 4

par(mfrow=c(1, 1))
plot(1:30, pca_W.1$scores[, 1], xlab="Day", ylab="Scores FPC1", lwd=2)
#points(2, pca_W.1$scores[2, 1], col=3, lwd=4) # 2nd day

# weekend and working days








































