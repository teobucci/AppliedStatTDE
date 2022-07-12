rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("traffic.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
data_W <-  d
matplot(t(data_W),type='l',main='Canadian temperature',xlab='Day',ylab='Temperature')


m <- 4           # spline order 
degree <- m-1    # spline degree 
nbasis <- 15

abscissa = 1:24
time = 1:24
# Create the basis
basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=nbasis, norder=m)
data_W.fd.1 <- Data2fd(y = t(data_W),argvals = time,basisobj = basis)
plot.fd(data_W.fd.1)

data_W.fd.1$coefs[1:3,1]
#bspl4.1  bspl4.2  bspl4.3 
#616.5454 354.9235 463.4883 

##### PUNTO B #####
pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

# scree plot
plot(pca_W.1$values[1:15],xlab='j',ylab='Eigenvalues') ### CHANGE 1:nsplines
plot(cumsum(pca_W.1$values)[1:15]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))
cumsum(pca_W.1$values)[1:3]/sum(pca_W.1$values)

# 0.9071786 0.9646919 0.9795244

# first three FPCs
x11()
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')


##### PUNTO C #####
# ELBOW AT 2 <- retain first 2
par(mfrow=c(1,2))
plot(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)

# first PC follows exactly the mean with greater weights except
# for early and late hours when a positive score is associated to 
# a value below the mean (less traffic?)

# second pc is close to the mean in all hours except
# from 6 to 18 where a positive score is significantly higher than the mean



##### PUNTO D #####
# scatter plot of the scores
par(mfrow=c(1,1))
plot(1:30,pca_W.1$scores[,1],xlab="Day",ylab="Scores FPC1",lwd=2)

# weekends have less traffic


