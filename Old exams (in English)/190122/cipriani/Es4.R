rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("temperature.txt",header=T)
n <- nrow(d)

abscissa <- 1:365
time <- 1:365

matplot(t(d[,1:365]),type='l')

##### PUNTO A #####

basis.1 <- create.fourier.basis(rangeval=c(0,366),nbasis=21)
data_W.fd.1 <- Data2fd(y = t(d[,1:365]),argvals = time,basisobj = basis.1)
plot.fd(data_W.fd.1)


data_W.fd.1$coefs[1:3,1:2]
#        station1  station2
#const 468.49271 494.02015
#sin1  -94.95708 -89.44538
#cos1  -47.38558 -42.65703



##### PUNTO B #####

plot.fd(data_W.fd.1)

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

plot(pca_W.1$values[1:21],xlab='j',ylab='Eigenvalues') ### CHANGE 1:nsplines
plot(cumsum(pca_W.1$values)[1:21]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))
cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)

#0.8321720 0.9776170 0.9961174 0.9982426 0.9992815

# first three FPCs
x11()
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')


par(mfrow=c(1,3))
plot(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

# - First PC follows the mean with greater (positive weights)
# - Second PC is associated with higher values of temperature
# in first half of the year and lower values of temperature in
# 2nd part of year when positive (except last days) - peaks in spring/autumn
# - 3rd PC follows closely the mean behaviour with slight deviations

##### PUNTO C #####
par(mfrow=c(1,1))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",
     col=as.numeric(as.factor(d$country)),lwd=2)


# The two first PCs  (especially the first) clearly divide the functions
# in 2 clusters corresponding to the countries Italy and Germany
# Italy has in general positive values for 1st PC and lower for 2nd PC
# meaning temperature higher than the mean



##### PUNTO D #####
# The first 2 PCs are more than enough of a reduction
# there is a clear elbow in the scree plot
# interpretation already given




##### PUNTO E #####