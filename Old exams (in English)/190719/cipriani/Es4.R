rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("watertemp.txt",header=T)
n <- nrow(d)


abscissa <- 1:365
time <- 1:365

matplot(t(d[,1:365]),type='l')

##### PUNTO A #####

basis.1 <- create.fourier.basis(rangeval=c(0,366),nbasis=45)
data_W.fd.1 <- Data2fd(y = t(d[,1:365]),argvals = time,basisobj = basis.1)
plot.fd(data_W.fd.1)


data_W.fd.1$coefs[1:3,1:2]
#        station1  station2
#const 295.58925 272.28422
#sin1   33.46757  28.00425
#cos1  -35.58000 -33.24506



##### PUNTO B #####

plot.fd(data_W.fd.1)

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

plot(pca_W.1$values[1:21],xlab='j',ylab='Eigenvalues') ### CHANGE 1:nsplines
plot(cumsum(pca_W.1$values)[1:21]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))
cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)

# 0.8522955 0.9799717 0.9927873 0.9944077 0.9950904
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
# 2nd part of year when positive (except last 2 months where trend is higher) 
# - 3rd PC follows closely the mean behaviour with slight deviations in the middle
#   where positive score is associated with below mean temperature (and the opposite on first/last days)

##### PUNTO C #####
par(mfrow=c(1,1))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",
     col=as.numeric(as.factor(d$Zone)),lwd=2)


# The two first PCs  (especially the first) clearly divide the functions
# in 3 clusters corresponding to the zones: deep has negative first PC (lower temp)
# medium and surface have higher PCs and temps



##### PUNTO D #####
# The first 2 PCs are more than enough of a reduction
# there is a clear elbow in the scree plot
# interpretation already given




