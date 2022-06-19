#ex 1 

#a) weather <- read.table('weather.txt', header=T)
head(weather)
dim(weather)

n <- dim(weather)[1]
p <- dim(weather)[2]
# Boxplot
x11()
par(mar=rep(8,4))
boxplot(weather, las=2, col='gold') 

x11()
par(mar=rep(8,4))
boxplot(scale(x=weather,center = T, scale=F), las=2, col='gold')

weather.sd <- scale(weather)
weather.sd <- data.frame(weather.sd)

head(weather.sd) #better to standardize the variables 

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(weather.sd, las=2, col='gold')

pc.weather <- princomp(weather, scores=T)
pc.weather
summary(pc.weather)

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pc.weather$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(weather,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(pc.weather$sdev^2)/sum(pc.weather$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(weather),labels=1:ncol(weather),las=2)

load.tour <- pc.weather$loadings
load.tour

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.tour[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

scores.weather <- pc.weather$scores
scores.weather

x11()
plot(scores.weather[,1:2])
abline(h=0, v=0, lty=2, col='grey')

x11()
layout(matrix(c(1,2),2))
boxplot(weather, las=2, col='gold', main='Standardized variables')
scores.weather <- data.frame(scores.weather)
boxplot(scores.weather, las=2, col='gold', main='Principal components')

x11()
biplot(pc.weather)


#ex 4) 
library(fda)

data <- read.table('tide.txt', header=T)
dim(data)
head(data)

NT <- dim(data)[1]
abscissa <- seq(0,23.5,by=0.5)
Xobs0 <- data$level

plot(abscissa,Xobs0, type = "l")

## point a)

nbasis <- 4:40
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(range(abscissa), nbasis[i], 4)
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

basis <- create.bspline.basis(rangeval=range(abscissa), nbasis=nbasis[which.min(gcv)],4)
plot(basis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)

#b) 
# Evaluate the basis on the grid of abscissa
basismat <- eval.basis(abscissa, basis)
dim(basismat) # number of data x number of basis
head(basismat)

# Fit via LS
help(lsfit)

est_coef <- lsfit(basismat, Xobs0, intercept=FALSE)$coef

Xsp0 <- basismat %*% est_coef

S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
sum(diag(S))
sigmahat <- sqrt(sum((Xsp0-Xobs0)^2)/(NT-12)) #estimate of sigma
lb <- Xsp0-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0+qnorm(0.975)*sigmahat*sqrt(diag(S))

x11()
plot(abscissa,Xsp0,type="l",col="blue",lwd=2,ylab="")
points(abscissa,lb,type="l",col="blue",lty="dashed")
points(abscissa,ub,type="l",col="blue",lty="dashed")

#c) 
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative

plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1bis,type='l',col="orange",lwd=3)
#not sure if I want a different basis 

