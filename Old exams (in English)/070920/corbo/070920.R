##################EX1
rm(list=ls())
weather <- read.table('weather.txt', header=TRUE)
head(weather)

n <- dim(weather)[1]
p <- dim(weather)[2]

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(weather, las=2, col='gold')


x11()
par(mar=rep(8,4))
boxplot(scale(x=weather,center = T, scale=F), las=2, col='gold')

# We perform the PCA on original data
pc.weather <- princomp(weather, scores=T)
pc.weather
summary(pc.weather)

# To obtain the rows of the summary:
# standard deviation of the components
pc.weather$sd
# proportion of variance explained by each PC
pc.weather$sd^2/sum(pc.weather$sd^2)
# cumulative proportion of explained variance
cumsum(pc.weather$sd^2)/sum(pc.weather$sd^2)

# loadings (recall: coefficients of the linear combination of the original 
#           variables that defines each principal component)

load.tour <- pc.weather$loadings
load.tour

load.tour[,1:8]

# graphical representation of the loadings of the first 3 principal components
x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PCs: contrast between umidity and the others first 4
# Second PCs: weihted mean of umidity and the others first 4
# Third PC: -maxwind

# The loadings reflect the previous observation: the first 3 PCs are 
# driven by the variables displaying the highest variability

weather.sd <- scale(weather)
weather.sd <- data.frame(weather.sd)

head(weather.sd)

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(weather.sd, las=2, col='gold')

pc.weather <- princomp(weather.sd, scores=T)
pc.weather
summary(pc.weather)

# If we wanted to perform dimensionality reduction, we could keep
# 1 or 2 PCs

# loadings
load.tour <- pc.weather$loadings
load.tour

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.tour[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))
#More inerpretable
# Interpretation of the loadings:
# In this case, the first PC represents an average of the number of nights spent in 
# all the types of hotels and residences, taken with very similar weights.
# The second PC contrasts the more expensive solutions (4,5 stars hotels and residences)
# against the cheap solutions (1,2 stars hotels and B&B)

# High PC1: general high flow of weather
# Low PC1: general low flow of weather 
# High PC2: high flow for expensive solutions, low flow for cheap solutions
# Low PC2: low flow for expensive solutions, high flow for cheap solutions
#(b)
scores.weather <- pc.weather$scores
scores.weather
x11()
plot(scores.weather[,1:2])
abline(h=-3, v=-8, col=1)
points(scores.weather[,1], rep(-3, n),  pch=19)
points(rep(0, n),scores.weather[,2],  pch=19)
abline(h=0, v=0, lty=2, col='grey')
text(scores.weather[,1],scores.weather[,2],dimnames(t(weather))[[2]], cex=1)

#(c) Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.weather, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(weather.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.weather$sde^2)/sum(pc.weather$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(weather.sd),labels=1:ncol(weather.sd),las=2)

#explained variance of fisrt 3 Pc:
cumsum(pc.weather$sde^2)[1:3]/sum(pc.weather$sde^2)   #.9407453 

#(d)
data.aug1 <- c(30, 23, 36, 22, 65,19,5,15)
data.aug1.sd=scale(data.aug1)
scores.aug1 <- t(pc.weather$loadings)%*%(data.aug1.sd-colMeans(weather.sd))
scores.aug1

x11()
plot(scores.weather[,1],scores.weather[,2],col='grey',pch=19,xlab='Comp.1',ylab='Comp.2')
points(scores.aug1[1],scores.aug1[2],col='black',pch=19) 
##################EX2
rm(list=ls())
candle <- read.table('candle.txt', header=TRUE)
candle
sunshine <- read.table('sunshine.txt', header=TRUE)
sunshine

D <- data.frame(LM1=candle[,1]-sunshine[,1], LM2=candle[,2]-sunshine[,2]) 
D
x11()
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

### T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0)

# Test the Gaussian assumption (on D!)
mcshapiro.test(D)


n <- dim(D)[1]
p <- dim(D)[2]  

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- rep(0,p)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P
# reject H0 at 5% 

k=p

cfr.t <- qt(1-alpha/(2*k),n-1)
BF={}
for(i in 1:k){
  IC.BF=c( D.mean[i]-cfr.t*sqrt(D.cov[i,i]/n) , D.mean[i], D.mean[i]+cfr.t*sqrt(D.cov[i,i]/n) )
  BF=rbind(BF,IC.BF)
}
dimnames(BF)[[2]] <- c('inf','center','sup')
BF

#(d)
D1 <- data.frame(diffbetw2=(candle[,1]-candle[,2])-(sunshine[,1]-sunshine[,2])) 
D1
shapiro.test(D1)
t.test(D1, conf.level = 0.95)

#p-value = 5.634e-09 SI PUò AFFERMARE CHE C'è DIFFERENZA

##################EX3
rm(list=ls())
leaven <- read.table('leaven.txt', header=TRUE)
leaven
x11()
plot(leaven$time, leaven$volume, col=as.numeric(factor(leaven$yeast))+1, pch=20)
# Model:
# volume = beta0       + beta1*time       + beta2*time^2        +
#        + beta3*yeast*time + beta5*yeast*time^2  + eps
# weather beta*yeast cuz it ask the same intercept
leaven$t2=leaven$time^2
dummy=ifelse(leaven$yeast=='sd',1,0)
mod1 <- lm(volume ~ time  +t2  + dummy:time + dummy:t2 , data = leaven)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$garden
sigma2

shapiro.test(mod1$residuals) # p-value = 0.9284 gaussiani

par(mfrow = c(2,2))
plot(mod1)   #meh

#(b)
library(car)
linearHypothesis(mod1, rbind(c(0,0,0,1,0),c(0,0,0,0,1)), c(0,0))   #2.2e-16 *** influisce
linearHypothesis(mod1, c(0,0,0,0,1), 0) #2.2e-16 *** influisce
#(c)
mod2 <- lm(volume ~ t2  + dummy:time + dummy:t2 , data = leaven)
summary(mod2)
#(d)
newdat <- data.frame(t2=4,time=2 ,dummy=0)
guess <- predict(mod2, newdat, interval = 'confidence', level = 0.99)
guess
#fit      lwr      upr
# 1.063421 1.034237 1.092604
newdat1 <- data.frame(t2=4,time=2 , dummy=1)
guess1 <- predict(mod2, newdat1, interval = 'confidence', level = 0.99)
guess1
# fit      lwr      upr
# 1.126164 1.104025 1.148302

##################EX4
rm(list=ls())
tide <- read.table('tide.txt', header=TRUE)
tide
attach(tide)
# Load package fda
library(fda)

# Set parameters
m <- 4          # spline order 
degree <- m-1    # spline degree 
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(rangeval=c(0,23.5), nbasis[i], m)
  gcv[i] <- smooth.basis(time, level, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbase=nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)
basis <- create.bspline.basis(rangeval=c(0,23.5), nbase, m)
x11()
plot(basis)

Xsp <- smooth.basis(argvals=time, y=level, fdParobj=basis)
Xsp0bis <- eval.fd(time, Xsp$fd)

x11()
par(mfrow=c(1,1))
plot(time,level,xlab="t",ylab="observed data")
points(time,Xsp0bis ,type="l",col="green",lwd=2)
abline(v=basis$params,lty=2)

#(b) Approximate pointwise confidence intervals ####
# As in linear models, we can estimate the variance of x(t) as
# sigma^2*diag[phi*(phi'phi)^{-1}(phi)']
NT <- length(time) 
df <- Xsp$df   #  the degrees of freedom in the smoothing curve  
df             #  for regression splines the df are the number of basis
basismat <- eval.basis(time, basis)
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
sum(diag(S))
sigmahat <- sqrt(sum((Xsp0bis-level)^2)/(NT-df)) #estimate of sigma
lb <- Xsp0bis-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0bis+qnorm(0.975)*sigmahat*sqrt(diag(S))

x11()
plot(time,Xsp0bis,type="l",col=1,lwd=2,ylab="")
points(time,lb,type="l",col="blue",lty="dashed")
points(time,ub,type="l",col="blue",lty="dashed")

#(c)
rappincX1 <- (level[3:NT]-level[1:(NT-2)])/(time[3:NT]-time[1:(NT-2)])

Xsp1bis <- eval.fd(time, Xsp$fd, Lfd=1)
x11()
plot(time[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(time,Xsp1bis ,type="l",col="blue",lwd=2)
#(d) rifaccio tutto con fourier
m <- 4          # spline order 
degree <- m-1    # spline degree 
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(rangeval=c(0,23.5), nbasis[i], m)
  gcv[i] <- smooth.basis(time, level, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbase=nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)
basis <- create.fourier.basis(rangeval=c(0,23.5), nbase, m)
x11()
plot(basis)

Xsp <- smooth.basis(argvals=time, y=level, fdParobj=basis)
Xsp0bis <- eval.fd(time, Xsp$fd)

x11()
par(mfrow=c(1,1))
plot(time,level,xlab="t",ylab="observed data")
points(time,Xsp0bis ,type="l",col="green",lwd=2)
abline(v=basis$params,lty=2)

# Approximate pointwise confidence intervals ####
# As in linear models, we can estimate the variance of x(t) as
# sigma^2*diag[phi*(phi'phi)^{-1}(phi)']
NT <- length(time) 
df <- Xsp$df   #  the degrees of freedom in the smoothing curve  
df             #  for regression splines the df are the number of basis
basismat <- eval.basis(time, basis)
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
sum(diag(S))
sigmahat <- sqrt(sum((Xsp0bis-level)^2)/(NT-df)) #estimate of sigma
lb <- Xsp0bis-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0bis+qnorm(0.975)*sigmahat*sqrt(diag(S))

x11()
plot(time,Xsp0bis,type="l",col=1,lwd=2,ylab="")
points(time,lb,type="l",col="blue",lty="dashed")
points(time,ub,type="l",col="blue",lty="dashed")

rappincX1 <- (level[3:NT]-level[1:(NT-2)])/(time[3:NT]-time[1:(NT-2)])

Xsp1bis <- eval.fd(time, Xsp$fd, Lfd=1)
x11()
plot(time[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(time,Xsp1bis ,type="l",col="blue",lwd=2)


#può risulatre migliore grazie alla periodicità
detach(tide)

