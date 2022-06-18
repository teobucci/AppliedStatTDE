#################EX1
rm(list = ls())
holiday <- read.table('holiday.txt', header=T)
head(holiday)
attach(holiday)
#(a)
# Model with interaction (complete model): 
# X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect city), j=1,2,3 (effect type)
fit1 <- aov(price  ~ location + type + location:type, holiday) 
summary(fit1)

# Model without interaction (additive model): 
# X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect city), j=1,2,3 (effect type)
fit2 <- aov(price  ~ location + type, holiday) 
summary(fit2)
# Reduced model (one-way): 
# X.jk = mu + beta.j + eps.jk; eps.jk~N(0,sigma^2), 
#     j=1,2,3 (effect type)
fit3 <- aov(price ~ type, holiday) 
summary(fit3)

#Hypothesis
g=2
b=3
cities=levels(factor(location))
tipes=levels(factor(type))
Ps={}
for(i in 1:g){
  for(j in 1:b){
    Ps=c(Ps,shapiro.test(price[which(location==cities[i])][which(type==tipes[j])])$p)
  }
}
Ps


bartlett.test(price , factor(type):factor(location))   # p-value = 0.2473


#(c)
### Interval at 95% for the differences (reduced additive model)
df <- fit3$df # n-g
Spooled <- sum(fit3$res^2)/df

means <- as.vector(tapply(  holiday$price,   holiday$type, mean))
names(means) <- levels(area)
means

alpha <- 0.05
k     <- 3 # b
g=3 #cambio per non cambiare tutto sotto
qT <- qt(1-alpha/(2*k), df)

ng <- c(length(which(type==levels(factor(type))[1])),length(which(type==levels(factor(type))[2])),length(which(type==levels(factor(type))[3])))
n=sum(ng)

inf12 <- means[1]-means[2] - qT * sqrt(  (Spooled)/(n-g) * (1/ng[1]+1/ng[2]) )
sup12 <- means[1]-means[2] + qT * sqrt(  (Spooled)/(n-g) * (1/ng[1]+1/ng[2]) )
inf13 <- means[1]-means[3] - qT * sqrt(  (Spooled)/(n-g) * (1/ng[1]+1/ng[3]) )
sup13 <- means[1]-means[3] + qT * sqrt(  (Spooled)/(n-g) * (1/ng[1]+1/ng[3]) )
inf23 <- means[2]-means[3] - qT * sqrt(  (Spooled)/(n-g) * (1/ng[2]+1/ng[3]) )
sup23 <- means[2]-means[3] + qT * sqrt(  (Spooled)/(n-g) * (1/ng[2]+1/ng[3]) )

CI <- rbind(cbind(inf12, sup12), cbind(inf13, sup13), cbind(inf23, sup23))
CI
m1 <- means[1]
m2 <- means[2]
m3 <- means[3]
detach(holiday)

##################EX2
rm(list=ls())
beans <- read.table('beans.txt', header=TRUE)
head(beans)

n <- dim(beans)[1]
p <- dim(beans)[2]

beansnum=beans[,-9]
# Boxplot
x11()
par(mar=rep(8,4))
boxplot(beansnum, las=2, col='gold')


x11()
par(mar=rep(8,4))
boxplot(scale(x=beansnum,center = T, scale=F), las=2, col='gold')

# We perform the PCA on original data
pc.beans <- princomp(beansnum, scores=T)
pc.beans
summary(pc.beans)

# To obtain the rows of the summary:
# standard deviation of the components
pc.beans$sd
# proportion of variance explained by each PC
pc.beans$sd^2/sum(pc.beans$sd^2)
# cumulative proportion of explained variance
cumsum(pc.beans$sd^2)/sum(pc.beans$sd^2)

# loadings (recall: coefficients of the linear combination of the original 
#           variables that defines each principal component)

load.tour <- pc.beans$loadings
load.tour

load.tour[,1:8]

# graphical representation of the loadings of the first 3 principal components
x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PCs: mean of area and convex area
# Second PCs:  contrast between area and convex area
# Third PC: perimeter and major vs minor


beans.sd <- scale(beansnum)
beans.sd <- data.frame(beans.sd)

head(beans.sd)

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(beans.sd, las=2, col='gold')

pc.beans <- princomp(beans.sd, scores=T)
pc.beans
summary(pc.beans)

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.beans, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(beans.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.beans$sde^2)/sum(pc.beans$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(beans.sd),labels=1:ncol(beans.sd),las=2)

# If we wanted to perform dimensionality reduction, we could keep
# 1 or 2 PCs

# loadings
load.tour <- pc.beans$loadings
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

# High PC1: general high flow of beans
# Low PC1: general low flow of beans 
# High PC2: high flow for expensive solutions, low flow for cheap solutions
# Low PC2: low flow for expensive solutions, high flow for cheap solutions
scores.beans <- pc.beans$scores
scores.beans
x11()
plot(scores.beans[,1:2],col=factor(beans$Type))
points(scores.beans[,1], rep(-3, n),  pch=19)
points(rep(0, n),scores.beans[,2],  pch=19)
abline(h=0, v=0, lty=2, col='grey')

#evidente la divisione che si crea sulla prima pC

vec=scores.beans[which(beans$Type=='cannellini'),1:2]
vec
n=dim(vec)[1]
p=dim(vec)[2]
x.mean   <- colMeans(vec)
x.cov    <- cov(vec)

alpha   <- .05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Center:
x.mean

# Directions of the principal axes:
eigen(x.cov/n)$vectors

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov/n)$values) 
#H0:X ~ N_p()    X1: H0^c
mcshapiro.test(vec)  #$pvalue 0.0152
library(car)
x11()
plot(vec)
ellipse(x.mean,(x.cov/n),r*sqrt(eigen(x.cov/n)$values) )

###################EX3
rm(list=ls())
students <- read.table('students.txt', header=TRUE)
head(students)

mod1 <- lm(watchtv ~ . , data = students)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2

shapiro.test(mod1$residuals) # p-value = 0.01178 gaussiani

par(mfrow = c(2,2))
plot(mod1)
library(car)
vif(mod1)

#(b)
library(glmnet)
x <- model.matrix(watchtv ~ . , data = students)[,-1] # matrix of predictors
y <- students$watchtv # vector of response
fit.lasso <- glmnet(x,y, lambda = 0.3, alpha=1) # alpha=1 -> lasso
coef.lasso <- predict(fit.lasso, s=0.3, type = 'coefficients')[1:10,]    # p+1(intercetta)-1(y)=p
coef.lasso 


#(c)
lambda.grid=10^seq(-2,0,length=50)
set.seed(1)
cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=3,lambda=lambda.grid)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:10,]    # p+1(intercetta)-1(y)=p
coef.lasso
#(d)
Z0   <- data.frame(gender='male', age=21, height=73,
                   distance=100, siblings=1, computertime=10, exercisehours=2, musiccds=35, playgames=4)
Z0=as.matrix(Z0)
predict(cv.lasso,s=bestlam.lasso,Z0, type="response") 


###################EX4
rm(list=ls())
power <- read.table('power.txt', header=TRUE)
head(power)
abscissa=1:365
pow=power[,1]
x11()
plot(abscissa,pow)

# generalized cross-validation
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(rangeval=c(0,365), nbasis[i])
  gcv[i] <- smooth.basis(abscissa, pow, basis)$gcv
}
x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbase=nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

basis <- create.fourier.basis(rangeval=c(0,365), nbase)
x11()
plot(basis)
#####opzione 1
pow.fd.1 <- Data2fd(y = pow,argvals = abscissa,basisobj = basis)
x11()
plot(abscissa,pow)
plot.fd(pow.fd.1)
#######opt 2 better
Xsp <- smooth.basis(argvals=abscissa, y=pow, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd)

x11()
par(mfrow=c(1,1))
plot(abscissa,pow,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="green",lwd=2)
abline(v=basis$params,lty=2)

#(b)
NT <- length(abscissa) # number of locations of observations
rappincX1 <- (pow[3:NT]-pow[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])


#altrimenti: 
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1)
x11()
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1bis ,type="l",col="blue",lwd=2)

#(c)
nbaseovers=4
basis <- create.fourier.basis(rangeval=c(0,365), nbaseovers)
Xsp <- smooth.basis(argvals=abscissa, y=pow, fdParobj=basis)
Xsp0bisOVER <- eval.fd(abscissa, Xsp$fd)

x11()
par(mfrow=c(1,1))
plot(abscissa,pow,xlab="t",ylab="observed data")
points(abscissa,Xsp0bisOVER ,type="l",col="blue",lwd=2)

#(d)
nbaseoverf=40
basis <- create.fourier.basis(rangeval=c(0,365), nbaseoverf)
Xsp <- smooth.basis(argvals=abscissa, y=pow, fdParobj=basis)
Xsp0bisOVERF <- eval.fd(abscissa, Xsp$fd)

x11()
par(mfrow=c(1,1))
plot(abscissa,pow,xlab="t",ylab="observed data")
points(abscissa,Xsp0bisOVERF ,type="l",col="red",lwd=2)

x11()
par(mfrow=c(1,1))
plot(abscissa,pow,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="green",lwd=2)
points(abscissa,Xsp0bisOVER ,type="l",col="blue",lwd=2)
points(abscissa,Xsp0bisOVERF ,type="l",col="red",lwd=2)