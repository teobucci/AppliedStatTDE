###################EX1
rm(list = ls())
pinnanobilis <- read.table('pinnanobilis.txt', header=T)
head(pinnanobilis)

pinna.e <- dist(pinnanobilis, method='euclidean')
pinna.ec <- hclust(pinna.e, method='complete')
x11()
plot(pinna.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
#rect.hclust(pinna.ec, k=2)
rect.hclust(pinna.ec, k=2)

cluster.ec <- cutree(pinna.ec, k=2) # euclidean-complete:
cluster.ec
table(cluster.ec)
x11()
plot(pinnanobilis, col=cluster.ec, pch=19)
coph.ec <- cophenetic(pinna.ec)
ec <- cor(pinna.e, coph.ec)
t1.mean <- sapply(pinnanobilis[cluster.ec=='1',],mean)
t2.mean <- sapply(pinnanobilis[cluster.ec=='2',],mean)

#(b)
n=dim(pinnanobilis)[1]
p=dim(pinnanobilis)[2]
levels(factor(cluster.ec))

# variables:
g <- 2
i1 <- which(cluster.ec==1)
i2 <- which(cluster.ec==2)
ng <- c(length(i1),length(i2)) 
ng

N <- sum(ng)
#One-way MANOVA:
### Model: X.ij = mu + tau.i + eps.ij; eps.ij~N_p(0,Sigma), [p=2]
###       X.ij, mu, tau.i in R^2, i=1,2,3 

# verify assumptions
# 1) normality
Ps <- c(mcshapiro.test(pinnanobilis[ i1, ])$p,
        mcshapiro.test(pinnanobilis[ i2, ])$p)
Ps
fit <- manova(as.matrix(pinnanobilis) ~ cluster.ec)
summary.manova(fit,test="Wilks")

# who's the responsible?
summary.aov(fit,test="Wilks")
# both variables

alpha <- 0.1
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(pinnanobilis,mean)         # estimates mu
m1 <- sapply(pinnanobilis[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(pinnanobilis[i2,],mean)    # estimates mu.2=mu+tau.2


inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/ng[1]+1/ng[2]) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/ng[1]+1/ng[2]) )


CI <- list(cbind(inf12, sup12))
CI

##############EX3
### Model:
### price = beta0 + beta1*length + beta2*power + beta3*draught + beta4*crew + beta5*year + beta6*material + Eps
### (linear in the parameters!)
### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)

rm(list = ls())
boats <- read.table("boats.txt")
boats=boats[-1,]
for (i in 1:6){
  boats[,i]=as.integer(boats[,i])
}

head(boats)
N <- dim(boats)[1]
groups=levels(factor(boats$V7))
groups
dummy=ifelse(boats$V7=='wood',1,0)
mod1 <- lm(V1 ~., data = boats)
summary(mod1)

coefs <- coef(mod1)
# should do all beta as sum of coeff :(
shapiro.test(mod1$residuals) # p-value = 0.2296 al 95% gaussiani
x11()
par(mfrow = c(2,2))
plot(mod1) #ok

library(car)
linearHypothesis(mod1, rbind(c(0,1,0,0,0,0,0),
                             c(0,0,1,0,0,0,0),
                             c(0,0,0,1,0,0,0)), c(0,0,0))   #2.2e-16 *** influisce
linearHypothesis(mod1, rbind(c(0,0,0,0,0,1,0),
                             c(0,0,0,0,0,0,1)), c(0,0))    #2.2e-16 *** influisce

mod2 <- lm(V1 ~V2+V3+V5+V6+V7, data = boats)
summary(mod2)

mod3 <- lm(V1 ~V2+V3+V5+dummy, data = boats)
summary(mod3)

shapiro.test(mod3$residuals) #pval= .1515 al pelo ma ok
vif(mod3) #ok
x11()
par(mfrow = c(2,2))
plot(mod3) #ok

coefs <- coef(mod3)

newdat <- data.frame(V2 = 10, V3=1070,V5=1,dummy=0)
guess <- predict(mod3, newdat, interval = 'confidence', level = 0.95)
guess


#       fit      lwr      upr
# 2517.115 2410.393 2623.836



###################EX4
rm(list=ls())
wind <- read.table('wind.txt', header=TRUE)
head(wind)
day1=t(as.matrix(wind[1,]))
abscissa=1:24
x11()
plot(abscissa,day1)

m=3
nbase=12
basis <- create.bspline.basis(c(0,24), nbase, m)

data_W <- t(wind)

data_W.fd.1 <- Data2fd(y = data_W,argvals = abscissa,basisobj = basis)
x11()
plot.fd(data_W.fd.1)

data_W.fd.1$coefs[1:3,1]
#bspl3.1  bspl3.2  bspl3.3 
#21.92529 12.41474 28.44647 
#(b)
pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=29 are non-null perchè i viene 12???
plot(pca_W.1$values[1:12],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:12]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))
cumsum(pca_W.1$values)[1:3]/sum(pca_W.1$values) #variance explained by first 3
sum(pca_W.1$values[1:3])/sum(pca_W.1$values) 
# first 3 FPCs
x11()
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')

# plot of the FPCs as perturbation of the mean
media <- mean.fd(data_W.fd.1)
x11()
par(mfrow = c(1,3))
plot(media,lwd=2, ylab='temperature',main='FPC1')
lines(media+pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=2)
lines(media-pca_W.1$harmonics[1,]*sqrt(pca_W.1$values[1]), col=3)

plot(media,lwd=2 ,ylab='temperature',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)

plot(media,lwd=2 ,ylab='temperature',main='FPC3')
lines(media+pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=2)
lines(media-pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=3)
#first - weighted average with heavier weights in the central part of the day 
#second - contrast between first 15 hour of the day and the rest of the evening 

# scatter plot of the scores
n=dim(wind)[1]
x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
points(pca_W.1$scores[1,1],pca_W.1$scores[1,2],col=2, lwd=4)  #point corresponding day1

#day 1 has high value of wind intensity in the first part of the day and generally an above the mean measurement of wind speed throughout the day (especially around half of the day) 
