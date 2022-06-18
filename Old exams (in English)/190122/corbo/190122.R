#############EX1
rm(list = ls())
acoruna <- read.table('acoruna.txt', header=T)
acoruna
pontevedra <- read.table('pontevedra.txt', header=T)
pontevedra

# The distribution of two different tapas are independent -> two differernt gaussian population!
# I can't do the dataset of differences

t1 <- acoruna
t2 <- pontevedra

n1 <- dim(t1)[1] # n1=30
n2 <- dim(t2)[1] # n2=30
p  <- dim(t1)[2] # p=2


# we compute the sample mean, covariance matrices and the matrix Spooled

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)

alpha   <- .01
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)

cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # FALSE: statistical evidence to reject H0 at level 1%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P # 0.0004434524  

# I can reject H0! The mean evaluation in the two cities differ.

# b) Interpret the results of the test at point (a) through two Bonferroni intervals of global 
# level 95% for appropriate differences in the mean. Comment the result

k <- p  # 2
cfr.t <- qt(1-alpha/(2*k),n1+n2-2)

IC.BF.1 <- c(t2.mean[1]-t1.mean[1]-cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)), t2.mean[1]-t1.mean[1]+cfr.t*sqrt(Sp[1,1]*(1/n1+1/n2)) )
IC.BF.2 <- c(t2.mean[2]-t1.mean[2]-cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)), t2.mean[2]-t1.mean[2]+cfr.t*sqrt(Sp[2,2]*(1/n1+1/n2)) )
IC.BF <- rbind(IC.BF.1, IC.BF.2)
dimnames(IC.BF)[[2]] <- c('inf','sup')                        
IC.BF
#             inf        sup
#IC.BF.1 -1.777758 -0.2135749
#IC.BF.2 -1.043642  0.5243082    this one contains 0.

#(c)
# Dataset of the averages

av.acoruna <- (acoruna[,1] + acoruna[,2])/2
av.pontevedra <- (pontevedra[,1] + pontevedra[,2])/2

# My test: H0: mu(av.pontevedra) >= mu(av.acoruna) vs H1: H0^C
#          H0: mu(av.pontevedra) - mu(av.acoruna) >= 0 vs H1: H0^C

t1 <- av.acoruna
t2 <- av.pontevedra

# Gaussianity assumption:
shapiro.test(t1)
shapiro.test(t2)
# ok

t1.cov  <-  var(t1)
t2.cov  <-  var(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

t.test(t2,t1, alternative = 'greater', var.equal = TRUE)

# pvalue of the test is 0.9934, so I accept H0 and I can assess that there is statistical
# evidence to say that the evaluations of pontevedra is higher than the one in acoruna.


#########EX2
rm(list = ls())
fish <- read.table('fish.txt', header=T)
fish 
N=dim(fish)[1]
x11()
plot(fish[,1], fish[,2], pch = 16, col = (factor(fish$abundance)))
library(MASS)
mod1 <- lda(abundance ~ x + y, data = fish)
preds1 <- predict(mod1, fish)
table(fish[,3], preds1$class)

errors1 <- (preds1$class != fish[,3])
APER1  <- sum(errors1)/length(fish[,3])
APER1   #0.14

mod2 <- qda(abundance ~ x + y, data = fish)
preds2 <- predict(mod2, fish)
table(fish[,3], preds2$class)

errors2 <- (preds2$class != fish[,3])
APER2  <- sum(errors2)/length(fish[,3])
APER2  #0.096
#meglio qda

#normality hyp:
mcshapiro.test(fish[which(fish$abundance=='L'),1:2])$p    #0 ???
mcshapiro.test(fish[which(fish$abundance=='H'),1:2])$p   #0.1108
x11()
plot(fish[,1], fish[,2], col = (factor(fish$abundance)))
points(mod2$means, pch=4,col=c('black','red') , lwd=2, cex=1.5)

x  <- seq(min(fish[,1]), max(fish[,1]), length=200)
y  <- seq(min(fish[,2]), max(fish[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(mod2, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

mod2$prior
mod2$means
mod2$scaling
#(b)
s = qda(fish[,1:2], fish[,3], CV=T) # x, grouping

# Compute the estimate of the AER by leave-out-out cross-validation 
table(class.true=fish[,3], class.assignedCV=s$class)

errorCV <- (s$class != fish[,3])
errorCV

AERCV   <- sum(errorCV)/length(fish[,3])
AERCV    #0.1

# the prediction is good, even though the second gaussian assumption is 
# not true

#(c)
set.seed(19)
err = rep(1000, 21)

library(class)

for (k in 10:30) {
  fish.knn <- knn.cv(train = fish[,1:2], cl = fish[,3], k = k)
  
  errorCV <- (fish.knn != fish[,3])
  err[k]   <- sum(errorCV)/length(fish[,3])
}
min(err)
kbest=which.min(err)  
kbest   #13
best <- knn.cv(train = fish[,1:2], cl = fish[,3], k = kbest)
errorCV <- (best != fish[,3])
err_fin  <- sum(errorCV)/length(fish[,3])  #0.128
err_fin

final <- knn(train = fish[,1:2],test=xy, cl = fish[,3], k = kbest)
z  <- as.numeric(final)
x11()
plot(fish[,1], fish[,2], col=(factor(fish$abundance)))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)

kerr <- knn(train = fish[,1:2], test = fish[,1:2], cl = fish[,3], k = 5)
errs <- sum(as.numeric(kerr != fish[,3]))
APERknn <- errs/dim(fish)[1]
APERknn   #0.112
#(d)
# the best is the qda model smaller error
testset <- data.frame(x = 10.8, y= 39.4)   
knn(train = fish[,1:2], test = testset, cl = fish[,3], k=kbest)  #H  
predict(mod2, testset)  #L


#################EX3
rm(list=ls())
tattoo <- read.table('tattoo.txt', header=TRUE)
head(tattoo)

mod1 <- lm(price ~ method + dimension + dimension:method + ncolors  + ncolors:method , data = tattoo)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2
summary(mod1)$sigma
shapiro.test(mod1$residuals) # p-value = 0.2647 gaussiani

par(mfrow = c(2,2))
plot(mod1)
vif(mod1)
#dimension:method and method have a high value of vif that sugest there could be some sort of dependance
#(b)
library(car)
linearHypothesis(mod1, rbind(c(0,1,0,0,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0))   #2.2e-16  *** influisce
linearHypothesis(mod1, rbind(c(0,0,0,1,0,0),
                             c(0,0,0,0,0,1)), c(0,0))    #2.916e-11  ** influisce
#(c)
# i due set di regressori, elementi di ncolors e method, sono entrambi molto influenti sull'estensione del giardino
# tuttavia il modello completo a quattro regressori presenta forti collinearità come testimoniato dai vif molto alti

linearHypothesis(mod1, rbind(c(0,1,0,0,0,0),
                             c(0,0,0,0,0,1)), c(0,0))    #0.4734 i can assume ncolors:method = 0 and method = 0
mod2 <- lm(price ~ dimension + ncolors + dimension:method , data = tattoo)
summary(mod2)

mod2$coefficients
sigma2=sum(mod2$residuals^2)/mod2$df
sigma2

shapiro.test(mod2$residuals) # p-value = 0.172 gaussiani

par(mfrow = c(2,2))
plot(mod2) 
vif(mod2)    #ora i vif sono molto più bassi
#(d)
#only fixed costs
newdat1 <- data.frame(dimension = 0, ncolors = 0, method='handmade')
guess1 <- predict(mod2, newdat1, interval = 'confidence', level = 0.95)
guess1
#fit      lwr      upr
# 13.89011 5.898826 21.88139
newdat2 <- data.frame(dimension = 6.5, ncolors = 1, method='handmade')
guess2 <- predict(mod2, newdat2, interval = 'confidence', level = 0.95)
guess2
#fit     lwr      upr
# 118.5852 116.379 120.7914


##############EX4
rm(list=ls())
temperature <- read.table('temperature.txt', header=TRUE)
head(temperature)
library(fda)

nbase=21
x11()
matplot(t(temperature[,-366]), type = 'l')

data_W <- t(temperature[,-366])
abscissa <- 1:365

basis.1 <- create.fourier.basis(rangeval=c(0,365),nbasis=nbase)
data_W.fd.1 <- Data2fd(y = data_W,argvals = abscissa,basisobj = basis.1)
x11()
plot.fd(data_W.fd.1)

data_W.fd.1$coefs[1:3,1:2]
#        Station1  Station2
# const 468.06033 493.55892
#sin1  -95.36283 -89.80744
#cos1  -46.39331 -41.68439

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

pca_W.1$values[1:5]
# 1868.863549  279.04nbase90   27.987432    3.556465    1.482923

# scree plot
x11()
plot(pca_W.1$values[1:nbase],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:nbase]/sum(pca_W.1$values),xlab='j',ylab='CPV')

cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)
#0.9992827
# first three FPCs
x11()
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

# la prima componente principale ci mostra delle misurazioni ove la temperatura è bassa nei primi tre mesi
# dell'anno, e che la vede risalire fino ad un massimo attorno ad ottobre
# la seconda, al contrario, ci mostra un andamento opposto con un minimo di temperature raggiunte attorno a 
# luglio ed un massimo tra febbraio e marzo
x11()
par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)

x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2], col = ifelse((temperature[,366])=='Italy','blue','black'), pch = 16, xlab = 'PC1', ylab = 'PC2')
legend('topleft',c('Italy', 'Germany'), col = c('blue','black'), pch = 16)


