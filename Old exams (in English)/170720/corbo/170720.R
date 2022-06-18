###########EX1
rm(list=ls())
occupancy <- read.table('occupancy.txt', header=T)
occupancy
priorp=c(1-9/24,9/24)
qda.occu <- qda(occupancy[,1:2], occupancy[,3],prior=priorp)
qda.occu

Qda.occu <- predict(qda.occu, occupancy[,1:2])

# compute the APER
Qda.occu$class
table(assigned=Qda.occu$class,true=occupancy[,3])

Aper=1*priorp[1]/40+5*priorp[2]/60
errorsq <- (Qda.occu$class != occupancy[,3])
errorsq

APERq   <- sum(errorsq)/length(occupancy[,3])
APERq   #0.06
###########LDA
lda.occu <- lda(occupancy[,1:2], occupancy[,3],prior=priorp)
lda.occu

Lda.occu <- predict(lda.occu, occupancy[,1:2])

# compute the APER
Lda.occu$class
table(Lda.occu$class, occupancy[,3])

Aper=1*priorp[1]/40+9*priorp[2]/60 

errorsl <- (Lda.occu$class != occupancy[,3])
errorsl

APERl   <- sum(errorsl)/length(occupancy[,3])
APERl   #0.1
#scelgo qda
#assumptions:normality in groups
occupied=occupancy[which(occupancy$X==1),1:2]
free=occupancy[which(occupancy$X==0),1:2]

mcshapiro.test(occupied)$p #0.4208
mcshapiro.test(free)$p   #0.8624

x11()
plot(occupancy[,1], occupancy[,2], col = (factor(occupancy$X)))
points(qda.occu$means, pch=4,col=c('black','red') , lwd=2, cex=1.5)

x  <- seq(min(occupancy[,1]), max(occupancy[,1]), length=200)
y  <- seq(min(occupancy[,2]), max(occupancy[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(qda.occu, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

newdatum <- data.frame(Humidity = 26, CO2 = 9)
predict(lda.occu, newdatum)   #1
#(d)
library(class)
final <- knn(train = occupancy[,1:2],test=xy, cl = occupancy[,3], k = 5)
z  <- as.numeric(final)
x11()
plot(occupancy[,1], occupancy[,2], col=(factor(occupancy$X)))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)

kerr <- knn(train = occupancy[,1:2], test = occupancy[,1:2], cl = occupancy[,3], k = 5)
errs <- sum(as.numeric(kerr != occupancy[,3]))
APERknn <- errs/dim(occupancy)[1]
APERknn   #0.06
###########EX2
rm(list=ls())
purity <- read.table('purity.txt', header=T)
purity 
D=purity[1:8,]-purity[9:16,]
D



###################EX3
rm(list=ls())
toxicity <- read.table('toxicity.txt', header=TRUE)
head(toxicity)

mod1 <- lm(tox ~ . , data = toxicity)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2

shapiro.test(mod1$residuals) # p-value = 0.1866 gaussiani

par(mfrow = c(2,2))
plot(mod1)
library(car)
vif(mod1)
Z0   <- data.frame(C1=100,C2=0.7,C3=2,C4=4,C5=1.4,C6=3)
alpha=0.05
IC <- predict(mod1, Z0, interval='confidence',level=1-alpha) 
IC
#fit      lwr      upr
# 37.40647 35.14482 39.668

lambda.grid=10^seq(-2,0,length=50)
library(glmnet)
x <- model.matrix(tox ~ . , data = toxicity)[,-1] # matrix of predictors
y <- toxicity$tox # vector of response
fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha=1) # alpha=1 -> lasso 
x11()
plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

norm_l1 <- NULL
for(i in 1:50)
  norm_l1 <- c(norm_l1,sum(abs(fit.lasso$beta[,i])))
X11()
plot(log(lambda.grid),norm_l1)

# Let's set lambda via CV
set.seed(1)
cv.lasso <- cv.glmnet(x,y,alpha=1,lambda=lambda.grid)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
X11()
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:5,] # p+1(intercetta)-1(y)=p
coef.lasso 
X11()
plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
abline(v=log(bestlam.lasso))

fit.lasso.best <- glmnet(x,y, lambda = bestlam.lasso, alpha=1) # alpha=1 -> lasso 

predict(fit.lasso.best,s=bestlam.lasso,as.matrix(Z0), type="class")   #36.97695

###################EX4
rm(list=ls())
traffic <- read.table('traffic.txt', header=TRUE)
head(traffic)

day1=t(as.matrix(traffic[1,]))
abscissa=1:24
x11()
plot(abscissa,day1)

m=4
nbase=15
basis <- create.bspline.basis(c(1,24), nbase, m)

x11()
matplot(t(traffic), type = 'l')
# da questo grafico preliminare mi sembra di intravedere tra distinti andamenti tra le mie stazioni

data_W <- t(traffic)
abscissa <- 1:24

data_W.fd.1 <- Data2fd(y = data_W,argvals = abscissa,basisobj = basis)
x11()
plot.fd(data_W.fd.1)

data_W.fd.1$coefs[1:3,1:2]
#          Day1     Day2
#bspl3.1 1184.9116 819.4833
#bspl3.2  488.3949 400.0216
#bspl3.3  349.9349 326.6408

#(b)
pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=29 are non-null perchè i viene 15???  semnrerebbe nbase
plot(pca_W.1$values[1:nbase],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:nbase]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))
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
# variation in amplitude (more in winter than in summer)

plot(media,lwd=2 ,ylab='temperature',main='FPC2')
lines(media+pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=2)
lines(media-pca_W.1$harmonics[2,]*sqrt(pca_W.1$values[2]), col=3)
# temperate climate or not

plot(media,lwd=2 ,ylab='temperature',main='FPC3')
lines(media+pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=2)
lines(media-pca_W.1$harmonics[3,]*sqrt(pca_W.1$values[3]), col=3)

# scatter plot of the scores on FPC1 wrt days
days=1:30
x11()
plot(days,pca_W.1$scores[,1],xlab="Days",ylab="Scores FPC1",lwd=2)

#il weekend ziopera c'è meno traffico

