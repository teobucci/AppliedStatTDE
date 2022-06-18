#############EX1
wine <- read.table('wine.txt', header=T)
wine 
attach(wine)
### question a)
# verify assumptions (on the last model):
# 1) normality
Ps={}
for(i in 1:3)
  for(j in 1:2)
    Ps <- c(Ps,shapiro.test(alcohol[ region==levels(factor(region))[i] ][ color==levels(factor(color))[j] ])$p)
Ps

# 2) homogeneity of variances
bartlett.test(alcohol, region)
bartlett.test(alcohol,color)

# Model with interaction (complete model): 
# X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect color), j=1,2,3 (effect region)
fit1 <- aov(alcohol ~ color + region + region:color, wine) 
summary(fit1)

# Model without interaction (additive model): 
# X.ijk = mu + tau.i + beta.j + eps.ijk; eps.ijk~N(0,sigma^2), 
#     i=1,2 (effect color), j=1,2,3 (effect region)
fit2 <- aov(alcohol ~ color + region, wine) 
summary(fit2)

# Reduced model (one-way): 
# X.jk = mu + tau.i  + eps.ijk; eps.jk~N(0,sigma^2), 
#     i=1,2 (effect color)
fit3 <- aov(alcohol ~ color, wine) 
summary(fit3)


#(c)
DF <- fit3$df # n*g*b - 1 - (g-1) 
Spooled <- sum(fit3$res^2)/DF

means <- as.vector(tapply(alcohol, color, mean))
names(means) <- levels(color)
means

alpha <- 0.01
k     <- 3 # g + 1 = 3 (g Conf Int for the means and 1 for the variance)
qT <- qt(1-alpha/(2*k), DF)
qCinf <- qchisq(1 - alpha / (2*k), DF)
qCsup <- qchisq(alpha / (2*k), DF)
ng <- c(length(which(color==levels(factor(color))[1])),length(which(color==levels(factor(color))[2])))

BF    <- rbind(cbind(inf=means - sqrt(as.vector(Spooled) / ng) * qT,
                     sup=means + sqrt(as.vector(Spooled) / ng) * qT),
               c(inf=Spooled * DF / qCinf,
                 sup=Spooled * DF / qCsup))
rownames(BF)[3] <- 'Var.'
BF

detach(wine)

##################EX2
rm(list=ls())
activity <- read.table('activity.txt', header=TRUE)
activity

priorp=c(1/8,1/2,3/8)
qda.act <- qda(activity[,1:2], activity[,3],prior=priorp)
qda.act

Qda.act <- predict(qda.act, activity[,1:2])

# compute the APER
Qda.act$class
table(Qda.act$class, activity[,3])

errorsq <- (Qda.act$class != activity[,3])
errorsq

APERq   <- sum(errorsq)/length(activity[,3])
APERq   #0.04666667
###########LDA
lda.act <- lda(activity[,1:2], activity[,3],prior=priorp)
lda.act

Lda.act <- predict(lda.act, activity[,1:2])

# compute the APER
Lda.act$class
table(Lda.act$class, activity[,3])

errorsl <- (Lda.act$class != activity[,3])
errorsl

APERl   <- sum(errorsl)/length(activity[,3])
APERl   #0.1177778
#scelgo qda
#assumptions:normality in groups
groups=levels(factor(activity$activity))
Ps={}
for(i in 1:3){
  Ps=c(Ps,mcshapiro.test(activity[which(activity$activity==groups[i]),1:2])$p)
}
Ps

x11()
plot(activity[,1], activity[,2], col = (factor(activity$activity)))
points(qda.act$means, pch=4,col=c('black','red','green') , lwd=2, cex=1.5)

x  <- seq(min(activity[,1]), max(activity[,1]), length=200)
y  <- seq(min(activity[,2]), max(activity[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(qda.iris, xy)$post  
z1 <- z[,1] - pmax(z[,2], z[,3])    
z2 <- z[,2] - pmax(z[,1], z[,3])    
z3 <- z[,3] - pmax(z[,1], z[,2])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)

newdatum <- data.frame(accel = 0.45, gyro = 0.52)
predict(lda.act, newdatum)   #sitting
#(d)
library(class)
Knn.act <- knn.cv(train = activity[,1:2], cl = activity[,3], k = 5)
error <- (Knn.act != activity[,3])
err_fin  <- sum(error)/length(activity[,3])  #0.05111111
err_fin
final <- knn(train = activity[,1:2],test=xy, cl = activity[,3], k = 5)
z  <- as.numeric(final)
x11()
plot(activity[,1], activity[,2], col=(factor(activity$activity)))
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)

#################EX3
bikes <- read.table('bikes.txt', header=TRUE)
bikes
dummy=ifelse(bikes$day=='Holiday',1,0)

mod1 <- lm(bike_count  ~ mean_temp   + mean_wind  + dummy + mean_temp:dummy + mean_wind:dummy , data = bikes)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2

shapiro.test(mod1$residuals) # p-value = 0.2133 gaussiani

par(mfrow = c(2,2))
plot(mod1) 
#(b)
library(car)
linearHypothesis(mod1, rbind(c(0,1,0,0,0,0),
                             c(0,0,1,0,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0,0))   #0.0002863  *** influisce
linearHypothesis(mod1, rbind(c(0,0,0,1,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0))    #0.009922  ** influisce
#(c)
linearHypothesis(mod1, rbind(c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0))    #0.6996 i can assume mean_temp:dummy = 0 and mean_wind:dummy = 0
mod2 <- lm(bike_count  ~ mean_temp   + mean_wind  + dummy , data = bikes)
summary(mod2)
mod3 <- lm(bike_count  ~ mean_temp  + dummy , data = bikes)
summary(mod3)

mod3$coefficients
sigma2=sum(mod3$residuals^2)/mod3$df
sigma2

shapiro.test(mod3$residuals) # p-value = 0.6076 gaussiani

par(mfrow = c(2,2))
plot(mod3) 

#(d)
newdat <- data.frame(mean_temp = 2, mean_wind = 3, dummy=1)
guess <- predict(mod3, newdat, interval = 'confidence', level = 0.95)
guess


###################EX4
rm(list=ls())
spectra <- read.table('spectra.txt', header=TRUE)
head(spectra)
spe1=t(as.matrix(spectra[1,]))
abscissa=1:80
x11()
plot(abscissa,spe1)

m=4
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,80), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, spe1, basis)$gcv
}
x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbase=nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)

basis <- create.bspline.basis(c(0,80), nbase, m)

data_W.fd.1 <- Data2fd(y = spe1,argvals = abscissa,basisobj = basis)
data_W.fd.1$coefs[1:3,]
#bspl4.1     bspl4.2     bspl4.3 
#0.15801359 -0.09183357  0.23146743 
#(b)
data_W=t(spectra)
data_W.fd.1 <- Data2fd(y = data_W,argvals = abscissa,basisobj = basis)
x11()
plot.fd(data_W.fd.1)

#(c)
library(fdakma)

set.seed(1)
fdakma_example <- kma(
  x=abscissa, y0=t(data_W), n.clust = 3, 
  warping.method = 'affine', 
  similarity.method = 'd0.pearson',  # similarity computed as the cosine
                                     # between the curves 
                                      # (correlation)
  center.method = 'k-means'
  #seeds = c(1,21) # you can give a little help to the algorithm...
)

kma.show.results(fdakma_example)
