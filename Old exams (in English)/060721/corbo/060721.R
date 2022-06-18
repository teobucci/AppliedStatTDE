#################EX1
rm(list=ls())
chicca <- read.table('chicca.txt', header=T)
head(chicca)

n <- dim(chicca)[1]
p <- dim(chicca)[2]
center <- c(0, 90)

alpha <- .01

x.mean   <- colMeans(chicca)
x.cov    <- cov(chicca)
x.invcov <- solve(x.cov)

x.T2       <- n * (x.mean-center) %*% x.invcov %*% (x.mean-center) 

cfr.orthoer <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
x.T2 < cfr.orthoer
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P #H0  4.304335e-13

eigen(x.cov)$vectors

# Length of the semi-axes of the ellipse:
cfr.chisq <- qchisq(1-alpha,p)
r <- sqrt(cfr.chisq)
r*sqrt(eigen(x.cov)$values) 

library(car)
x11()
plot(chicca)
ellipse(center, shape=x.cov/n, sqrt(cfr.chisq), col = 'blue', lty = 2, lwd=2)
points(x.mean, pch = 4, lwd = 2, col ='red')
k=p
cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF1 <- c( x.mean[1]-cfr.t*sqrt(x.cov[1,1]/n) , x.mean[1], x.mean[1]+cfr.t*sqrt(x.cov[1,1]/n) )
IC.BF2 <- c( x.mean[2]-cfr.t*sqrt(x.cov[2,2]/n) , x.mean[2], x.mean[2]+cfr.t*sqrt(x.cov[2,2]/n) )

Bf <- rbind(IC.BF1, IC.BF2)
dimnames(Bf)[[2]] <- c('inf','center','sup')
Bf

total=chicca$delay+chicca$stay
t.test(total, mu = 90)   #p-value = 7.453e-10

##############EX2
rm(list=ls())
ortho <- read.table('orthopaedics.txt', header=TRUE)
head(ortho)
n <- dim(ortho)[1]
library(MASS)
priorp=c(0.35,0.65)
qda.ort <- qda(ortho[,1:2], ortho[,3], prior = priorp)
qda.ort

pred <- predict(qda.ort, ortho[,1:2])
table(pred$class, ortho[,3])

x11()
plot(ortho[,1], ortho[,2], col = (factor(ortho$norm_abnorm)))
points(qda.ort$means, pch=4,col=c('black','red') , lwd=2, cex=1.5)

x  <- seq(min(ortho[,1]), max(ortho[,1]), length=200)
y  <- seq(min(ortho[,2]), max(ortho[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(qda.ort, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

classCV <- qda(ortho[,1:2], ortho[,3], CV = TRUE, prior=priorp)
table(ortho[,3]$class, ortho[,3])
#   AB NO
#AB 38  8
#NO 32 72

table(class.true=ortho[,3], class.assignedCV=classCV$class)

errors <- (classCV$class != ortho[,3])
APER.qdaCV <- sum(errors)/n

APER=32*0.35/70+8*0.65/80
APER
# [1] 0.2666667
newdatum <- data.frame(incidence = 60, tilt = 0)
np=predict(qda.ort, newdatum)
np  #NO
#(d)
library(e1071)
y <- rep(0,n)
y[which(ortho$norm_abnorm=='NO')] <- 1
x11()
plot(ortho[,1], ortho[,2],pch=20, col=as.character(y+1))

dat <- data.frame(x=ortho[,c(2,1)], y=as.factor (y))
svmfit <- svm(y~., data=dat , kernel ='linear', cost =0.1, scale =FALSE )
summary(svmfit)

x11()
par(mfrow=c(1,2))
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19)

newdatum <- data.frame(x.incidence = 60, x.tilt = 0)
ypred <- predict(svmfit,newdatum)

dat.te <- data.frame(x.incidence = 60, x.tilt = 0, y=as.factor (np$class))
pred.te <- predict (svmfit , newdata =dat.te)
pred.te
table(pred.te , dat.te$y)

ypred = predict(svmfit,dat)
table(true.label=dat$y, assigned.label =ypred )
APER.svm<- (27+17)/n  #0.2933333
APER.svm
#################EX3
pc <- read.table('pc.txt', header=TRUE)
pc
dummyMac=ifelse(pc$OS=='Mac',1,0)
dummyLinux=ifelse(pc$OS=='Linux',1,0)

mod1 <- lm(price   ~ freq  + cache_acc + freq:dummyMac + cache_acc:dummyMac + freq:dummyLinux + cache_acc:dummyLinux, data = pc)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2

shapiro.test(mod1$residuals) # p-value = 0.1274 gaussiani

par(mfrow = c(2,2))
plot(mod1) 
#(b)
library(car)
linearHypothesis(mod1, rbind(c(0,1,0,0,0,0,0),
                             c(0,0,1,0,0,0,0),
                             c(0,0,0,1,0,0,0),
                             c(0,0,0,0,1,0,0),
                             c(0,0,0,0,0,1,0),
                             c(0,0,0,0,0,0,1)), c(0,0,0,0,0,0))   #1.903e-08 *** influisce
#(c)
#Hypothesis:
#cache_acc = 0
#cache_acc:dummyMac = 0
#cache_acc:dummyLinux = 0
linearHypothesis(mod1, rbind(c(0,0,1,0,0,0,0),
                             c(0,0,0,0,1,0,0),
                             c(0,0,0,0,0,0,1)), c(0,0,0))    #0.3696 non influisce
#(d)
mod2 <- lm(price   ~ freq  + freq:dummyMac +  freq:dummyLinux, data = pc)
summary(mod2)
mod3 <- lm(price   ~ freq  + freq:dummyMac, data = pc)
summary(mod3)

mod3$coefficients
sigma2=sum(mod3$residuals^2)/mod3$df
sigma2

shapiro.test(mod3$residuals) # p-value = 0.1966 gaussiani

par(mfrow = c(2,2))
plot(mod3) 
#(e)
newdat <- data.frame(freq = 3.2, dummyMac=0)
guess <- predict(mod3, newdat, interval = 'confidence', level = 0.90)
guess

newdat <- data.frame(freq = 3.2, dummyMac=0,dummyLinux=0)
guess <- predict(mod2, newdat, interval = 'confidence', level = 0.90)
guess


###############EX4
rm(list=ls())
colours <- read.table('colours.txt', header=TRUE)
head(colours)
library(sp)
attach(colours)
coordinates(colours) <- c('x','y')
car <- read.table('colours.txt',header=TRUE)
#a
v <- variogram(revenue ~ 1, data = colours)
plot(v)
v.fit1 <- fit.variogram(v, vgm(40, "Sph",500))
plot(v, v.fit1, pch = 3)
v.fit1

colo.gstat <- gstat(formula = revenue ~ 1,
                    data = colours, nmax = 100, model=v.fit1, set = list(gls=1))
colo.gstat

predict(colo.gstat, colours[1,], BLUE = TRUE)  #a0 12.18036  #[using ordinary kriging]
#b
v1 <- variogram(revenue ~ colour, data = colours)
plot(v1)
v1.fit1 <- fit.variogram(v1, vgm(5,"Sph",1500))
plot(v1, v1.fit1, pch = 3)
v1.fit1
#model      psill    range1
#   Nug   0.000    0.000
#   Sph 14.15398  1054.988


colo.gstat <- gstat(formula = revenue ~ colour,
                   data = colours, nmax = 100, model=v1.fit1, set = list(gls=1))
colo.gstat

predict(colo.gstat, colours[1,], BLUE = TRUE)  #a01 9.2311  [using universal kriging]
predict(colo.gstat, colours[2,], BLUE = TRUE)  #a02 24.22476  
predict(colo.gstat, colours[3,], BLUE = TRUE)  #a03 9.007159  

#(d)
s0 = c(514811.55, 5037308.54) 
g.tr <- gstat(formula = revenue ~ colour, data = colours, model = v1.fit1)
newdat0 <- data.frame(x = s0[1], y = s0[2], colour = 'red')
coordinates(newdat0) <- c('x','y')
guess <- predict(g.tr, newdat0)
guess
# revenue prediction = 10.48128  
newdat1 <- data.frame(x = s0[1], y = s0[2], colour = 'yellow')
coordinates(newdat1) <- c('x','y')
guess <- predict(g.tr, newdat1)
guess
# revenue prediction = 25.47494  
newdat2 <- data.frame(x = s0[1], y = s0[2], colour = 'orange')
coordinates(newdat2) <- c('x','y')
guess <- predict(g.tr, newdat2)
guess
# revenue prediction = 10.25734 
