##################EX1
rm(list=ls())
shopping <- read.table('shopping.txt', header=TRUE)
shopping
#(a)
n=dim(shopping)[1]
x.mean   <- colMeans(shopping)
x.cov    <- cov(shopping)
# Center:
x.mean

# Directions of the principal axes:
eigen(x.cov/n)$vectors

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov/n)$values) 
#H0:X ~ N_p()    X1: H0^c
mcshapiro.test(shopping)  #$pvalue 0.6636
#(b)
shopping$totpur=shopping$men+shopping$women
n <- dim(shopping)[1]
p <- dim(shopping)[2]  

shopping.mean   <- sapply(shopping,mean)
shopping.cov    <- cov(shopping)

alpha   <- .05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
T2={}
for(i in 1:p){
  IC.T2=c( shopping.mean[i]-sqrt(cfr.fisher*shopping.cov[i,i]/n) , shopping.mean[i], shopping.mean[i]+sqrt(cfr.fisher*shopping.cov[i,i]/n) )
  T2=rbind(T2,IC.T2)
}
dimnames(T2)[[2]] <- c('inf','center','sup')
T2

#(c)
D1=shopping$totpur-0.2*shopping$accesses
D1
shapiro.test(D1) #p-value = 0.6847
t.test(D1,conf.level = 0.95,alternative = "greater")  #p-value = 0.01061 , p-value = 0.005305 con greater

##########EX2
rm(list=ls())
rice <- read.table('rice.txt', header=TRUE)
rice
x11()
plot(rice, pch=16)

Distance=dist(rice, method='euclidean')
gruppi <- hclust(Distance, method='complete')
x11()
plot(gruppi, hang=-0.1, sub='', xlab='', labels=F)
rect.hclust(gruppi, k=3)
# cut in two groups
cluster.ec <- cutree(gruppi, k=3)

table(cluster.ec)
mu1=sapply(rice[cluster.ec==1,],mean)
mu2=sapply(rice[cluster.ec==2,],mean)
mu3=sapply(rice[cluster.ec==3,],mean)
x11()
plot(rice, pch=16, col = as.vector(cluster.ec)+1)
points(mu1[1], mu1[2], pch = 4, cex = 1.5, lwd = 2 , col=2)
points(mu2[1], mu2[2], pch = 4, cex = 1.5, lwd = 2 , col=3)
points(mu3[1], mu3[2], pch = 4, cex = 1.5, lwd = 2 , col=4)

#problemi: dimensioni diverse, ed evidente separazione in due zone nel gruppo più grande

#veloce prova con single:
gruppi.s <- hclust(Distance, method='single')
x11()
plot(gruppi.s, hang=-0.1, sub='', xlab='', labels=F)
rect.hclust(gruppi.s, k=3)
# cut in two groups
cluster.es <- cutree(gruppi.s, k=3)
table(cluster.es)
mu1=sapply(rice[cluster.es==1,],mean)
mu2=sapply(rice[cluster.es==2,],mean)
mu3=sapply(rice[cluster.es==3,],mean)
x11()
plot(rice, pch=16, col = as.vector(cluster.es)+1)
points(mu1[1], mu1[2], pch = 4, cex = 1.5, lwd = 2 , col=2)
points(mu2[1], mu2[2], pch = 4, cex = 1.5, lwd = 2 , col=3)
points(mu3[1], mu3[2], pch = 4, cex = 1.5, lwd = 2 , col=4)


# average metodo:
gruppi.a <- hclust(Distance, method='average')
x11()
plot(gruppi.a, hang=-0.1, sub='', xlab='', labels=F)
rect.hclust(gruppi.a, k=3)
# cut in two groups
cluster.ea <- cutree(gruppi.a, k=3)

table(cluster.ea)
mu1=sapply(rice[cluster.ea==1,],mean)
mu2=sapply(rice[cluster.ea==2,],mean)
mu3=sapply(rice[cluster.ea==3,],mean)
means={}
means=rbind(mu1,mu2,mu3)
x11()
plot(rice, pch=16, col = as.vector(cluster.ea)+1)
points(mu1[1], mu1[2], pch = 4, cex = 1.5, lwd = 2 , col=2)
points(mu2[1], mu2[2], pch = 4, cex = 1.5, lwd = 2 , col=3)
points(mu3[1], mu3[2], pch = 4, cex = 1.5, lwd = 2 , col=4)

# average e single sono uguali
#(c)
g=3
alpha <- 0.05
k=2*g
IC={}
Ps={}
for(i in 1:g){
  X=rice[cluster.es==i,1] #only need 1
  n=length(X)
  Ps=c(Ps,shapiro.test(X)$p)
  x.mean   <- mean(X)
  x.cov    <- var(X)
  
  ICmean <- c(inf=x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
              center= x.mean,
              sup= x.mean + sqrt((x.cov)/n) * qt(1 - alpha/(2*k), n-1))
  
  ICvar <- c(inf=(x.cov)*(n-1) / qchisq(1 - alpha/(2*k), n-1),center=(x.cov),sup=(x.cov)*(n-1) / qchisq(alpha/(2*k), n-1))
  
  IC=rbind(IC,ICmean,ICvar)
}
Ps
IC
##############EX3
rm(list=ls())
sli <- read.table('landslides.txt', header=TRUE)
sli
mod1 <- lm(rate ~ rain  + hardness  + coarse + fine , data = sli)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$garden
sigma2

shapiro.test(mod1$residuals) # p-value = 0.3607 gaussiani

par(mfrow = c(2,2))
plot(mod1) #ok
vif(mod1)
#(b)
mod2 <- lm(rate ~ rain  + coarse + fine , data = sli)
summary(mod2)

mod2$coefficients
sigma2=sum(mod2$residuals^2)/mod2$garden
sigma2

shapiro.test(mod2$residuals) # p-value = 0.359 gaussiani

par(mfrow = c(2,2))
plot(mod2)   #ok
#(c)
library(car)
linearHypothesis(mod2,c(0,0,1,-2), 0)   #0.9836 non posso rifiutare coarse - 2 fine = 0
mod3 <- lm(rate ~ rain + fine, data = sli)
summary(mod3)
#(d)
alpha=0.01
Z0   <- data.frame(rain=700, coarse=10, fine=8)
ICBmean <- predict(mod2, Z0, interval='confidence',level=1-alpha) 
ICBmean

Z0   <- data.frame(rain=700, fine=8)
ICBmean <- predict(mod3, Z0, interval='confidence',level=1-alpha) 
ICBmean
###################EX4
rm(list=ls())
hotels <- read.table('hotels.txt',header=TRUE)
attach(hotels)
car <- read.table('hotels.txt',header=TRUE)

# create dummy: 0 = urban, 1 = vegetation
DUMMY <- rep(0,length(winter))
DUMMY[which(winter=='yes')] <- 1
hotels <- data.frame(cbind(x,y,DUMMY,distance,price))
names(hotels) <- c('x','y','winter','distance','price')
coordinates(hotels) <- c('x','y')



v <- variogram(price ~ 1, data = hotels)
plot(v)
v.fit1 <- fit.variogram(v, vgm(5000, "Sph", 500))
plot(v, v.fit1, pch = 3)
v.fit1

g.tr <- gstat(formula = price ~ 1, data = hotels, model = v.fit1)
predict(g.tr, hotels[1,], BLUE = TRUE) #a0

#second model 
v2 <- variogram(price ~ distance + winter + winter:distance, data = hotels)
plot(v2)
v.fit2 <- fit.variogram(v2, vgm(1000, "Sph", 500))
plot(v2, v.fit2, pch = 3)
v.fit2

hotels

hot.gstat <- gstat(id = 'price', formula = price ~ distance + winter + winter:distance,
                   data = hotels, nmax = 100, model=v.fit2, set = list(gls=1))
hot.gstat

# Estimate the variogram from GLS residuals:
?variogram.gstat
v.gls<-variogram(hot.gstat)
plot(v.gls)

v.gls.fit <- fit.variogram(v.gls, vgm(1000, "Sph", 500))
plot(v.gls, v.gls.fit, pch = 19)

# Update gstat object with variogram model
hot.gstat <- gstat(id = 'price', formula = price ~ distance + winter + winter:distance,
                   data = hotels, nmax = 100, model=v.gls.fit, set = list(gls=1))
hot.gstat

predict(hot.gstat, hotels[2,], BLUE = TRUE)
predict(hot.gstat, hotels[1,], BLUE = TRUE)

a1 = (predict(hot.gstat, hotels[2,], BLUE = TRUE)$price.pred - predict(hot.gstat, hotels[1,], BLUE = TRUE)$price.pred)/(car[2,4]-car[1,4])
a0= predict(hot.gstat, hotels[2,], BLUE = TRUE)$price.pred - a1*car[2,4] #use original table

a12 = (predict(hot.gstat, hotels[5,], BLUE = TRUE)$price.pred - predict(hot.gstat, hotels[3,], BLUE = TRUE)$price.pred)/(car[5,4]-car[3,4])
a02= predict(hot.gstat, hotels[5,], BLUE = TRUE)$price.pred - a1*car[5,4] #use original table

s0.new <- as.data.frame(matrix(c(342399.74,5072272.75,1,248.29),1,4))
names(s0.new) <- c('x','y','winter','distance')
coordinates(s0.new) <- c('x','y')
predict(hot.gstat, s0.new)
#this is price per single night - non stationary model 

sf =c(342362.58 , 5072518.24) #center
s0 = c(342399.74 , 5072272.75) #our pos
d=sqrt((sf[1]-s0[1])^2+(sf[2]-s0[2])^2)

newdat <- data.frame(x = s0[1], y = s0[2],distance=d ,DUMMY = 1)
coordinates(newdat) <- c('x','y')
guess <- predict(g.tr, newdat, BLUE = TRUE)
guess
# prediction = 429.8245
# variance of prediction = 243.2858
