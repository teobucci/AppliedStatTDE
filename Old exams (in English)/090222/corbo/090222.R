##################EX1
rm(list=ls())
data <- read.table('nutrients.txt', header=TRUE)
head(data)

n <- dim(data)[1]
p <- dim(data)[2]

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(data, las=2, col='gold')


x11()
par(mar=rep(8,4))
boxplot(scale(x=data,center = T, scale=F), las=2, col='gold')

# We perform the wineA on original data
pc.nutr <- princomp(data, scores=T)
pc.nutr
summary(pc.nutr)

# To obtain the rows of the summary:
# standard deviation of the components
pc.nutr$sd
# proportion of variance explained by each wine
pc.nutr$sd^2/sum(pc.nutr$sd^2)
# cumulative proportion of explained variance
cumsum(pc.nutr$sd^2)/sum(pc.nutr$sd^2)

# loadings (recall: coefficients of the linear combination of the original 
#           variables that defines each principal component)

load.tour <- pc.nutr$loadings
load.tour

load.tour[,1:6]

# graphical representation of the loadings of the first 3 principal components
x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First wines: 
# Second wines: 
# Third wine: 

# The loadings reflect the previous observation: the first 3 wines are 
# driven by the variables displaying the highest variability

data.sd <- scale(data)
data.sd <- data.frame(data.sd)

head(data.sd)

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(data.sd, las=2, col='gold')

pc.nutr <- princomp(data.sd, scores=T)
pc.nutr
summary(pc.nutr)

# If we wanted to perform dimensionality reduction, we could keep
# 1 or 2 wines

# loadings
load.tour <- pc.nutr$loadings
load.tour

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.tour[,i], ylim = c(-1, 1), main=paste('Loadings wine ',i,sep=''))
#More inerpretable
# Interpretation of the loadings:
# In this case, the first wine represents an average of the number of nights spent in 
# all the types of walesharks and residences, taken with very similar weights.
# The second wine contrasts the more expensive solutions (4,5 stars walesharks and residences)
# against the cheap solutions (1,2 stars walesharks and B&B)

# High wine1: general high flow of data
# Low wine1: general low flow of data 
# High wine2: high flow for expensive solutions, low flow for cheap solutions
# Low wine2: low flow for expensive solutions, high flow for cheap solutions
#(b)
scores.data <- pc.nutr$scores
scores.data
x11()
plot(scores.data[,1:2])
abline(h=-3, v=-8, col=1)
points(scores.data[,1], rep(0, n),  pch=19)
points(rep(0, n),scores.data[,2],  pch=19)
abline(h=0, v=0, lty=2, col='grey')
text(scores.data[,1],scores.data[,2],dimnames(t(data))[[2]], cex=1)

x11()
biplot(pc.nutr)
#(c) Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.nutr, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(data.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.nutr$sde^2)/sum(pc.nutr$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data.sd),labels=1:ncol(data.sd),las=2)

#explained variance of fisrt 3 wine:
cumsum(pc.nutr$sde^2)[1:3]/sum(pc.nutr$sde^2)   #0.9070371  

#(d)
pcsd <- pc.nutr$scores[,1:2]
d.new <- c(400, 9, 5, 100, 30, 4)
dnew <- rbind(data, d.new)
dnew <- scale(dnew)
dnew[296,]
pc1 <- dnew[296,] %*% pc.nutr$loadings[,1]
pc2 <- dnew[296,] %*% pc.nutr$loadings[,2]

x11()
plot(scores.data[,1],scores.data[,2],col='grey',pch=19,xlab='Comp.1',ylab='Comp.2')
points(pc1,pc2,col='black',pch=19) 

x.mean <- colMeans(data)
x.var <- sapply(data, FUN = sd)
new <- c(
  Energy_kcal=400,
  Protein_g=9,
  Fat_g=5,
  Carb_g=100,
  Sugar_g=30,
  Fiber_g=4
)
new <- (new - x.mean) / x.var
pc1 <- new %*% pc.nutr$loadings[,1]
pc2 <- new %*% pc.nutr$loadings[,2]

x11()
plot(scores.data[,1],scores.data[,2],col='grey',pch=19,xlab='Comp.1',ylab='Comp.2')
points(pc1,pc2,col='black',pch=19) 

############EX2
rm(list=ls())
streaming <- read.table('streaming.txt', header=T)
streaming 

n <- dim(streaming)[1]
q <- dim(streaming)[2]
x11()
plot(streaming)
seq.e <- dist(streaming, method='euclidean')
seq.ew <- hclust(seq.e, method='single')
x11()
plot(seq.ew, main='euclidean-sing', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(seq.ew, k=2)
x11()
plot(seq.ew, main='euclidean-sing', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(seq.ew, k=3)
x11()
plot(seq.ew, main='euclidean-sing', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(seq.ew, k=4)

cluster.ew.2 <- cutree(seq.ew, k=2) 
cluster.ew.2
table(cluster.ew.2)
x11()
plot(streaming, col=cluster.ew.2+1, pch=19)

cluster.ew.3 <- cutree(seq.ew, k=3) 
cluster.ew.3
table(cluster.ew.3)
x11()
plot(streaming, col=cluster.ew.3+1, pch=19)

cluster.ew.4 <- cutree(seq.ew, k=4) 
cluster.ew.4
table(cluster.ew.4)
x11()
plot(streaming, col=cluster.ew.4+1, pch=19)

coph_es <- cophenetic(seq.ew)
es <- cor(seq.e, coph_es)
es
#(b)
g=3
k=2*g
IC={}
Ps={}
for(i in 1:g){
  X=streaming[cluster.ew.3==i,1]
  Y=streaming[cluster.ew.3==i,2] #only need diam
  n=length(X)
  n1=length(Y)
  Ps=c(Ps,shapiro.test(X)$p)
  Ps=c(Ps,shapiro.test(Y)$p)
  x.mean   <- mean(X)
  x.cov    <- var(X)
  y.mean   <- mean(Y)
  y.cov    <- var(Y)
  
  alpha <- 0.05
  
  ICmin <- c(inf=x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
              center= x.mean,
              sup= x.mean + sqrt((x.cov)/n) * qt(1 - alpha/(2*k), n-1))
  
  ICart <- c(inf=y.mean - sqrt(y.cov/n1) * qt(1 - alpha/(2*k), n1-1),
             center= y.mean,
             sup= y.mean + sqrt((y.cov)/n1) * qt(1 - alpha/(2*k), n1-1))
  IC=rbind(IC,ICmin,ICart)
}
Ps
IC


##############EX3
rm(list=ls())
wine <- read.table('wine.txt', header=TRUE)
wine
dummyRed=ifelse(wine$type=='Red',1,0)
dummyRose=ifelse(wine$type=='Rose',1,0)

mod1 <- lm(alcohol ~ sugar+dummyRed+dummyRose+sugar:dummyRed + sugar:dummyRose, data = wine)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2

shapiro.test(mod1$residuals) # p-value = 0.7562 gaussiani

par(mfrow = c(2,2))
plot(mod1) 
vif(mod1)    #altissimi
#(b)
library(car)
linearHypothesis(mod1, rbind(c(0,0,1,0,0,0),
                             c(0,0,0,1,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0,0))   #2.2e-16 *** influisce

linearHypothesis(mod1, rbind(c(0,1,0,0,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0))   #2.2e-16 *** influisce

#(c)
#Hypothesis:
#dummyRose = 0
#dummyRed = 0
linearHypothesis(mod1, rbind(c(0,0,0,1,0,0),
                             c(0,0,1,0,0,0)), c(0,0))   #0.5486

mod2 <- lm(alcohol ~ sugar + sugar:dummyRed + sugar:dummyRose, data = wine)
summary(mod2)

mod2$coefficients
sigma2=sum(mod2$residuals^2)/mod2$df
sigma2

shapiro.test(mod2$residuals) # p-value = 0.7223 gaussiani

par(mfrow = c(2,2))
plot(mod2) 
vif(mod2)    #molto meglio
#(d)
newdat <- data.frame(sugar = 20, dummyRed=1, dummyRose=0)
guess <- predict(mod2, newdat, interval = 'prediction', level = 0.99)
guess
#fit      lwr      upr
# 13.29224 11.53151 15.05298

#################EX3
rm(list=ls())
wine <- read.table('wine.txt', header=TRUE)
wine

mod1 <- lm(alcohol ~ type+sugar:type, data = wine)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2
summary(mod1)$sigma #0.6726414
shapiro.test(mod1$residuals) # p-value = 0.7562 gaussiani

par(mfrow = c(2,2))
plot(mod1) 

vif(mod1)

library(car)
linearHypothesis(mod1, rbind(c(0,1,0,0,0,0),
                             c(0,0,1,0,0,0),
                             c(0,0,0,1,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0,0,0))   #2.2e-16 *** influisce

linearHypothesis(mod1, rbind(c(0,0,0,1,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)), c(0,0,0))   #2.2e-16 *** influisce

#(c)
#Hypothesis:
#dummyRose = 0
#dummyRed = 0
linearHypothesis(mod1, rbind(c(0,1,0,0,0,0),
                             c(0,0,1,0,0,0)), c(0,0))   #0.5486

mod2 <- lm(alcohol ~ type:sugar, data = wine)
summary(mod2)

mod2$coefficients
sigma2=sum(mod2$residuals^2)/mod2$df
sigma2
summary(mod2)$sigma  #0.6711115
shapiro.test(mod2$residuals) # p-value = 0.7223 gaussiani

par(mfrow = c(2,2))
plot(mod2) 
vif(mod2)


#(d)
newdat <- data.frame(sugar = 20, type='Red')
guess <- predict(mod2, newdat, interval = 'prediction', level = 0.99)
guess
# 13.29224 11.53151 15.05298
###############EX4
rm(list=ls())
walesharks <- read.table('walesharks.txt', header=T)
walesharks

attach(walesharks)
walesharks[,3]=log(walesharks[,3])
car <- walesharks
library(sp)
coordinates(walesharks)=c('x','y')
car <- read.table('walesharks.txt',header=TRUE)
# a) Estimate two empirical variograms, assuming the following models:
#    F(s_i)=beta_0+beta_1*D.s_i+delta(s_i). 
#    Choose the most appropriate model for the observations.
library(gstat)
v.t=variogram(sights  ~ log.chlorofill , data=walesharks)
plot(v.t,pch=19)   #better, i choose this, if they would be equal, choose the simpler
v.t

v.fit2 <- fit.variogram(v.t, vgm(0.5, "Exp", 100000))
plot(v.t, v.fit2, pch = 3)
v.fit2

wale.gstat <- gstat(formula =sights  ~ log.chlorofill,
                   data = walesharks, nmax = 100, model=v.fit2)#, set = list(gls=1))
wale.gstat

predict(wale.gstat, walesharks[2,], BLUE = TRUE)
predict(wale.gstat, walesharks[1,], BLUE = TRUE)

a1 = (predict(wale.gstat, walesharks[2,], BLUE = TRUE)$var1.pred - predict(wale.gstat, walesharks[1,], BLUE = TRUE)$var1.pred)/(car[2,4]-car[1,4])
a0= predict(wale.gstat, walesharks[2,], BLUE = TRUE)$var1.pred - a1*car[2,4] #use original table
a1
a0

#(b)
v.t1=variogram(log.chlorofill  ~  1, data=walesharks)
plot(v.t1,pch=19)   
v.t1

v.fit1 <- fit.variogram(v.t1, vgm(4, "Sph", 50000))
plot(v.t1, v.fit1, pch = 3)
v.fit1

g.t1 <- gstat(formula = log.chlorofill  ~  1, data = walesharks, model = v.fit1)

D.s0=c(253844.8, 385997.7)
s0.new=data.frame(x=253844.8, y=385997.7) # UTM coordinates
coordinates(s0.new)=c('x','y')

guess <- predict(g.t1, s0.new)
guess
guess$var1.pred   #3.983745

s1.new=data.frame(x=253844.8, y=385997.7,log.chlorofill = guess$var1.pred)
coordinates(s1.new)=c('x','y')
gue=predict(wale.gstat,s1.new)
gue$var1.pred    #5.739921
detach(walesharks)
#(d)
gue$var1.var   #0.4041272
#The variance of the prediction is very high and it is not fully informative of the true uncertainty, 
#since I did an universal kriging.
#I really recommend not using this variance: it does not account for the fact that sigma is unknown 
#and it is estimated just running the algorithm (large underestimation of the uncertainty)

