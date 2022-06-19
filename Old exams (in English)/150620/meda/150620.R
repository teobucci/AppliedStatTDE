#ex 1 

#a) 
attach(pollution)
load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
library(car)

pollution <- read.table(file='pollution.txt', header=T)
head(pollution)
dim(pollution)

n <- dim(pollution)[1]
p <- dim(pollution)[2]

#normal assumptions 
mcshapiro.test(pollution) #p-value of the test around 0.29 so we can confirm the normal assumptions

alpha <- 0.05
mu0 <- c(50,50)

x.mean   <- sapply(pollution,mean) #value [165 150]
x.cov    <- cov(pollution)
x.invcov <- solve(x.cov)

# T2 Statistics
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0)  #value 263
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)  #T2 statistic follows a fisher distribution  
# Test: 
x.T2 < cfr.fisher   #False so we are in the rejection region, we have evidence to reject H0 at level 1%
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P #p- value is equal to 0 

#b)# Spectral decomposition of convariance matrix
decomp <- eigen(x.cov)
decomp
# Direction of axes
decomp$vectors
# Center
x.mean
# Radius of ellipse
r <- sqrt(cfr.fisher)
r
#Length of semi-axes
lengthSemiAxes <- r*sqrt(decomp$values)
lengthSemiAxes

x11()
plot(pollution, asp = 1)
ellipse(x.mean, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)

#c)
# - If the mean under H0 (mu0) is contained in the confidence region
#   of level 1-alpha, then we do not reject H0 at level alpha
# => the confidence region of level 1-alpha contains all the mu0
#    that we would accept at level alpha

#D)
T2 <- cbind(inf = x.mean - sqrt(cfr.fisher*diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + sqrt(cfr.fisher*diag(x.cov)/n))
T2


#EX 2) 
head(stoneflakes)

x11()
plot(stoneflakes)

HC <- hclust(dist(stoneflakes, method='euclidean'), method = 'ward.D2')

x11()
plot(HC, hang=-0.1, sub='', labels=F, xlab='')

rect.hclust(HC, k=3)

pag <- cutree(HC, k=3) 
table(pag)

x11() #plot of the clustering 
plot(stoneflakes , col=pag+1, asp=1, pch=16, lwd=2)

#b) 
clust <- as.factor(pag)

i1 <- which(pag=='1')
i2 <- which(pag=='2')
i3 <- which(pag=='3')

p  <- 2
g <- 3
n1 <- table(pag)[1]
n2 <- table(pag)[2]
n3 <- table(pag)[3]
n <- n1+n2+n3

load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
library(car)

mcshapiro.test(stoneflakes[pag=='1',])
mcshapiro.test(stoneflakes[pag=='2',])
mcshapiro.test(stoneflakes[pag=='3',]) #normality assumptions verified in both groups

fit <- manova(as.matrix(stoneflakes) ~ as.factor(pag))
summary.manova(fit,test="Wilks") #high evidence to say memebership to a group influences the mean 

#c)### Via Bonferroni
alpha <- 0.1
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

m1 <- sapply(stoneflakes[i1,],mean)    
m2 <- sapply(stoneflakes[i2,],mean)
m3 <- sapply(stoneflakes[i3,],mean)

W <- diag(t(fit$res) %*% fit$res)/(n-g)  
ng <- c(length(i1),length(i2),length(i3)) 

Bf12 <- cbind(m1-m2 - qt(1 -alpha/(2*k), n-g) * sqrt((1/ng[1]+1/ng[2])*W), m1-m2, m1-m2 + qt(1 -alpha/(2*k), n-g) * sqrt((1/ng[1]+1/ng[2])*W))
Bf23 <- cbind(m2-m3 - qt(1 -alpha/(2*k), n-g) * sqrt((1/ng[2]+1/ng[3])*W), m2-m3, m2-m3 + qt(1 -alpha/(2*k), n-g) * sqrt((1/ng[2]+1/ng[3])*W))
Bf31 <- cbind(m3-m1 - qt(1 -alpha/(2*k), n-g) * sqrt((1/ng[3]+1/ng[1])*W), m3-m1, m3-m1 + qt(1 -alpha/(2*k), n-g) * sqrt((1/ng[3]+1/ng[1])*W))

IC <- list(Bf12, Bf23,Bf31)
IC
#just for the difference in length between group 1 and 2 we don't have global statistical evidence to 
#state there's a difference, all the other combinations don't contain the zero 

#c) 
attach(airfoil)
# sound = b.0 + b1*Fast + b2*freq + b3*fast*freq
# bo.low = b0 
#bo.fast = b0 + b1 

# E[eps]=0, Var(eps)=sigma^2

velocity<-airfoil$velocity

fit <- lm(sound ~ frequency + airfoil$velocity + frequency:airfoil$velocity)
summary(fit)

sum(residuals(fit)^2)/fit$df  #variance 

par(mar=c(1,1,1,1))
par(mfrow=c(2,2))

plot(fit)

shapiro.test(residuals(fit))

#b) # 1. the variable frequency;
linearHypothesis(fit,
                 rbind(c(0,1,0,0),
                       c(0,0,0,1)),
                       
                 c(0,0)) #very low p-valu -- coefficients related to frequency are significant 

# 2. the variable velocity;
linearHypothesis(fit,
                 rbind(c(0,0,1,0),
                       c(0,0,0,1)),
                 
                 c(0,0))#very low p-value -- coefficients related to velocity are significant
#3 variabile impact of velocity on frequency;
linearHypothesis(fit,
                 c(0,0,0,1),
                 0) #remove the single variabile frequency:velocity 

#c) 
fit2 <- lm(sound ~ frequency + velocity)
summary(fit2)
#only impact of velocity on the intercept 

#d) 
new_data <- data.frame(velocity='H',frequency=15000)
IP <- predict(fit2, newdata=new_data, interval='confidence', level=1-0.05)
IP

#ex 4) 

#a) 
a <- read.table('revenues.txt')
data=read.table('revenues.txt')
attach(data)
coordinates(data)=c('x','y')

v.t=variogram(revenue ~ population, data=data)
plot(v.t,pch=19)
#fitting the variogram 
v.fit <- fit.variogram(v.t, vgm(600, "Exp", 2000))
plot(v.t, v.fit, pch = 3)
v.fit

g.tr <- gstat(formula = revenue ~ population, data = data, model = v.fit)

a1 = (predict(g.tr, data[2,], BLUE = TRUE)$var1.pred- predict(g.tr, data[1,], BLUE = TRUE)$var1.pred)/(a[2,4]-a[1,4])
a0= predict(g.tr, data[2,], BLUE = TRUE)$var1.pred - a1*a[2,4] #use original table

#b) 
v.t2=variogram(population ~ distance, data=data)
plot(v.t2,pch=19)
#fitting the variogram 
v.fit2 <- fit.variogram(v.t2, vgm(3500, "Exp", 1000,2000))
plot(v.t2, v.fit2, pch = 3)
v.fit2

g.tr2<- gstat(formula = population ~ distance , data = data, model = v.fit2)

s0.new=data.frame(x=514703.8, y=5035569.3,distance=1666.32) # UTM coordinates
coordinates(s0.new)=c('x','y')
predict(g.tr2, s0.new)
s0.new=data.frame(x=514703.8, y=5035569.3,population=6090.064,distance=1666.32)
coordinates(s0.new)=c('x','y')
predict(g.tr, s0.new)

#variable underestimated in this cases 
