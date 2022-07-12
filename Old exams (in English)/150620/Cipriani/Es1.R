rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("pollution.txt",header=T)
n <- nrow(d)

x <- d

##### PUNTO A #####

n <- dim(x)[1]
p <- dim(x)[2]
alpha <- 0.05 # At level 5%

# Sample mean, covariance 
x.mean   <- sapply(x,mean)
x.cov    <- cov(x)
x.invcov <- solve(x.cov)
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

mu0 = c(50,50)
x.T2 <- n * t(x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
x.T2 < cfr.fisher # FALSE

P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P # 9.131362e-11


mcshapiro.test(x) #0.232

##### PUNTO B #####
x11()
plot(x, asp=1, pch=1)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)

# Center:
x.mean #  PM2.5      PM10 
#        92.07547 111.44978 

# Directions of the principal axes:
eigen(x.cov/n)$vectors
#0.5256937 -0.8506739
# 0.8506739  0.5256937


# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov/n)$values) 
# 24.268852  8.575489


##### PUNTO C #####
x11()
plot(x, asp=1, pch=1)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)
points(50,50, pch=4,col=2,cex = 3)

# outside, ok with point a (reject H0)

##### PUNTO D #####

T2 <- cbind(inf = x.mean - sqrt(cfr.fisher*diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + sqrt(cfr.fisher*diag(x.cov)/n))
T2


#          inf    center      sup
#PM2.5 77.37913  92.07547 106.7718
#PM10  90.31843 111.44978 132.5811

