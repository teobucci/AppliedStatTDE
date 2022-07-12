rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table(".txt",header=T)
n <- nrow(d)



##### PUNTO A #####
x <- d
mcshapiro.test(x) # 0.102 ok
x.mean   <- sapply(x,mean)
x.cov    <- cov(x)
x.invcov <- solve(x.cov)
n <- nrow(x) 
p <- ncol(x)  # CHANGE TO DIMENSION OF MULTIVARIETY 

alpha <- 0.01
mu0 <- c(14.2350,42.4520)

# T2 Statistics
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Test: 
x.T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

# Compute the p-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P # TRUE 0.015 accept (per un pelo) H0





##### PUNTO B #####

x11()
plot(x, asp = 1)
ellipse(x.mean, shape=x.cov, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
points(x.mean[1], x.mean[2], pch = 16, col ='red', cex = 1.5) # We add a red point in correspondence of the sample mean

# { m \in R^2 s.t. (x.mean-m)' %*% (x.cov)^-1 %*% (x.mean-m) < cfr.fisher }
# Characterize region
# Center:
x.mean # 14.23503 42.45173 

# Directions of the principal axes:
eigen(x.cov)$vectors

# -0.9531768 -0.3024136
# 0.3024136 -0.9531768


# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov)$values) 
# 0.02493350 0.00464064
