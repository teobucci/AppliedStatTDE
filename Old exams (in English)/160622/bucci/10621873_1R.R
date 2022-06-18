# -----------
# EXERCISE 1
# -----------

rm(list=ls())
d1 <- read.table('discomaniac.txt', header=TRUE)
d2 <- read.table('lipsticks.txt', header=TRUE)
load('mcshapiro.test.RData')
head(d1)
head(d2)
names(d1)
names(d2)

library(car)

n <- dim(d1)[1]
p <- dim(d1)[2]

# ------------------------- point a


d <- data.frame(pricediff=d1[,3]-d2[,3], mediadiff=d1[,4]-d2[,4]) 


plot(d, asp=1, pch=19, main='data of differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')


#### TEST
# Computation of Hotelling's T2 and quantile of Fisher distribution
d.mean   <- sapply(d,mean)
d.cov    <- cov(d)
d.invcov <- solve(d.cov)

# Significance level
alpha   <- .05
delta.0 <- c(0,0) # equivalent mu0

# Hotelling's T2
d.T2 <- n * (d.mean-delta.0) %*% d.invcov %*% (d.mean-delta.0)
d.T2
# Fisher quantile
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

# Test result
d.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# P-value computation
P <- 1-pf(d.T2*(n-p)/(p*(n-1)), p, n-p)
P







# ------------------------- point b


# Multivariate shapiro on d
mcshapiro.test(d)$p
# multivariate Gaussianity ok (not reject H0)


# ------------------------- point c


#### CONFIDENCE/REJECTION REGION

# Ellipsoidal confidence region with confidence level (1-alpha)100%
plot(d, asp=1, pch=1, main='data of the differences')

# Ellipse centered around our sample mean
ellipse(center=d.mean, shape=d.cov/n, radius=sqrt(cfr.fisher), lwd=2)




# ------------------------- point d





k <- 4 # number of intervals
cfr.t <- qt(1-alpha/(2*k),n-1)
Bf.mean <- cbind(inf = d.mean - cfr.t*sqrt(diag(d.cov)/n),
            center = d.mean, 
            sup = d.mean + cfr.t*sqrt(diag(d.cov)/n))
Bf.mean

# Bonferroni intervals for variance 
Bf.var <- cbind(inf = (n-1)*diag(d.cov)/qchisq(1-alpha/(2*k),n-1),
            center = diag(d.cov),
            sup = (n-1)*diag(d.cov)/qchisq(alpha/(2*k),n-1))
Bf.var

# we didn't reject H0, stat evidence that mean is 0





plot(d, asp=1, pch=1, main='data of the differences')
ellipse(center=d.mean, shape=d.cov/n, radius=sqrt(cfr.fisher), lwd=2, col='grey', center.cex=1.25)
abline(h=0, v=0, col='grey', lty=1, lwd=2)
points(delta.0[1], delta.0[2], pch=16, col='grey35', cex=1.25)


abline(v = Bf.mean[1,1], col='blue', lwd=1, lty=2)
abline(v = Bf.mean[1,3], col='blue', lwd=1, lty=2)
abline(h = Bf.mean[2,1], col='blue', lwd=1, lty=2)
abline(h = Bf.mean[2,3], col='blue', lwd=1, lty=2)






































































































































