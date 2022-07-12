rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d1 <- read.table("presales.txt",header=T)
d2 <- read.table("sales.txt",header=T)
n <- nrow(d1)



##### PUNTO A #####
d <- data.frame(Flip.Flops=(d1[,1]-d2[,1])/d1[,1], diff2=(d1[,2]-d2[,2])/d1[,2]) 
# pre sales - sales

x11()
plot(d, asp=1, pch=19, main='data of differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')
# pre.sales prices seem higher than sale

# Multivariate shapiro on d
mcshapiro.test(d) #0.064


#### TEST
# Computation of Hotelling's T2 and quantile of Fisher distribution
n <- dim(d)[1]  # 45
p <- dim(d)[2]  #  2
d.mean   <- sapply(d,mean)
d.cov    <- cov(d)
d.invcov <- solve(d.cov)

# Significance level
alpha   <- .01
delta.0 <- c(0.2,0.2) # equivalent mu0

# Hotelling's T2
d.T2 <- n * (d.mean-delta.0) %*% d.invcov %*% (d.mean-delta.0)
d.T2
# Fisher quantile
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

# Test result
d.T2 < cfr.fisher # TRUE: accept H0 at level 1%

# P-value computation
P <- 1-pf(d.T2*(n-p)/(p*(n-1)), p, n-p)
P # 0.1749089





##### PUNTO B #####
d.mean
#Flip.Flops      diff2 
#0.1910582    0.2144863 

d.cov
#             Flip.Flops       diff2
#Flip.Flops 0.013989653   0.004755409
#diff2      0.004755409   0.005611812


##### PUNTO C #####
x11()
plot(d, asp=1, pch=1, main='data of the differences')

# Ellipse centered around our sample mean
ellipse(center=d.mean, shape=d.cov, radius=sqrt(cfr.fisher), lwd=2)

# E = { m in R^2 | ( x_ - m )' S^(-1) (x_ - m ) < F (0.99, 2, 43) * (n-1) * p / (n – p )}

d.mean
#Flip.Flops      diff2 
#0.1910582  0.2144863 

# Directions of the principal axes:
eigen(d.cov)$vectors
#-0.9113171  0.4117051
#-0.4117051 -0.9113171

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(d.cov)$values) 
# 0.4118372 0.1907902
