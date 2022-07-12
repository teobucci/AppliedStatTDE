rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

data <- read.table("bento.txt",header=T)
n <- nrow(d)

d <- data.frame(diff1=data[,1]-data[,5], diff2=data[,2]-data[,6],
                diff3=data[,3]-data[,7], diff4=data[,4]-data[,8] )


##### PUNTO A #####
x11()
plot(d, asp=1, pch=19, main='data of differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

# Multivariate gaussianity
mcshapiro.test(d)


n <- dim(d)[1]  # 32
p <- dim(d)[2]  #  4
d.mean   <- sapply(d,mean)
d.cov    <- cov(d)
d.invcov <- solve(d.cov)

# Significance level
alpha   <- 0.05
delta.0 <- c(0,0,0,0) # equivalent mu0

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
# 0 there is difference

##### PUNTO B #####
d = -d
IC.T2.1 <- c( d.mean[1]-sqrt(cfr.fisher*d.cov[1,1]/n) , d.mean[1], d.mean[1]+sqrt(cfr.fisher*d.cov[1,1]/n) )
IC.T2.2  <- c( d.mean[2]-sqrt(cfr.fisher*d.cov[2,2]/n) , d.mean[2], d.mean[2]+sqrt(cfr.fisher*d.cov[2,2]/n) )
IC.T2.3 <- c( d.mean[3]-sqrt(cfr.fisher*d.cov[3,3]/n) , d.mean[3], d.mean[3]+sqrt(cfr.fisher*d.cov[3,3]/n) )
IC.T2.4 <- c( d.mean[4]-sqrt(cfr.fisher*d.cov[4,4]/n) , d.mean[4], d.mean[4]+sqrt(cfr.fisher*d.cov[4,4]/n) )
T2 <- rbind(IC.T2.1, IC.T2.2, IC.T2.3, IC.T2.4)
dimnames(T2)[[2]] <- c('inf','center','sup')
T2    

#              inf     center       sup
#IC.T2.1 -37.34390  -2.145937  33.05202
#IC.T2.2 107.51324 123.577812 139.64238
#IC.T2.3 -12.40493  11.463438  35.33181
#IC.T2.4 111.98999 118.150000 124.31001




