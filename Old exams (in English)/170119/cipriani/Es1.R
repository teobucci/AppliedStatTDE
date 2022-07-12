rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

data <- read.table("luggage.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
t1 <- data[,c(1,3)]
t2 <- data[,c(2,4)]
n1 <- dim(t1)[1] # n1=3
n2 <- dim(t2)[1] # n2=4
p  <- dim(t1)[2] # p=2
# we compute the sample mean, covariance matrices and the matrix Spooled
t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)

#### TEST
# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
alpha   <- 0.1
delta.0 <- c(0,0)
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # FALSE: statistical evidence to reject H0 at level 5%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P   # P-value low 
# significant difference




##### PUNTO B #####
d <- data.frame(diff1=data[,1]-data[,3], diff2=data[,2]-data[,4]) 

x11()
plot(d, asp=1, pch=19, main='data of differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')


#### TEST
# Computation of Hotelling's T2 and quantile of Fisher distribution
n <- dim(d)[1]  # 232
p <- dim(d)[2]  #  2
d.mean   <- sapply(d,mean)
d.cov    <- cov(d)
d.invcov <- solve(d.cov)

# Significance level
alpha   <- 0.1
delta.0 <- c(0,0) # equivalent mu0

# Hotelling's T2
d.T2 <- n * (d.mean-delta.0) %*% d.invcov %*% (d.mean-delta.0)
d.T2
# Fisher quantile
cfr.fisher2 <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher2

# Test result
d.T2 < cfr.fisher2 # FALSE: we reject H0 at level 5%

# P-value computation
P <- 1-pf(d.T2*(n-p)/(p*(n-1)), p, n-p)
P # 0, again different






##### PUNTO C #####
p <- 4
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
cfr.fisher2 <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

# for mean of weights menvswomen
IC.T2.X1 <- c(t1.mean[1]-t2.mean[1]-sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)),t1.mean[1]-t2.mean[1], t1.mean[1]-t2.mean[1]+sqrt(cfr.fisher*Sp[1,1]*(1/n1+1/n2)) )
IC.T2.X2 <- c(t1.mean[2]-t2.mean[2]-sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)),t1.mean[2]-t2.mean[2], t1.mean[2]-t2.mean[2]+sqrt(cfr.fisher*Sp[2,2]*(1/n1+1/n2)) )
IC.T2 <- rbind(IC.T2.X1, IC.T2.X2)

# for difference of luggages
IC.T2.1 <- c( d.mean[1]-sqrt(cfr.fisher2*d.cov[1,1]/n) , d.mean[1], d.mean[1]+sqrt(cfr.fisher2*d.cov[1,1]/n) )
IC.T2.2  <- c( d.mean[2]-sqrt(cfr.fisher2*d.cov[2,2]/n) , d.mean[2], d.mean[2]+sqrt(cfr.fisher2*d.cov[2,2]/n) )
T2 <- rbind(IC.T2.1, IC.T2.2)

T2 <- rbind(IC.T2, T2)
dimnames(T2)[[2]] <- c('inf','center','sup')


T2

#             inf    center       sup
#IC.T2.X1 -4.736502 -4.339052 -3.941601
#IC.T2.X2 -4.378741 -3.991897 -3.605052
#IC.T2.1  -3.069034 -2.714483 -2.359931
#IC.T2.2  -2.749345 -2.367328 -1.985310

# 0 is in none of these CI (as expected)




##### PUNTO D #####
t.test(data[,4], alternative='greater', mu = 23, conf.level=0.9)
# p-value = 1
# alternative hypothesis: true mean is greater than 23
# no charge

