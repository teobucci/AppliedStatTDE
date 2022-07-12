rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

t1 <- read.table("acoruna.txt",header=T)
t2 <- read.table("pontevedra.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
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
alpha   <- .01
delta.0 <- c(0,0)
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # FALSE: statistical evidence to reject H0 at level 1%, evaluations are different

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P   # P-value low 0.0004434524


mcshapiro.test(t1)
mcshapiro.test(t2) # assumptions verified

##### PUNTO B #####

alpha <- 0.1
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
dimnames(IC)[[2]] <- c('inf','center','sup') 
IC

#      inf     center        sup
#T1 -1.5321009 -0.9956667 -0.4592324
#T2 -0.7973925 -0.2596667  0.2780592

# Only evaluation by T1 are significantly different
# 0 is in Bonf. Int. for T2 but the difference in T1
# is enough to reject H0 at point a



##### PUNTO C #####
t1.av <- (t1[,1]+t1[,2])/2
t2.av <- (t2[,1]+t2[,2])/2

shapiro.test(t1.av) # 0.09
shapiro.test(t2.av) # 0.659 ok


### question b)
# Test:   H0: mu1<=2*mu2   vs H1: mu1>2*mu2
# i.e. H0: mu1-2*mu2<=0 vs H1: mu1-2*mu2>0
# i.e H0: a'mu<=0      vs H1: a'mu>0 con a=c(1,-2)

### question b)
# Test:   H0: mu1<=mu2   vs H1: mu1>mu2
# i.e. H0: mu1-mu2<=0 vs H1: mu1-mu2>0
# i.e H0: a'mu<=0      vs H1: a'mu>0 con a=c(1,-1)
a <- c(1,-1)
delta.0 <- 0

n <- n1
extra <- cbind(t1.av,t2.av)
extra <- as.matrix(extra)
t.stat <- (mean(extra %*% a) - delta.0 ) / sqrt( as.numeric(var(extra %*% a)) / n ) # t-statistics (statistica 1!)

# Reject for large values of t 
# => compute the p-value as the probability of the right tail (i.e., of values >tstat)
P <- 1-pt(t.stat, n-1)
P # 0.01121848 we accept H0 at level 1%, no stat evidence that acoruna is higher

#alternatively:
t.test(t1.av-t2.av, alternative= 'greater')


