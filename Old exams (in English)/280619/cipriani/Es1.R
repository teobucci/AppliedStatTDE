rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table(".txt",header=T)
n <- nrow(d)


t1 <- read.table("terrassa.txt",header=T)
t2 <- read.table("girona.txt",header=T)



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
alpha   <- .05
delta.0 <- c(0,0)
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # FALSE: statistical evidence to reject H0 at level 1%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P   # P-value 0

mcshapiro.test(t1) # 0.01
mcshapiro.test(t2) # 0.95



##### PUNTO B #####

#### BONFERRONI INTERVALS FOR THE DIFFERENCE OF MEANS
# 1) Bonf intervals for the difference of means
alpha <- 0.05
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
IC


# T1 7.231152 8.814286 10.397420
# T2 1.139940 2.994286  4.848632
# Sooo much different


##### PUNTO C #####
t1.av <- (t1[,1]+t1[,2])/2
t2.av <- (t2[,1]+t2[,2])/2

t.diff <- t2.av - t1.av
t.test(t.diff, alternative="greater")

# 3.674e-12 YES

