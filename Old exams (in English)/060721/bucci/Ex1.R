# -----------
# EXERCISE 1
# -----------

rm(list=ls())
data <- read.table('chicca.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point a

mcshapiro.test(data)
# we don't reject Gaussianity

# H0: mu == mu0 vs H1: mu != mu0
# with mu0=c(0, 90)

mu0   <- c(0, 90)
alpha <- 0.01

# Compute the test statistics
x.mean   <- colMeans(data)
x.cov    <- cov(data)
x.invcov <- solve(x.cov)

x.T2 <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
x.T2
# 80.99549

# Verify if the test statistics belongs to the rejection region
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher
# 9.819932
x.T2 < cfr.fisher
# FALSE --> we reject H0

# RR for the mean (ellipsoidal region) 
# { m \in R^4(?) t.c. n * (m-mu0)' %*% (x.cov)^-1 %*% (m-mu0) > cfr.fisher }

# Center of the rejection region
mu0
# 0 90

# Region of rejection (centered in mu0)
plot(data,pch=19)
ellipse(mu0, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
points(x.mean[1], x.mean[2], pch = 16, col ='red', cex = 1.5)

# ------------------------- point b

P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P

# ------------------------- point c

k <- p
cfr.t <- qt(1 - alpha/(k*2), n-1)

Bf <- cbind(inf = x.mean - cfr.t*sqrt(diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + cfr.t*sqrt(diag(x.cov)/n))
Bf
# inf    center      sup
# delay  3.004227  4.433333  5.86244
# stay  89.491055 91.233333 92.97561
# 0 is outside the delay CI

# ------------------------- point d

staying_time_real <- data$delay + data$stay
mean(staying_time_real)
shapiro.test(staying_time_real)
t.test(staying_time_real, alternative='two.sided', mu = 90, conf.level = 0.9)
# p-value = 7.453e-10
# We reject H0: the scheduling policy is inappropriate.










































































































































