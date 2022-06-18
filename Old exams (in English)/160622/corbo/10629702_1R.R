##################EX1
rm(list=ls())
discomaniac <- read.table('discomaniac.txt', header=TRUE)
head(discomaniac)
lipsticks <- read.table('lipsticks.txt', header=TRUE)
head(lipsticks)

D <- data.frame(price=discomaniac[,3]-lipsticks[,3], condition=discomaniac[,4]-lipsticks[,4]) 
D
n=dim(D)[1]
x11()
plot(D, asp=1, pch=19, main='Dataset of Differences')
abline(h=0, v=0, col='grey35')
points(0,0, pch=19, col='grey35')

### T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0)

# Test the Gaussian assumption (on D!)
#load(file.choose())
mcshapiro.test(D)      #0.7516

n <- dim(D)[1]  
p <- dim(D)[2]

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)
D.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P   # 0.01625523
# reject H0 at 5% 


# Center:
D.mean

# Directions of the principal axes:
eigen(D.cov/n)$vectors

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(D.cov/n)$values) 
# Warning: Conf Reg => D.cov/n !!!!!!!!!!!!!!
# Confidence region (centred in x.mean)
# { m \in R^2 s.t. n * (x.mean-m)' %*% (x.cov)^-1 %*% (x.mean-m) < cfr.fisher }
library(car)
x11()
plot(D, asp = 1,main='Comparison of confidence regions')
ellipse(D.mean, D.cov/n, sqrt(cfr.fisher), col = 'red', lty = 1, center.pch = 4, center.cex=1.5, lwd=2)
points(delta.0[1],delta.0[2],col='blue',pch=19)

k=2*p

cfr.t <- qt(1-alpha/(2*k),n-1)
BF={}
for(i in 1:k){
  IC.BF=c( D.mean[i]-cfr.t*sqrt(D.cov[i,i]/n) , D.mean[i], D.mean[i]+cfr.t*sqrt(D.cov[i,i]/n) )
  ICvar <- c(inf=(D.cov[i,i])*(n-1) / qchisq(1 - alpha/(2*k), n-1),center=(D.cov[i,i]),sup=(D.cov[i,i])*(n-1) / qchisq(alpha/(2*k), n-1))
  BF=rbind(BF,IC.BF,ICvar)
}
dimnames(BF)[[2]] <- c('inf','center','sup')
BF
