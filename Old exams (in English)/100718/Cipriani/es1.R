rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("revenues.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
mcshapiro.test(d) # 0.706 assumptions met
x11()
matplot(t(d), type='l') # revenues go down in sept

n<- dim(d)[1]
q <- dim(d)[2]
M <- sapply(d,mean)
S <- cov(d)

C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), 3, 4, byrow=T)
# here we are looking at the effects on the d
# between the 1st and the 2nd, 3rd, 4th

# Test: H0: C%*%mu == 0 vs H1: C%*%mu != 0
alpha   <- .1
delta.0 <- c(0,0,0)

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

T2 <- n * t( Md - delta.0 ) %*% Sdinv %*% ( Md - delta.0 )
cfr.fisher <- ((q-1)*(n-1)/(n-(q-1)))*qf(1-alpha,(q-1),n-(q-1)) 

T2 < cfr.fisher # FALSE there is difference

# T2 is much higher than cfr.fisher => the p-value will be very small
P <- 1-pf(T2*(n-(q-1))/((q-1)*(n-1)),(q-1),n-(q-1))
P # 0


##### PUNTO B #####
# Bonferroni intervals 
C <- matrix(c(-1, 1, 0, 0,
              -1, 0, 1, 0,
              -1, 0, 0, 1), 3, 4, byrow=T)

k     <- 3   # number of increments (i.e., dim(C)[1])
cfr.t <- qt(1-alpha/(2*k),n-1)

Md <- C %*% M 
Sd <- C %*% S %*% t(C)
Sdinv <- solve(Sd)

IC.BF <- cbind( Md - cfr.t*sqrt(diag(Sd)/n) , Md, Md + cfr.t*sqrt(diag(Sd)/n) )
IC.BF

# Increase in high season june -> july -> august
# great decrease in sept




