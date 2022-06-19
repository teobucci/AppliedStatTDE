library(car)
library(MASS)
library(class)
library(sp)
library(lattice)
library(geoR)
library(gstat)
rm(list = ls())
setwd('C:/Users/gianm/Desktop/TDEApplied/Exam 15 Giugno')
load('mcshapiro.test.RData')
df <- read.table("pollution.txt")
head(df)
mcshapiro.test(df)# pvalue = 0.2284

p <- dim(df)[2]
n <- dim(df)[1]

# Test: H0: mu=c(50,50) vs H1: mu!=c(50,50)
alpha    <- 0.05
mu0      <- c(50,50)
x.mean   <- colMeans(df)
x.cov    <- cov(df)
x.invcov <- solve(x.cov)

x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
x.T2 < cfr.fisher

P <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p)
P

plot(df[,1],df[,2], pch = 16, xlab = 'PM2.5', ylab = 'PM10', main = 'Pollution')
ellipse(x.mean, x.cov/n, sqrt(cfr.fisher), col = 'red', lty = 2, lwd=2, center.cex=1)
points(x.mean[1],x.mean[2], col ='orange', pch = 16, cex = 1)
points(50,50, col = 'orange',pch = 16)
# Confidence region (centred in x.mean)
# { m \in R^2 s.t. n * (x.mean-m)' %*% (x.cov)^-1 %*% (x.mean-m) < cfr.fisher }
# centre:
x.mean
# direction of the axes
eigen(x.cov)$vector[,1]
eigen(x.cov)$vector[,2]
# radius
sqrt(cfr.fisher)
# lenght of the semi-axes
sqrt(eigen(x.cov/n)$values)*sqrt(cfr.fisher)

a1 <- c(1,0)
a2 <- c(0,1)

ICT21<-data.frame(L=t(a1)%*%x.mean-sqrt(t(a1)%*%x.cov%*%a1/n)*sqrt(cfr.fisher),C=t(a1)%*%x.mean,U=t(a1)%*%x.mean+sqrt(t(a1)%*%x.cov%*%a1/n)*sqrt(cfr.fisher))
ICT22<-data.frame(L=t(a2)%*%x.mean-sqrt(t(a2)%*%x.cov%*%a2/n)*sqrt(cfr.fisher),C=t(a2)%*%x.mean,U=t(a2)%*%x.mean+sqrt(t(a2)%*%x.cov%*%a2/n)*sqrt(cfr.fisher))
ICT21
ICT22
