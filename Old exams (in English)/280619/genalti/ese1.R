rm(list = ls())
load('mcshapiro.test.RData')
ter <- read.table("terrassa.txt")
gir <- read.table("girona.txt")
head(ter)

mcshapiro.test(gir)
mcshapiro.test(ter) # qui siamo al pelo ma la assumiamo comunque normale

# usa Spooled
D <- data.frame(ter[,1]-gir[,1], ter[,2]-gir[,2])
mcshapiro.test(D)# 0.862

n <- dim(D)[1]  
p <- dim(D)[2]  

D.mean   <- sapply(D,mean)
D.cov    <- cov(D)
D.invcov <- solve(D.cov)

alpha   <- .05
delta.0 <- c(0,0)

D.T2 <- n * (D.mean-delta.0) %*% D.invcov %*% (D.mean-delta.0)

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p) #2.997602e-15 c'è differenza: le tapa sono meglio a girona

# reject H0 at 95% (also at 99%)

k <- 2

cfr.t <- qt(1-alpha/(2*k),n-1)

IC.BF.T1 <- c( D.mean[1]-cfr.t*sqrt(D.cov[1,1]/n) , D.mean[1], D.mean[1]+cfr.t*sqrt(D.cov[1,1]/n) )
IC.BF.T2  <- c( D.mean[2]-cfr.t*sqrt(D.cov[2,2]/n) , D.mean[2], D.mean[2]+cfr.t*sqrt(D.cov[2,2]/n) )

Bf <- rbind(IC.BF.T1, IC.BF.T2)
dimnames(Bf)[[2]] <- c('inf','center','sup')

#              inf    center       sup
# IC.BF.T1 -10.221226 -8.814286 -7.407346
# IC.BF.T2  -4.780614 -2.994286 -1.207958

avg.ter <- (ter[,1]+ter[,2])/2
shapiro.test(avg.ter) # 0.7903

avg.gir <- (gir[,1]+gir[,2])/2
shapiro.test(avg.gir) # 0.5392

avg.D <- avg.ter - avg.gir
shapiro.test(avg.D) # 0.2912

t.test(avg.D, conf.level = 0.95)
# p-value = 7.348e-12 there is difference

