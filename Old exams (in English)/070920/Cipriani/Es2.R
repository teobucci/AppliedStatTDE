rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

t1 <- read.table("candle.txt",header=T)
t2 <- read.table("sunshine.txt",header=T)

n1 <- dim(t1)[1] # n1=3
n2 <- dim(t2)[1] # n2=4
p  <- dim(t1)[2] 

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)


##### PUNTO A #####
# Test H0: mu1 == mu2  vs  H1: mu1 != mu2
# i.e.,
# Test H0: mu1-mu2 == c(0,0)  vs  H1: mu1-mu2 != c(0,0)
alpha   <- .05
delta.0 <- c(0,0)
Spinv   <- solve(Sp)
T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  

# p-value 3.09412e-08: the mean measurements produced by the two
# brands differ

mcshapiro.test(t1) #0.544
mcshapiro.test(t2) #0.6236

##### PUNTO B #####

# p-value 3.09412e-08: the mean measurements produced by the two
# brands differ




##### PUNTO C #####

alpha <- 0.05
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
IC

#LM1 -2.8881033 -1.7092 -0.5302967
#LM2  0.4725819  1.6934  2.9142181

# Both conf. interval don't contain zero
# Sunshine is less bright at close distance
# and more bright at far distance

##### PUNTO D #####
t_diff_candle <- t1$LM2 - t1$LM1
t_diff_sunshine <- t2$LM2 - t2$LM1

shapiro.test(t_diff_candle)
shapiro.test(t_diff_sunshine)


t.test(t_diff_candle, t_diff_sunshine, conf.level=0.95, alternative="greater", 
       mu=0, var.equal=T, paired=F)

#t = -6.473, df = 98, p-value = 1
# alternative hypothesis: true difference in means is greater than 0, can't reject H0
