rm(list = ls()) 
load('mcshapiro.test.RData')


##### Problem 1 #####
df1 = read.table("discomaniac.txt", header = TRUE)
df2 = read.table("lipsticks.txt", header = TRUE)

val1 = df1[,c(3,4)]
val2 =  df2[,c(3,4)]

df_diff = val1-val2

## Gaussianity of differences
mcshapiro.test(df_diff)$p    #  0.7428
# boxplot(df_diff)


## T2 Hotelling Test 
# H0: delta == delta.0 vs H1: delta != delta.0
# with delta.0=c(0,0)

n = dim(df_diff)[1]
p = dim(df_diff)[2]

diff_mean = sapply(df_diff, mean)
diff_mean
diff_cov = cov(df_diff)
diff_invcov = solve(diff_cov)

alpha   = .05
delta_0 = rep(0, p)

diff_T2 = n * (diff_mean-delta_0) %*% diff_invcov %*% (diff_mean-delta_0)
diff_T2

cfr.fisher = ((n-1)*p/(n-p))*qf(1-alpha/p,p,n-p)
cfr.fisher

diff_T2 < cfr.fisher # FALSE: we reject H0 at level 5%
# we compute the p-value
P = 1-pf(diff_T2*(n-p)/(p*(n-1)), p, n-p)
P   # rifiuto l'ipotesi nulla di delta_o = 0

# 0.01625523


#### POINT C
# plot of ellipse with simultaneous CI
x11()

library(car)
plot(df_diff, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=diff_mean, shape=diff_cov/n, radius=sqrt(cfr.fisher), lwd=2)
# e = ellipse(center=df_mean, shape=df_cov, radius=sqrt(qchisq(1-alpha,p)),lty=2,col='red',lwd=2)
abline(v = 0, col='grey', lwd=1, lty=2)
abline(h = 0, col='grey', lwd=1, lty=2)



## POINT D
k = 4 # number of CI
g = 2 # number of clusters
IC={}
Ps={}
for(i in 1:g){
  X = df_diff[,i] # i need only the major axis
  n = length(X)
  Ps = c(Ps,shapiro.test(X)$p)
  x.mean   = mean(X)
  x.cov    = var(X)
  
  ICmean = c(inf    = x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
             center = x.mean,
             sup    = x.mean + sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1))
  
  ICvar = c(inf     = x.cov*(n-1) / qchisq(1 - alpha/(2*k), n-1),
            center  = x.cov,
            sup     = x.cov*(n-1) / qchisq(alpha/(2*k), n-1))
  
  IC = rbind(IC,
             ICmean,
             ICvar)
}
Ps
IC

# > Ps
# [1] 0.4688601 0.8783316

# inf   center      sup
# ICmean  0.0480422 0.683000 1.317958
# ICmean -0.6597456 0.200500 1.060746
# ICvar   0.5322847 1.059612 2.841861
# ICvar   0.9770104 1.944921 5.216247














