rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("holiday.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
plastic <- d
plastic3 <- d$price
Ex   <- factor(plastic$location, labels=c('Chiavari','Rapallo')) # Treat.1
Ad   <- factor(plastic$type, labels=c('hotel','bb', 'apartment')) # Treat.2
ExAd <- Ex
levels(ExAd) <- c('CH','CB','CA','RH','RB','RA')
ExAd[Ex=='Chiavari' & Ad=='hotel'] <- 'CH'
ExAd[Ex=='Chiavari' & Ad=='bb'] <- 'CB'
ExAd[Ex=='Chiavari' & Ad=='apartment'] <- 'CA'
ExAd[Ex=='Rapallo' & Ad=='hotel'] <- 'RH'
ExAd[Ex=='Rapallo' & Ad=='bb'] <- 'RB'
ExAd[Ex=='Rapallo' & Ad=='apartment'] <- 'RA'


#### Verify the assumptions (although we only have 5 data in each group!)
####----------------------
# 1) normality (multivariate) in each group (4 test)
Ps <- c(shapiro.test(plastic3[ ExAd==levels(ExAd)[1]])$p,
        shapiro.test(plastic3[ ExAd==levels(ExAd)[2]])$p,
        shapiro.test(plastic3[ ExAd==levels(ExAd)[3]])$p,
        shapiro.test(plastic3[ ExAd==levels(ExAd)[4]])$p,
        shapiro.test(plastic3[ ExAd==levels(ExAd)[5]])$p,
        shapiro.test(plastic3[ ExAd==levels(ExAd)[6]])$p)
Ps #0.2579667 0.6449031 0.9418255 0.6741621
   # 0.2909484 0.3074365

# 2) homogeneity of the covariance (qualitatively)
bartlett.test(plastic3,ExAd) # 0.2473 assumptions ok


fit.aov2.int <- aov(price ~ location*type, data=d)
summary.aov(fit.aov2.int)

##### PUNTO B #####
### Test:
### 1) H0: gamma.11 = gamma.12 = gamma.21 = gamma.22 = 0    vs   H1: (H0)^c
###    i.e.,
###    H0: There is no significant interaction between the factors in terms of price
### 2) H0: tau.1 = tau.2 = 0    vs   H1: (H0)^c
###    i.e.,
###    H0: The effect "location" doesn't significantly influence price 

fit.aov2.int2 <- aov(price ~ location +type, data=d)
summary.aov(fit.aov2.int2)

fit.aov2.int2 <- aov(price ~ type, data=d)
summary.aov(fit.aov2.int2)



##### PUNTO C #####

fit.aov2.int2$coefficients
#(Intercept)      typebb   typehotel 
#   792.425      39.800     200.600 



##### PUNTO D #####
fit3 <- aov(price ~ type, data=d)
summary.aov(fit3)


n       <- length(d$price)      # total number of obs.
ng      <- table(d$type)       # number of obs. in each group
treat   <- levels(as.factor(d$type))     # levels of the treatment
g       <- length(treat)



k <- g*(g-1)/2
alpha= 0.05


Mediag  <- tapply(d$price, d$type, mean)
SSres <- sum(residuals(fit3)^2)
S <- SSres/(n-g)

# CI for all the differences
ICrange=NULL
for(i in 1:(g-1)) {
    for(j in (i+1):g) {
        print(paste(treat[i],"-",treat[j]))        
        print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                           Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
        ICrange=rbind(ICrange,as.numeric(c(L = Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )), C = Mediag[i]-Mediag[j],
                                           U = Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    }}

ICrange

#               L       C           U
# apt-bb    -116.5228  -39.8   36.92284
# apt-hotel -277.3228 -200.6 -123.87716
# bb-hotel  -237.5228 -160.8  -84.07716

