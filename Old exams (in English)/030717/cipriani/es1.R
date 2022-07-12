rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("kimono.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
g <- length(levels(as.factor(d$city))) # factor 1
b <- length(levels(as.factor(d$type))) # factor 2
n <- length(d$value)/(g*b)

M           <- mean(d$value)
Mdistr      <- tapply(d$value,  d$city, mean)
Mbenz       <- tapply(d$value, d$type, mean)
SSdistr <- sum(n*b*(Mdistr - M)^2)              # or from the summary: 1.53    
SSbenz  <- sum(n*g*(Mbenz  - M)^2)              # or from the summary: 66.70
SSres   <- sum((d$value - M)^2) - (SSdistr+SSbenz) 

#### Two-ways ANOVA
###----------------
### Model with interaction (complete model): 
### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
###     i=1,2 (effect station), j=1,2 (effect gasoline)
fit.aov2.int <- aov(value ~ city*type, data=d)
summary.aov(fit.aov2.int)

#city          1      2       2     0.930  0.335    
#type          1  29343   29343 14528.170 <2e-16 ***
#city:type     1      0       0     0.124  0.725    
#Residuals   524   1058       2                     


# ASSUMPTIONS
city   <- factor(d$city, labels=c('L','H')) # Treat.1
type   <- factor(d$type, labels=c('L','H')) # Treat.2
citytype <- city
levels(citytype) <- c('LL','LH','HL','HH')
citytype[city=='L' & type=='L'] <- 'LL'
citytype[city=='L' & type=='H'] <- 'LH'
citytype[city=='H' & type=='L'] <- 'HL'
citytype[city=='H' & type=='H'] <- 'HH'


# 1) normality (multivariate) in each group (4 test)
Ps <- c(shapiro.test(d$value[ citytype==levels(citytype)[1]])$p,
        shapiro.test(d$value[ citytype==levels(citytype)[2]])$p,
        shapiro.test(d$value[ citytype==levels(citytype)[3]])$p,
        shapiro.test(d$value[ citytype==levels(citytype)[4]])$p)
Ps
#  0.50984756 0.38843793 0.21254572 0.07499362


# 2) homogeneity of the covariance
bartlett.test(d$value,citytype)
# Bartlett's K-squared = 1.8837, df = 3, p-value = 0.5969

##### PUNTO B #####

# 1) H0: gamma.11 = gamma.12 = gamma.21 = gamma.22 = 0    vs   H1: (H0)^c
###    i.e.,
###    H0: There is no significant interaction between the factors city and type
###        in terms of values p-value = 0.77

fit.aov2.int2 <- aov(value ~ city + type, data=d)
summary(fit.aov2.int2 )

### 2) H0: tau.1 = tau.2 = 0    vs   H1: (H0)^c
###    i.e.,
###    H0: The effect "city" doesn't significantly influence value
###    p-value = 0.335

fit.aov3 <- aov(value ~ type, data=d)
summary(fit.aov3)

##### PUNTO C #####
## DUBBI SU QUESTO
alpha=0.05
n       <- length(d$value)      # total number of obs.
ng      <- table(d$type)       # number of obs. in each group
treat   <- levels(as.factor(d$type))     # levels of the treatment
g       <- length(treat)



k <- g*(g-1)/2
alpha= 0.05


Mediag  <- tapply(d$value, d$type, mean)
SSres <- sum(residuals(fit.aov3)^2)
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

# "hand-made - ready-to-use"
#   L       C         U
#  14.66669 14.90947  15.15225

