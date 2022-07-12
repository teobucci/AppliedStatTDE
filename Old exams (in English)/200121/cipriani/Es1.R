rm(list=ls())
graphics.off()
locolor('mcshapiro.test.RData')

d <- recolor.table("wine.txt",hecolorer=T)
n <- nrow(d)



##### PUNTO A #####

region   <- factor(d$region, labels=c('P','T','V')) # Treat.1
color   <- factor(d$color, labels=c('R','W')) # Treat.2
regioncolor <- region
levels(regioncolor) <- c('PR','PW','TR','TW','VR','VW')
regioncolor[region=='P' & color=='R'] <- 'PR'
regioncolor[region=='P' & color=='W'] <- 'PW'
regioncolor[region=='T' & color=='R'] <- 'TR'
regioncolor[region=='T' & color=='W'] <- 'TW'
regioncolor[region=='V' & color=='R'] <- 'VR'
regioncolor[region=='V' & color=='W'] <- 'VW'

#### Verify the assumptions (although we only have 5 data in each group!)
####----------------------
# 1) normality (multivariate) in each group (4 test)
Ps <- c(shapiro.test(d$alcohol[ regioncolor==levels(regioncolor)[1]])$p,
        shapiro.test(d$alcohol[ regioncolor==levels(regioncolor)[2]])$p,
        shapiro.test(d$alcohol[ regioncolor==levels(regioncolor)[3]])$p,
        shapiro.test(d$alcohol[ regioncolor==levels(regioncolor)[4]])$p,
        shapiro.test(d$alcohol[ regioncolor==levels(regioncolor)[5]])$p,
        shapiro.test(d$alcohol[ regioncolor==levels(regioncolor)[6]])$p)
Ps # 0.2369077 0.6525198 0.4612391 0.5279147
  #0.9702243 0.6612603

# 2) homogeneity of the covariance (qualitatively)
bartlett.test(d$alcohol,regioncolor) # 0.98 
# assumptions verified


fit.aov2.int <- aov(alcohol ~ region*color,d)
summary.aov(fit.aov2.int)



##### PUNTO B #####

### Test:
### 1) H0: gamma.11 = gamma.12 = gamma.21 = gamma.22 = 0    vs   H1: (H0)^c
###    i.e.,
###    H0: There is no significant interaction between the factors s
###         in terms of alcohol
### 3) H0: beta.1 = beta.2 = 0    vs   H1: (H0)^c
###    i.e.,
###    H0: The effect "region" doesn't significantly influence alcohol

### Additive model: 
### X.ijk = mu + tau.i + eps.ijk; eps.ijk~N(0,sigma^2), 
###     i=1,2 (effect color)


fit.aov2.int2 <- aov(alcohol ~ region + color,d)
summary.aov(fit.aov2.int2)


fit.aov2.int3 <- aov(alcohol ~ color,d)
summary.aov(fit.aov2.int3)
# reduced model

fit.aov2.int3$coefficients
# (Intercept)  colorwhite 
#  8.4493264  -0.9350851 

##### PUNTO C #####

t1 <- data.frame(d$alcohol[d$color=="red"])
t2 <- data.frame(d$alcohol[d$color=="white"])


fit3 <- aov(alcohol ~ color,d) 
summary(fit3)


n <- dim(d)[1]
g <- 2
k <- 3 # 2 for groups + 1 variance
S <- sum(residuals(fit3)^2)/(n-g)

alpha<- .01

Mg  <- tapply(d[,1], d$color, mean) 

label <- levels(factor(d$color))
n1 <- length(d[d$color==label[1],1])
n2 <- length(d[d$color==label[2],1])
n3 <- length(d[d$color==label[3],1])
t <- qt(1-alpha/(2*k),n-g)

# Conf int for the means
ICB1<-data.frame(L=Mg[1]-sqrt(S*(1/n1))*t,C=Mg[1],U=Mg[1]+sqrt(S/n1)*t)
ICB2<-data.frame(L=Mg[2]-sqrt(S*(1/n2))*t,C=Mg[2],U=Mg[2]+sqrt(S/n2)*t)
ICB<-data.frame(rbind(ICB1,ICB2))
ICB

#          L        C        U
#red   8.201944 8.449326 8.696709
#white 7.266859 7.514241 7.761624

# Conf int for variances
chi_u <- qchisq(alpha/(2*k),n-g)
chi_l <- qchisq(1-alpha/(2*k),n-g)
ICBV <- data.frame(L=(n-g)*S/chi_l,C=S,U=(n-g)*S/chi_u)
ICBV

#      L         C         U
#  0.3748061 0.5156073 0.7442128


