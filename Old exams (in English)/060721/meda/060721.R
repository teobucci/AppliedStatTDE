# TDE 6/7/2021 

#EX- 1

load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
library(car)

chicca <- read.table(file='chicca.txt', header=T)
head(chicca)
dim(chicca)

n <- dim(chicca)[1]
p <- dim(chicca)[2]

#normal assumptions 
mcshapiro.test(chicca) #p-value of the test around 0.2 so we can confirm the assumptions

alpha <- 0.01
mu0 <- c(0,90)

x.mean   <- sapply(chicca,mean) #value [4.43 91.23]
x.cov    <- cov(chicca)
x.invcov <- solve(x.cov)

# T2 Statistics
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0)  #value 81
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)  #T2 statistic follows a fisher distribution  
# Test: 
x.T2 < cfr.fisher   #False so we are in the rejection region, we have evidence to reject H0 at level 1%
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

x11()
plot(chicca, asp = 1)
ellipse(mu0, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)

points(x.mean[1], x.mean[2], pch = 16, col ='red', cex = 1.5) #true mean in the rejection region

#center [0 90]




#POINT B)

# Compute the p-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P

#POINT C)

### Bonferroni intervals on the components of the mean
### with global level 99%
###----------------------------------------------------
k <- p
cfr.t <- qt(1 - alpha/(k*2), n-1)

Bf <- cbind(inf = x.mean - cfr.t*sqrt(diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + cfr.t*sqrt(diag(x.cov)/n))
Bf

#The bonferroni interval for the mean of the delay does not contain the value 0, so this supports our decision
#to reject Ho at level 1%; while with bonferroni at level 99% we are not in the position to say that the stay
#is significantly different from 90

#point d)

a <- c(1, 1) #testing if the sum of delay and stay is different from 90 
alpha_2 <- 0.1

t.M.bis <- abs(x.mean%*%a-90)/sqrt((t(a)%*%x.cov%*%a)/n) #la statistica test per la combinazine lineare della media di una gaussiana
t.M.bis 

P   <- (1 - pt(t.M.bis, n-1))*2
P #really low p-value, evidence that we have to change the scheduling policy

#alternativa
lc <- chicca[,1] + chicca[,2]
t.test(lc, alternative = 'greater', mu = 90, conf.level = 0.90)


#EX 2 

#POINT a) 
med <- read.table('orthopaedics.txt', header=T)
head(med)

normality <- factor(med[,3], levels=c('NO', 'AB'))
normality

normal <- med[1:80,1:2]
abnormal <- med[81:150,1:2]

med2 <- med[,1:2]

library(MASS)
load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
mcshapiro.test(normal)
mcshapiro.test(abnormal) #both groups are normal 

fit <- manova(as.matrix(med2) ~ normality) #differences in mean between the two groups 
summary.manova(fit,test="Wilks")

S1 <- cov(normal) #covariances of the two groups
S2 <- cov(abnormal)
#I use a qda classifier since the covarinace matrices look different

qda.med <- qda(med2, normality,prior=c(0.65,0.35)) #set the prior as specified in the text 
qda.med #I see the group means from the qda table

#report the classification regions plot (inside the elliptic region are the ones classified as normal)
x11()
plot(med2, main='pelvic', xlab='pelvic.incidence', ylab='pelvic.tilt_angles', pch=20)
points(normal, col='red', pch=20)
points(abnormal, col='green', pch=20)
legend("topright", legend=levels(normality), fill=c('red','green'))

points(qda.med$means, col=c('red','green'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(normal), max(normal), length=200)
y  <- seq(min(abnormal), max(abnormal), length=200)
xy <- expand.grid(incidence=x, tilt=y)

z  <- predict(qda.med, xy)$post  
z1 <- z[,1] - z[,2]    
z2 <- z[,2] - z[,1] 

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T,lty =2)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T,lty =2)

points(60,0, pch=3, col='blue', lwd=2)

#point b) 
QdaCV <- qda(med2, normality, prior=c(0.65, 0.35), CV=T)
table(class.true=normality, class.assignedCV=QdaCV$class)

errorsqCV <- (8/80)*0.65 + (32/70)*0.35  #looking at the table and using the right priors
errorsqCV #0.225

#point c)
predict(qda.med, c(incidence=60, tilt=0)) #higher posterior for AB group, so I assign it to the abnormal group

points(60,0, pch=3, col='springgreen', lwd=2) 

#point d)
library(e1071)

dat <- data.frame(x=med2, y=as.factor (normality))
svmfit <- svm(y~., data=dat , kernel ='linear', cost =0.10, scale =FALSE )
summary(svmfit)

x11()
par(mfrow=c(1,2))
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

#FROM THE PLOT WE SEE THAT IN THIS CASE THE PATIENT AT POINT C WOULD BE CLASSIFIED AS NORMAL


#EX 3)

# point a) 

pc <- read.table('pc.txt', header=T)
pc

#creation of dummy variables
dmac <- rep(c(1,0), c(20,40))      # dmac = 1 if Mac,  0 otherwise
dwin<- rep(c(0,1,0), c(20,20,20)) # 1 if Windows

dati <- data.frame(pc,
                   dmac  = rep(c(1,0), c(20,40)),      # dummy for mav
                   dwin  = rep(c(0,1,0), c(20,20,20)))
dati

#Model:
# Reg = b.0+b1*f+b2*f*mac+b3*f*win+b4*c+b5*c*mac+b6*c*win + eps 

# Indeed:
# beta1.lin=b1;      beta1.LIN=b4;
# beta1.MAC=b1+b2;  beta1.mac=b4+b5;
# beta1.WIN=b1+b3; beta1.win=b4+b6

fit <- lm(price ~ freq + freq:dmac + freq:dwin + cache_acc + cache_acc:dmac + cache_acc:dwin, data=dati)
summary(fit)

#alpha = 1345.657
# beta.linux = 34.623
#beta.mac = 121.213
#beta.win = 76.203
#gamma.linux = -2.589
#gamma.mac = -4.239
#gamma.win = -9.003 

sum(residuals(fit)^2)/fit$df  #11497.83 estimate of sigma^2

#point b and c) 
par(mfrow=c(2,2)) #omoschedastic residuals, zero  mean
plot(fit)

shapiro.test(residuals(fit)) #normality assumptions to perform tests 

dev.off()

# 1. the variable operating system;
linearHypothesis(fit,
                 rbind(c(0,0,1,0,0,0,0),
                       c(0,0,0,1,0,0,0),
                       c(0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,1)),
                 c(0,0,0,0))  # very low p-value for the test, so we consider significant the coefficients
                              # for the operating system 

# 2. the variable cache_acc;
linearHypothesis(fit,
                 rbind(c(0,0,0,0,1,0,0),
                       c(0,0,0,0,0,1,0),
                       c(0,0,0,0,0,0,1)),
                       c(0,0,0))   #p-value 0.4727 so at 10% we dont'reject the null hypthesis that all 

#test for variable frequency                                  # the coefficients of cache_acc are different from 0 
linearHypothesis(fit,
                 rbind(c(0,1,0,0,0,0,0),
                       c(0,0,1,0,0,0,0),
                       c(0,0,0,1,0,0,0)),
                       c(0,0,0))  #low-pvalue, we keep the variable frequency in the model 

#effect of op system on frequency 
linearHypothesis(fit,
                 rbind(c(0,0,1,0,0,0,0),
                       c(0,0,0,1,0,0,0)),
                       c(0,0))   #at 10% we consider significant the impact of op system on the variable freq

#point d) 
fit2 <- lm(price ~ freq + freq:dmac + freq:dwin, data=dati)
summary(fit2)

#alpha = 1275.64
#freq.linux = 44.17
#freq. mac = 123.06
#freq. win = 59.93

new_data <- data.frame(freq=3.2,cache_acc=10,dmac=0,dwin=1)
IP <- predict(fit2, newdata=new_data, interval='confidence', level=1-0.1)
IP 
#confdence interval for the new observation 


#ex 4) 

head(colours)
attach(colours)

library(sp)           ## Data management
library(lattice)      ## Data management        ## Geostatistics
library(gstat)

# create 2 dummies: DR - 1 red - 0 others
                #   DO - 1 orange - 0 others 
DO <- rep(0,length(colours$colour))
DO[which(colour=='orange')] <- 1
DR <- rep(0,length(colours$colour))
DR[which(colour=='red')] <- 1
data <- data.frame(cbind(x,y,revenue,DO,DR,optional))
names(data) <- c('x','y','revenue','orange','red','opt')
coordinates(data) <- c('x','y')

#model divided by zones
v <- variogram(revenue ~ orange + red, data = data)
plot(v)
v.fit1 <- fit.variogram(v, vgm(6, "Sph", 3000))
plot(v, v.fit1, pch = 3)
v.fit1

#Model considering costant the area
v2 <- variogram(revenue ~ 1, data = data)
plot(v2)
v.fit2 <- fit.variogram(v2, vgm(50, "Sph", 500))
plot(v2, v.fit2, pch = 3)
v.fit2

#I would choose definitely the model with dependence in the colour (also singular model in v2)

#b and c)
g.tr <- gstat(formula = revenue ~ orange + red, data = data, model = v.fit1)
g.tr
g.tr2 <- gstat(formula = revenue ~ 1, data = data, model = v.fit2)

#a0,g
predict(g.tr, data[1,], BLUE = TRUE) #red
predict(g.tr, data[2,], BLUE = TRUE) #yellow 
predict(g.tr, data[4,], BLUE = TRUE) #orange
#coefficients without assuming difference between the colour (a0)
predict(g.tr2, data[1,], BLUE = TRUE)


#d) 

#yellow
s0.new <- as.data.frame(matrix(c(514811.55, 5037308.54,0,0,1),1,5))
names(s0.new) <- c('x','y','orange','red','opt')
coordinates(s0.new) <- c('x','y')
predict(g.tr, s0.new)

#orange
s0.new <- as.data.frame(matrix(c(514811.55, 5037308.54,1,0,1),1,5))
names(s0.new) <- c('x','y','orange','red','opt')
coordinates(s0.new) <- c('x','y')
predict(g.tr, s0.new)

#red
s0.new <- as.data.frame(matrix(c(514811.55, 5037308.54,0,1,1),1,5))
names(s0.new) <- c('x','y','orange','red','opt')
coordinates(s0.new) <- c('x','y')
predict(g.tr, s0.new)
