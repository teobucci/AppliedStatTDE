#EX -1 

#a)
#point a) 

tourists <- read.table('tourists.txt', header=T)
head(tourists)
dim(tourists)

tourists.label <- tourists[,1:2]
tourists <- tourists[,-(1:2)]
n <- dim(tourists)[1]
p <- dim(tourists)[2]
# Boxplot
x11()
par(mar=rep(8,4))
boxplot(tourists, las=2, col='gold') 

x11()
par(mar=rep(8,4))
boxplot(scale(x=tourists,center = T, scale=F), las=2, col='gold')

tourists.sd <- scale(tourists)
tourists.sd <- data.frame(tourists.sd)

head(tourists.sd) #better to standardize the variables 

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(tourists.sd, las=2, col='gold')

pc.tourists <- princomp(tourists.sd, scores=T)
pc.tourists
summary(pc.tourists)

load.tour <- pc.tourists$loadings
load.tour

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.tour[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

#1 or 2 PC's seem sufficient to explain the variability of the data
#We have that first PCA is a average with similar weights of nigths spent considering all possible housing solution
#second PC is contrast between expensive solutions against cheaper ones 

scores.tourists <- pc.tourists$scores
scores.tourists

x11()
plot(scores.tourists[,1:2])
abline(h=0, v=0, lty=2, col='grey')

#High PC1 --- high number of people who spent the night in the city 
#High PC2 --- high number of people for expensive solutions, low for cheap solutions 
#low PC2 ---- low number of people for expensive solutions, high for cheap solutions

x11()
layout(matrix(c(1,2),2))
boxplot(tourists.sd, las=2, col='gold', main='Standardized variables')
scores.tourists <- data.frame(scores.tourists)
boxplot(scores.tourists, las=2, col='gold', main='Principal components')

x11()
biplot(pc.tourists)

# Let's use the categorical variables to further interpret the results
head(tourists.label)

# Color according to Month
tourists.label[,1]
# We order the labels according to time order
tourists.label[,1] <- factor(tourists.label[,1], levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))
col.ramp <- rainbow(12)
col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] = col.ramp[which(tourists.label[i,1] == levels(tourists.label[,1]))]

x11()
plot(scores.tourists[,1:2], col=col.lab1, pch=19, xlim=c(-8,25), ylim=c(-3,3.2))
abline(h=-3, v=-8, col=1)
points(scores.tourists[,1], rep(-3, n), col=col.lab1, pch=19)
points(rep(-8, n),scores.tourists[,2], col=col.lab1, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(tourists.label[,1]),fill=rainbow(12),bty='n')

#Scores depending on region of origin 
tourists.label[,2]
col.ramp <- rainbow(20)
col.lab2 <- rep(NA, n)
for(i in 1:n)
  col.lab2[i] = col.ramp[which(tourists.label[i,2] == levels(factor(tourists.label[,2])))]

x11()
plot(scores.tourists[,1:2], col=col.lab2, pch=19, xlim=c(-8,30), ylim=c(-3,3.2))
abline(h=-3, v=-8, col=1)
points(scores.tourists[,1], rep(-3, n), col=col.lab2, pch=19)
points(rep(-8, n),scores.tourists[,2], col=col.lab2, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(factor(tourists.label[,2])),fill=rainbow(20),bty='n')

#c) Since the two first principal components explain around 94% of the variability and both the 
#new two components are quite easily interpretable, we can reduce our dataset and work only on the
#new dataset built through the first two pcs 
















#EX -2

#a) 
attach(horsecolic)

#dividing into groups 
pain<-horsecolic[which(Pain=='Yes'),][,-5]  
nopain<-horsecolic[which(Pain=='No'),][,-5]    

p  <- 4
n1 <- dim(pain)[1]
n2 <- dim(nopain)[1]

# Verify gaussianity
load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
mcshapiro.test(pain)
mcshapiro.test(nopain)  #verified

#should check also same covariance matrix of the two groups 

# Test for independent Gaussian populations
p.mean <- sapply(pain,mean)
nop.mean <- sapply(nopain,mean)
p.cov  <-  cov(pain)
nop.cov  <-  cov(nopain)
Sp      <- ((n1-1)*p.cov + (n2-1)*nop.cov)/(n1+n2-2)

p<-4
alpha <- 0.01
IC <- cbind(p.mean-nop.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            p.mean-nop.mean,
            p.mean-nop.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
IC
#The intervals rectal.temperature and packed.cell.volume contain the value 0, so for these variables 
#there's no statistical difference (at global level 99%) in the mean between the horses in pain and the others

#Pulse and Respiratoy are significant variables 

#b) 

#lda -- normality in groups and same covariance matrix 

classpain <- horsecolic[,-1]
classpain <- classpain[,-3]

Pain <- as.factor(Pain)
#creating the dataset for classification - just two variables
pain <- pain[,-3] 
nopain <- nopain[,-3] 
pain <- pain[,-1]
nopain <- nopain [,-1]

library(MASS)
lda.pain <- lda(classpain, Pain)
lda.pain
#priors estimated from the sample

x11()
plot(classpain, main='pain', xlab='pulse', ylab='respiratory rate', pch=20)
points(pain, col='red', pch=20)
points(nopain, col='green', pch=20)
legend("topright", legend=levels(Pain), fill=c('red','green'))

points(lda.pain$means, col=c('red','green'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(pain), max(pain), length=200)
y  <- seq(min(nopain), max(nopain), length=200)
xy <- expand.grid(Pulse=x, Respiratory.rate=y)

z  <- predict(lda.pain, xy)$post  
z1 <- z[,1] - z[,2]    
z2 <- z[,2] - z[,1] 

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T,lty =2)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T,lty =2)

#c) 
Lda.pain <- predict(lda.pain, classpain)


#Compute the APER
Lda.pain$class   # assigned classes
Pain     # true labels
table(class.true=Pain, class.assigned=Lda.pain$class)

errors <- (Lda.pain$class != Pain)
errors
sum(errors)
length(Pain)

APER   <- sum(errors)/length(Pain)
APER

#EX 3)

#a)
load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
library(car)

castle <- read.table(file='castle.txt', header=T)
head(castle)
dim(castle)

n <- dim(castle)[1]
p <- dim(castle)[2]

#normal assumptions 
mcshapiro.test(castle) #p-value of the test around 0.2 so we can confirm the assumptions

alpha <- 0.05
mu0 <- c(45.733,7.333)

x.mean   <- sapply(castle,mean) #value [45.73/7.33]
x.cov    <- cov(castle)
x.invcov <- solve(x.cov)

# T2 Statistics
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0)  #value 2.76
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)  #T2 statistic follows a fisher distribution  
# Test: 
x.T2 < cfr.fisher   #True so we are not in the rejection region, we don't have evidence to reject H0 at level 5%
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

x11()
plot(castle, asp = 1)
ellipse(mu0, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)

points(x.mean[1], x.mean[2], pch = 16, col ='red', cex = 1.5) #true mean in the acceptance region

P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P #we confirm at any level of the test that the center is the center of Aosta 

#b) Confidence region of level 95%

# Center:
x.mean

# Directions of the principal axes:
eigen(x.cov)$vectors

# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov)$values)

x11()
plot(castle, asp = 1)
ellipse(x.mean, shape=x.cov, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)

#EX 4) 

#a) 
alb <- read.table('albatross.txt', header=T)
head(alb)
dim(alb)
attach(alb)
#creation of dummy variables
dupw <- rep(c(1,0), c(50,50))      # dupw = 1 if upwind,  0 otherwise


dati <- data.frame(distance,
                   Vi = Vi^2,
                   Va = Va^2,
                   dupw)
dati

#Model:
# dist = a0 + a1*upw + b0*va^2 + b1*va^2*upw + gamma0*vi^2 + gamma1*upw*vi^2 +eps 

# Indeed:
# alpha1 = a0 +a1;      
# beta1 = b0 + b1;  
# gamma1 = gamma0 + gamma1; 

fit <- lm(distance ~ Vi + Vi:dupw + Va + Va:dupw + dupw, data=dati)
summary(fit)

sum(residuals(fit)^2)/fit$df  #3.23 est8365 estimate of sigma^2

par(mfrow=c(2,2))#omoschedastic residuals, zero  mean
plot(fit)

shapiro.test(residuals(fit)) #normality assumptions to perform tests 

#b) 
linearHypothesis(fit,
                 c(0,0,0,1,0,0),
                 0)      
#we can eliminate the difference in the intercept of the model between the two groups 
fit2 <- lm(distance ~ Vi + Vi:dupw + Va + Va:dupw, data=dati)
summary(fit2)

#c) 

linearHypothesis(fit2,
                 c(0,1,1,0,0),
                       0)
linearHypothesis(fit2,
                 c(0,1,1,1,1),
                 0)                #we see that we can affirm betag + gammag = 0


fit3 <- lm(distance ~ I(Vi - Va) + I(Vi-Va):dupw,data = dati)
summary(fit3)
#model built with the constraint found on the test above 

#d) 
new_data <- data.frame(Vi=25^2,Va=35^2,dupw=c(1,0))
IP <- predict(fit3, newdata=new_data, interval='prediction', level=1-0.01/2)
rownames(IP) <- c('Up','Down')
IP 
#landing not safe at this level in case of downwind since the interval is up to 21m
#while in case of upwind we can consider the landing safe since the upper bound is just below 17m 
