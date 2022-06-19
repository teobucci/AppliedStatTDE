#PB 2) 

#point a) 

beans <- read.table('beans.txt', header=T)
head(beans)
dim(beans)

beans.label <- beans[,9]
beans <- beans[,-9]
n <- dim(beans)[1]
p <- dim(beans)[2]
# Boxplot
x11()
par(mar=rep(8,4))
boxplot(beans, las=2, col='gold') 

x11()
par(mar=rep(8,4))
boxplot(scale(x=beans,center = T, scale=F), las=2, col='gold')

beans.sd <- scale(beans)
beans.sd <- data.frame(beans.sd)

head(beans.sd) #better to standardize the variables 

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(beans.sd, las=2, col='gold')

pc.beans <- princomp(beans.sd, scores=T)
pc.beans
summary(pc.beans)

load.tour <- pc.beans$loadings
load.tour

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.tour[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

scores.beans <- pc.beans$scores
scores.beans

x11()
plot(scores.beans[,1:2])
abline(h=0, v=0, lty=2, col='grey')

x11()
layout(matrix(c(1,2),2))
boxplot(beans.sd, las=2, col='gold', main='Standardized variables')
scores.beans <- data.frame(scores.beans)
boxplot(scores.beans, las=2, col='gold', main='Principal components')

x11()
biplot(pc.beans)

# Let's use the categorical variables to further interpret the results
head(beans.label)

# Color according to Month
beans.label
# We order the labels according to time order
beans.label <- factor(beans.label, levels=c("adzuki","cannellini","black-eyed"))
col.ramp <- rainbow(3)
col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] = col.ramp[which(beans.label[i] == levels(beans.label))]
x11()
plot(scores.beans[,1:2], col=col.lab1, pch=19, xlim=c(-8,25), ylim=c(-3,3.2))
abline(h=-3, v=-8, col=1)
points(scores.beans[,1], rep(-3, n), col=col.lab1, pch=19)
points(rep(-8, n),scores.beans[,2], col=col.lab1, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(beans.label),fill=rainbow(3),bty='n')
graphics.off()
dev.off()
scores.beans[9]= beans.label
can <- which(scores.beans[9]=='cannellini')
can = scores.beans[can,]
can = can[,1:2]

# TEST PER LA MEDIA DI UNA GAUSSIANA CON P=2 ##
mcshapiro.test(can) ## controllo ipotesi -- iid e normalità 

#Hotelling theorem 
dim(can)
mu0      <- c(0, 0)
x.mean   <- colMeans(can) #oppure sapply(can,mean)
x.cov    <- cov(can)
x.invcov <- solve(x.cov)
n <- dim(can)[1]
p <- dim(can)[2]

alpha <- .05 #livello del test 

x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) # statistica test
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p) ## quantile fisher 
x.T2 < cfr.fisher #False mi indica rejection
Pb <- 1-pf(x.T2*(n-p)/(p*(n-1)), p, n-p) #p-value 

#visualizzazione grafica della regione di confidenza 
cfr.fisher <- (p*(n-1)/(n-p))*qf(1-alpha,p,n-p)
library(car)
x11()
plot(can)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)

axes <-eigen(x.cov/n)$vectors
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov/n)$values) 

#18/6/21 Pb.1 
holiday <- read.table('holiday.txt', header=T)
attach(holiday)

#1)
### Model with interaction (complete model): 
### X.ijk = mu + tau.i + beta.j + gamma.ij + eps.ijk; eps.ijk~N(0,sigma^2), 
###     i=1,2 (effect station), j=1,2 (effect gasoline)

# Verify the assumptions
# 1) normality (univariate) in each group
Ps <- c(shapiro.test(holiday$price[ type=='hotel' & location=='Chiavari' ])$p,
        shapiro.test(price[ type=='hotel' & location=='Rapallo' ])$p,
        shapiro.test(price[ type=='bb' & location=='Chiavari' ])$p,
        shapiro.test(price[ type=='bb' & location=='Rapallo' ])$p,
        shapiro.test(price[ type=='apt' & location=='Chiavari' ])$p,
        shapiro.test(price[ type=='apt' & location=='Rapallo' ])$p)

Ps
# 2) homogeneity of variances
bartlett.test(list(price[ type=='hotel' & location=='Chiavari' ],
                   price[ type=='hotel' & location=='Rapallo' ],
                   price[ type=='bb' & location=='Chiavari' ],
                   price[ type=='bb' & location=='Rapallo' ],
                   price[ type=='bb' & location=='Chiavari' ],
                   price[ type=='apt' & location=='Rapallo' ]))

fit.aov2.int <- aov(price ~ location + type + location:type)
summary.aov(fit.aov2.int)

#reduced
fit <- aov(price ~ type + location)
summary(fit)

final <-aov(price~type)
summary.aov(final)

#bonf
DF <- final$df # n*g*b - 1 - (g-1) = 15*3*2-1-2 = 90-3 = 87
Spooled <- sum(final$res^2)/DF

means <- as.vector(tapply(holiday$price, holiday$type, mean))
names(means) <- levels(type)
means

n <- dim(holiday)[1]
g <- 3
k <- g*(g-1)/2
S <- sum(residuals(final)^2)/(n-g)
cov <- tapply(holiday[,1], type, var)


alpha<- .05

Mg  <- tapply(holiday[,1], type, mean) 

label <- levels(factor(type))
n1 <- length(holiday[type==label[1],1])
n2 <- length(holiday[type==label[2],1])
n3 <- length(holiday[type==label[3],1])
t <- qt(1-alpha/(2*k),n-g)



# Conf int for the means
ICB1<-data.frame(L=Mg[1]-Mg[2]-sqrt(S*(2/n1))*t,C=Mg[1]-Mg[2],U=Mg[1]-Mg[2]+sqrt(S*2/n1)*t)
ICB2<-data.frame(L=Mg[2]-Mg[3]-sqrt(S*(2/n2))*t,C=Mg[2]-Mg[3],U=Mg[2]-Mg[3]+sqrt(S*2/n2)*t)
ICB3<-data.frame(L=Mg[3]-Mg[1]-sqrt(S*(2/n3))*t,C=Mg[3]-Mg[1],U=Mg[3]-Mg[1]+sqrt(S*2/n3)*t)
ICB<-data.frame(rbind(ICB1,ICB2,ICB3))
ICB

# Conf int for variances
chi_u <- qchisq(alpha/(2*k),n-g)
chi_l <- qchisq(1-alpha/(2*k),n-g)
ICBV <- data.frame(L=(n-g)*S/chi_l,C=S,U=(n-g)*S/chi_u)
ICBV


#############################################################################
# PB 3 18/6/2021 

#a) 
stud <- read.table('students.txt', header=T)
head(stud)
dim(stud)
gen = as.factor(stud$gender)

fit <- lm(watchtv ~ gen + age + height + distance + siblings + computertime + exercisehours +musiccds + playgames, data=stud)
summary(fit)

shapiro.test(residuals(fit))
x11(); par(mfrow=c(2,2)); plot(fit)

#b)
x <- model.matrix(stud$watchtv ~ gen + stud$age + stud$height + stud$distance + stud$siblings + stud$computertime + stud$exercisehours + stud$musiccds + stud$playgames)[,-1] # matrix of predictors
y <- stud$watchtv # vector of response

fit.lasso <- glmnet(x,y, lambda = 0.3, alpha=1) # alpha=1 -> lasso 

coef.lasso <- predict(fit.lasso, s=0.3, type = 'coefficients')[1:10,]
coef.lasso

#c
lambda.grid <- lambda.c <- seq(0.01,1,length=100)
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:10,]
coef.lasso

#d
num <-c(1,0,21,73,100,1,10,2,35,4)
coef.lasso %*% num

#Pb 4) 
data <- read.table('power.txt', header=T)
dim(data)
head(data)

NT <- dim(data)[1]
abscissa <- 1:365
Xobs0 <- data$power

plot(abscissa,Xobs0, type = "l")

## point a)

# generalized cross-validation
nbasis <- 6:50
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(range(abscissa), nbasis[i])
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v=nbasis[which.min(gcv)],col='red')

basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis[which.min(gcv)])
plot(basis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)


## point b)

# compute the central finite differences
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative

plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1bis,type='l',col="orange",lwd=3)


## point c)

# oversmoothing
nbasis <- 5
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)


## point d)

# overfitting
nbasis <- 50
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)

