#EX 1) 

#a)
#hypothesis of independence between the two normal bivariate population 
#normality (which the text says we can assume)
#same covariance 
#work under Hotelling's theorem 

t1 <- read.table("terrassa.txt")
t2 <- read.table("girona.txt")

load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
mcshapiro.test(t1) #low p-value but we proceed anyway 
mcshapiro.test(t2)


n1 <- dim(t1)[1] 
n2 <- dim(t2)[1] 
p  <- dim(t1)[2]

t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)

alpha   <- .05
delta.0 <- c(0,0)
Spinv   <- solve(Sp)

T2 <- n1*n2/(n1+n2) * (t1.mean-t2.mean-delta.0) %*% Spinv %*% (t1.mean-t2.mean-delta.0)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # False: statistical evidence to reject H0 at level 5%

P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P  


#b)
IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(2*p), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(2*p), n1+n2-2))
IC
#we see that both the difference between the mean values of the tasters in different cities doesn't contain the value 0,
#so we realize with Bonf correction at global 5% that there's an impact of the city on the evaluation 

#c) 
a = as.matrix(c(1/2,1/2))
a

d = as.matrix(t1)%*%a
d2 = as.matrix(t2)%*%a

shapiro.test(d)
shapiro.test(d2) #both normal populations ,this time univariate 

var.test(d,d2) #we can consider the same variance 

n1 <- dim(d)[1] # n1=3
n2 <- dim(d2)[1] # n2=4
p  <- 1

t1.mean <- mean(d)
t2.mean <- mean(d2)
t1.cov  <-  var(as.data.frame(d))
t2.cov  <-  var(as.data.frame(d2))

a = as.matrix(c(1/2,1/2))
a

d = as.matrix(t1)%*%a
d2 = as.matrix(t2)%*%a

shapiro.test(d)
shapiro.test(d2)

n1 <- dim(d)[1] # n1=3
n2 <- dim(d2)[1] # n2=4
p  <- 1

t1.mean <- mean(d)
t2.mean <- mean(d2)
t1.cov  <-  var(as.data.frame(d))
t2.cov  <-  var(as.data.frame(d2))
var.test(d,d2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)


alpha   <- .05
delta.0 <- 0
Spinv   <- solve(Sp)
qt(1-alpha/2,68)
t.test(d2,d,alternative ="greater",var.equal = TRUE) #Test univariate to see if mean of d2 is statically larger than the one of d
#EX 2

#a) 
attach(profiling)
pclass <- profiling[,-3]

type <- as.factor(type)
plot(profiling)

tourists <- which(type=='tourist')
residents <- which(type=='resident')

plot(pclass, main='TIME', xlab='walking', ylab='waiting', pch=19)
points(pclass[residents,], col='red', pch=19)
points(pclass[tourists,], col='green', pch=19)
legend("topright", legend=levels(type), fill=c('red','green'))

load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
mcshapiro.test(pclass[residents,])
mcshapiro.test(pclass[tourists,]) #both groups are normal 

fit <- manova(as.matrix(pclass) ~ type) #differences in mean between the two groups 
summary.manova(fit,test="Wilks")

S1 <- cov(pclass[residents,]) #covariances of the two groups
S2 <- cov(pclass[tourists,])
#I use a qda classifier since the covarinace matrices look different

qda.prof <- qda(pclass, type) #set the prior as specified in the text 
qda.prof #I see the group means from the qda table

x11()
plot(pclass, main='TIME', xlab='walking', ylab='waiting', pch=19)
points(pclass[residents,], col='red', pch=19)
points(pclass[tourists,], col='green', pch=19)
legend("topright", legend=levels(type), fill=c('red','green'))

points(qda.prof$means, col=c('red','green'), pch=4, lwd=2, cex=1.5)

x  <- seq(1, max(pclass[residents,]), length=200)
y  <- seq(3, max(pclass[tourists,]), length=200)
xy <- expand.grid(t1=x, t2=y)

z  <- predict(qda.prof, xy)$post  
z1 <- z[,1] - z[,2]    
z2 <- z[,2] - z[,1] 

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T,lty =2)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T,lty =2)



#b)
profiled <- predict(qda.prof, pclass)
names(profiled)

profiled$class   # assigned classes
type     # true labels
table(class.true=type, class.assigned=profiled$class)

errors <- (profiled$class != type)
errors
sum(errors)
length(type)

APER   <- sum(errors)/length(type)
APER  #5%

#c) 
predict(qda.prof, c(t1=35, t2=3))
#hogher posterior for tourist 

#EX 3)
attach(airport)
#a) 
#Creating dummy variables
dmat <- rep(c(1,0), c(53,115))      # dFr = 1 if France,  0 otherwise
dpom<- rep(c(0,1,0), c(53,48,67))

# Equivalent Model:
# time = b.0+b.1*dmat+b.2*pom +b.3*dist+b.4*dmat*dist+b.5*dpom*dist + eps

# Indeed:
# beta0.sera=b.0;      beta1.sera=b.3;
# beta0.mat=b.0+b.1;  beta1.mat=b.3+b.4;
# beta0.pom=b.0+b.2; beta1.pom=b.3+b.5

dati <- data.frame(time  = duration,
                   dist = distance,
                   dmat,
                   dpom)
dati

fit <- lm(time ~ dmat + dpom + dist + dist:dmat + dist:dpom, data=dati)
summary(fit)

#paramters divided in groups as shown above 

sum(residuals(fit)^2)/fit$df  #30.7923 estimate of sigma^2

par(mfrow=c(2,2)) #omoschedastic residuals, zero  mean
plot(fit)

shapiro.test(residuals(fit)) #normality assumptions to perform tests 

dev.off()

#b) effect of time of the day 
linearHypothesis(fit,
                 rbind(c(0,1,0,0,0,0),
                       c(0,0,1,0,0,0),
                       c(0,0,0,0,0,1),
                       c(0,0,0,0,1,0)),
                 c(0,0,0,0))  #at least one variable related to the effect of time of the day is significant 

#effect of distance 
linearHypothesis(fit,
                 rbind(c(0,0,0,1,0,0),
                       c(0,0,0,0,1,0),
                       c(0,0,0,0,0,1)),
                       c(0,0,0))  #at least one variable related to the distance is significant in the model

#effect of time of the day on the the variable distance
linearHypothesis(fit,
                 rbind(c(0,0,0,0,0,1),
                       c(0,0,0,0,1,0)),
                 c(0,0))  #at level 1% we don't have evidence to say that these parameters are different from 0, but at 5% we might say it 

#effect of intercept
linearHypothesis(fit,
                 rbind(c(0,1,0,0,0,0),
                       c(0,0,1,0,0,0),
                       c(1,0,0,0,0,0)),
                 c(0,0,0))   #no evidence to assume it different from 0 

fit2 <- lm(time ~ 0 + dist + dist:dmat + dist:dpom, data=dati)
summary(fit2)

par(mar=c(1,1,1,1))
par(mfrow=c(2,2))#omoschedastic residuals, zero  mean
plot(fit2)


#D) 
new_data <- data.frame(dist=57,dmat=1,dpom=0)
IP <- predict(fit2, newdata=new_data, interval='prediction', level=1-0.01)

#I must leave at 7 o'clock to be sure at 99% to arrive at 9:30 at the airport, since the 
#upper bound of the interval is 2h 6minutes, so leaving at 7:30 would not guaranteed that

#ex 4)

data=read.table('montserrat.txt')
names(data)[3]='speed'
attach(data)
coordinates(data)=c('x','y')

v=variogram(speed ~ 1, data=data)
plot(v,pch=19)

v.t=variogram(speed ~ distance, data=data)
plot(v.t,pch=19)

#b)
v.fit2 <- fit.variogram(v.t, vgm(8, "Sph", 30))
plot(v.t, v.fit2, pch = 3)
v.fit2

#c)
g.tr <- gstat(formula = speed ~ distance, data = data, model = v.fit2)
a1 = (predict(g.tr, data[2,], BLUE = TRUE)$var1.pred- predict(g.tr, data[1,], BLUE = TRUE)$var1.pred)/(montserrat[2,4]-montserrat[1,4])
a0= predict(g.tr, data[2,], BLUE = TRUE)$var1.pred - a1*montserrat[2,4]
#system between two generalized prediction to find coefficients ao and a1 

#d) 
s0.new <- as.data.frame(matrix(c(402476,4605558,0),1,3))
names(s0.new) <- c('x','y','distance')
coordinates(s0.new) <- c('x','y')
predict(g.tr, s0.new)
