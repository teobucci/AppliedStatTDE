rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("profiling.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
A <- which(d$type=="tourist")
B <- which(d$type=="resident")
#### Assumptions:
###------------------
# 1) if L=i, X.i ~ N(mu.i, sigma.i^2), i=A,B
# 2) sigma.A=sigma.B
# 3) c(A|B)=c(B|A) (equal misclassification costs)

# verify assumptions 1) e 2): 
# 1) normality (univariate) within the groups
mcshapiro.test(d[A,1:2]) #0.6996
mcshapiro.test(d[B,1:2]) #0.1192

# 2) equal variance (univariate)
bartlett.test(d[,1:2], d$type) #2.2e-16

# IT'S QDA TIME
d2 <- d[,1:2]
species.name <- d$type
qda.d <- qda(d2, species.name)
qda.d

# Prior probabilities of groups:
#resident   tourist 
# 0.7132146 0.2867854 

# Group means:
#           t1       t2
#resident 34.97363 15.01548
#tourist  45.11078 15.28026

cov(d2[A,])
#           t1       t2
#t1 39.261409 2.106733
#t2  2.106733 7.950719

cov(d2[B,])
#       t1        t2
#t1  9.984163 -7.989768
#t2 -7.989768  9.839547

x11()
plot(d2, main='Plot', pch=20)
points(d2[A,], col='red', pch=20)
points(d2[B,], col='green', pch=20)
legend("topright", legend=levels(as.factor(species.name)), fill=c('red','green'), cex=.7)

points(qda.d$means, pch=4,col=c('green','red') , lwd=2, cex=1.5)
x  <- seq(min(d[,1]), max(d[,1]), length=200)
y  <- seq(min(d[,2]), max(d[,2]), length=200)
xy <- expand.grid(Sepal.Length=x, Sepal.Width=y) #### CHANGE THIS ONE

z  <- predict(qda.d, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

##### PUNTO B #####

Qda.d <- predict(qda.d, d2)

table(class.true=species.name, class.assigned=Qda.d$class)
errorsq <- (Qda.d$class != species.name)
APERq   <- sum(errorsq)/length(species.name)
APERq # 0.0562343



##### PUNTO C #####

predict(qda.d,data.frame(t1=35,t2=3))

#$class
#[1] tourist


#$posterior
#resident   tourist
#[1,] 0.000154081 0.9998459

