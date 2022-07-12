rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("activity.txt",header=T)
n <- nrow(d)
d$activity <- as.factor(d$activity)


##### PUNTO A #####
plot(d[,1:2],col=as.factor(d$activity))
A <- which(d$activity=="walking")
C <- which(d$activity=="laying")
B <- which(d$activity=="sitting")

# verify assumptions 1) e 2): 
# 1) normality (univariate) within the groups
mcshapiro.test(d[A,1:2]) #  0.4148
mcshapiro.test(d[B,1:2]) # 0.424
mcshapiro.test(d[C,1:2]) # 0.2536

# 2) equal variance (univariate)
bartlett.test(d[,1:2],d$activity) #0.4079

# Let's try LDA
priors <- c(3/24,12/24,9/24)
iris2 <- d[,1:2]
species.name <- d$activity
lda.iris <- lda(iris2, species.name, prior=priors)
lda.iris

#  Prior probabilities of groups:
#laying   sitting   walking 
#0.125   0.500   0.375 

#Group means:
#         accel      gyro
#laying  0.2147333 0.1474000
#sitting 0.3588000 0.3376667
#walking 0.6877333 0.6447333

x11()
plot(iris2, pch=20)
points(iris2[A,], col='red', pch=20)
points(iris2[B,], col='green', pch=20)
points(iris2[C,], col='blue', pch=20)
legend("topright", legend=levels(species.name), fill=c('red','green','blue'), cex=.7)

points(lda.iris$means, pch=4,col=c('blue','green','red') , lwd=2, cex=1.5)
x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(accel=x, gyro=y)

z  <- predict(lda.iris, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2],z[,3])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1],z[,3])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    
z3 <- z[,3] - pmax(z[,1],z[,2])

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)





##### PUNTO B #####
# 1) Compute the APER
table(class.true=species.name, class.assigned=Lda.iris$class)
APER   <- (41*priors[3])/(41+109) + (12*priors[1])/(12+138)
APER # 0.1125





##### PUNTO C #####
data.new <- data.frame(accel=0.45,gyro=0.52)
predict(lda.iris,data.new)


# sitting
#     laying  sitting    walking
#0.0004033331 0.912808 0.08678869

##### PUNTO D #####
library(MASS)
library(e1071)
library(class)
k <- 5

x11()
plot(iris2, main='Plot', xlab='x1', ylab='x2', pch=20)
points(iris2[A,], col=2, pch=20)
points(iris2[B,], col=3, pch=20)
points(iris2[C,], col=4, pch=20)
legend("topright", legend=levels(species.name), fill=c(2,3,4))

x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(accel=x, gyro=y) ### CHANGE THIS

iris.knn <- knn(train = iris2, test = xy, cl = d$activity, k = k)
z  <- as.numeric(iris.knn)
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5,3.5), drawlabels=F, add=T)




knn_for_pred = knn(train = d[,1:2], test = d[,1:2], cl = d$activity, k = k , prob = T)
err_rate = sum(knn_for_pred != d$activity)/length(d$activity)
err_rate # 0.03555556

# knn is way better




