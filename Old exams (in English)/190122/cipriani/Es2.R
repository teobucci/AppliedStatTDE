rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("fish.txt",header=T)
n <- nrow(d)
d$abundance <- as.factor(d$abundance)
species.name <- d$abundance

##### PUNTO A #####
plot(d[,1:2],col=d$abundance)
g=2 
i1 <- which(species.name=='H')
i2 <- which(species.name=='L')

# verify assumptions 1) e 2): 
# 1) normality (univariate) within the groups
mcshapiro.test(d[i1,1:2]) #0.10
mcshapiro.test(d[i2,1:2]) #0.87

# 2) equal variance (univariate)
bartlett.test(d[,1:2],species.name) # 2.2e-16
# Let's go with qda

iris2 <- d[,1:2]
qda.iris <- qda(iris2, species.name)
qda.iris
Qda.iris <- predict(qda.iris, iris2)

#Prior probabilities of groups:
#    H    L 
#0.48 0.52 

#Group means:
#    x        y
#H 10.61727 39.92299
#L 12.14345 39.56668

cov(d[i1,1:2]) # H

#x           y
#x  0.45351455 -0.03940145
#y -0.03940145  0.01724919

cov(d[i2,1:2]) # L
#x          y
#x  0.5105541 -0.1367735
#y -0.1367735  0.1177096

x11()
plot(iris2, main='Plot', pch=20)
points(iris2[i1,], col='red', pch=20)
points(iris2[i2,], col='green', pch=20)
legend("topright", legend=levels(species.name), fill=c('red','green'), cex=.7)

points(qda.iris$means, pch=4,col=c('red','green') , lwd=2, cex=1.5)
x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(x=x, y=y) #### CHANGE THIS ONE

z  <- predict(qda.iris, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


##### PUNTO B #####
# No particular weaknesses, group H not so gaussian maybe
QdaCV.iris <- qda(iris2, species.name, CV=T)
table(class.true=species.name, class.assignedCV=QdaCV.iris$class)
errorsqCV <- (QdaCV.iris$class != species.name)
AERqCV   <- sum(errorsqCV)/length(species.name)
AERqCV
# 0.1


##### PUNTO C #####
set.seed(19)

AER_cv=NULL
k_grid  <- 10:30
n = dim(d)[1]
for(j in k_grid){
    errors_cv = 0
    for(i in 1:n){
        knn_d <- knn(train = d[-i,1:2], test = d[i,1:2], cl = d[-i,]$abundance, k = j , prob = T)
        if(knn_d!=d[i,]$abundance)
            errors_cv <- errors_cv +1
    }
    AER_cv   <- c(AER_cv,sum(errors_cv)/n)
}
rbind(k_grid,AER_cv)
aer_min = min(AER_cv)
k_opt=k_grid[which.min(AER_cv)]
aer_min # 0.128
k_opt # 13

library(class)

k <- 13

x11()
plot(iris2, main='Plot', xlab='x1', ylab='x2', pch=20)
points(iris2[i1,], col=2, pch=20)
points(iris2[i2,], col=3, pch=20)
points(iris2[i3,], col=4, pch=20)
legend("topright", legend=levels(species.name), fill=c(2,3,4))

x  <- seq(min(iris2[,1]), max(iris2[,1]), length=200)
y  <- seq(min(iris2[,2]), max(iris2[,2]), length=200)
xy <- expand.grid(Sepal.Length=x, Sepal.Width=y) ### CHANGE THIS

iris.knn <- knn(train = iris2, test = xy, cl = species.name, k = k)
z  <- as.numeric(iris.knn)
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)



##### PUNTO D #####
predict(qda.iris, data.frame(x=10.8, y=39.4))

#$class
 #L
#$posterior
#H         L
#0.006722207 0.9932778





