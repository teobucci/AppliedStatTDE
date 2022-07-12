rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("debris.txt",header=T)
n <- nrow(d)

d$risk <- as.factor(d$risk)
A = which(d$risk == 'H')
B = which(d$risk == 'L')

##### PUNTO A #####
plot(d[,1:2], col=as.numeric(d$risk))
# some kind of separation


#### Assumptions:
###------------------
# 1) if L=i, X.i ~ N(mu.i, sigma.i^2), i=A,B
# 2) sigma.A=sigma.B
# 3) c(A|B)=c(B|A) (equal misclassification costs)

# verify assumptions 1) e 2): 
# 1) normality within the groups
mcshapiro.test(d[A,1:2]) # 0.482
mcshapiro.test(d[B,1:2]) # 0

# 2) equal variance 
bartlett.test(d[A,1:2],d[B,1:2]) # p-value = 6.224e-09
# Let's go with qda


#### qda
###------------------
qda1 <- qda(d[,1:2], d$risk)
qda1



#Prior probabilities of groups:
#    H         L 
#0.4333333 0.5666667 


# Group means:
#x         y
#H 2.7830000 -1.233777
#L 0.5692588 -2.873935

cov(d[A,1:2])
#x        y
#x 1.058284 1.067964
#y 1.067964 3.017442

cov(d[B,1:2])
#x          y
#x 2.2028985  0.2054168
#y 0.2054168 15.6631038


# PLOT
x11()
plot(d[,1:2], pch=20)
points(d[A,1:2], col='red', pch=20)
points(d[B,1:2], col='green', pch=20)
legend("topright", legend=levels(d$risk), fill=c('red','green'), cex=.7)

points(qda1$means, pch=4,col=c('red','green') , lwd=2, cex=1.5)
x  <- seq(min(d[,1]), max(d[,1]), length=200)
y  <- seq(min(d[,2]), max(d[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(qda1, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

##### PUNTO B #####
# Normality not assumed, but qda is robust with respect to this assumption

qdaCV <- lda(d[,1:2], d$risk, CV=TRUE)  # specify the argument CV
table(class.true=d$risk, class.assignedCV=qdaCV$class)
errorsCV <- (qdaCV$class != d$risk)
AERCV   <- sum(errorsCV)/length(d$risk)
AERCV # 0.1933333



##### PUNTO C #####
set.seed(321)
err = NULL
k_grid = 10:30
for (k in k_grid) {
    knn_data <- knn.cv(train = d[,1:2], cl = d$risk, k = k)
    
    errorCV <- (knn_data != d$risk)
    err = c(err, sum(errorCV)/length(d$risk) )
}
err_min = min(err)
k_opt = k_grid[which.min(err)]
err_min # 0.0966667
k_opt # 20


k <- 20 #da definire
x <- seq(min(d[,1]), max(d[,1]), length=400)
y <- seq(min(d[,2]), max(d[,2]), length=400)
xy <- expand.grid(x,y)
knn_d = knn(train = d[,1:2], test =xy , cl = d$risk , k = k , prob = T)

attr = attributes(knn_d) # mi da levels,class e prob

knn.class <- (knn_d == 'H')+0 #al posto di L metti la classe della prima cosa che viene classficata!
knn.B <- ifelse(knn.class==1, attributes(knn_d)$prob, 1 - attributes(knn_d)$prob)



x11()
plot(d[,1:2], col = d$risk , pch = 20)
z = as.numeric(knn_d)
contour(x, y, matrix(z, 400), levels=c(1.5, 2.5), drawlabels=F, add=T)
colors = unique(d$risk)
legend("topleft", legend=unique(d$risk), fill=colors, cex=.7)






##### PUNTO D #####
data.new <- c(x=1,y=-4)
predict(qda1,data.new) # L  H: 0.3739417 L:0.6260583

knn_d = knn(train = d[,1:2], test =data.new , cl = d$risk , k = k , prob = T)
knn_d # H: 0.65


