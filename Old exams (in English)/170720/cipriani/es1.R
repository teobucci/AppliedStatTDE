rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("occupancy.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
plot(d[,1:2], col=d$X +1)
# There seems to be a difference

# Prior, occupied = 9/24
p_1 = 9/24
p_0 = 1-(9/24)

A <- which(d$X == 0)
B <- which(d$X == 1)
mcshapiro.test(d[A,1:2]) #0.8452
mcshapiro.test(d[B,1:2]) #0.4212

# 2) equal variance (univariate)
bartlett.test(d[A,1:2],d[B,1:2]) #p-value = 0.2725
bartlett.test(d[,1:2],d$X)

# assumptions for LDA, QDA satisfied

###LDA
lda1 <- lda(d[,1:2], d$X, prior = c(p_0,p_1))
lda1
Lda <- predict(lda1, d[,1:2])

table(class.true=d$X, class.assigned=Lda$class)
errors <- (Lda$class != d$X)
sum(errors) #10


###QDA
qda1 <- qda(d[,1:2], d$X, prior = c(p_0,p_1))
qda1
Qda <- predict(qda1, d[,1:2])

table(class.true=d$X, class.assigned=Qda$class)
errors <- (Qda$class != d$X)
sum(errors) #6
# Let's go with QDA


# Model parameters
# Group means:
#  Humidity      CO2
#0 24.53469 5.128682
#1 26.36389 9.578757

# Cov of p_0
cov(d[A,1:2])
#Humidity      CO2
#Humidity 0.8097969 0.414999
#CO2      0.4149990 1.154485

# Cov of p_1
cov(d[B,1:2])
#          Humidity      CO2
#Humidity 2.726594 1.526467
# CO2      1.526467 3.666431

# PLOT
x11()
plot(d[,1:2], pch=20)
points(d[A,1:2], col='red', pch=20)
points(d[B,1:2], col='green', pch=20)
legend("topright", legend=levels(d$X), fill=c('red','green'), cex=.7)

points(qda1$means, pch=4,col=c('red','green') , lwd=2, cex=1.5)
x  <- seq(min(d[,1]), max(d[,1]), length=200)
y  <- seq(min(d[,2]), max(d[,2]), length=200)
xy <- expand.grid(Humidity=x, CO2=y)

z  <- predict(qda1, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


##### PUNTO B #####
table(class.true=d$X, class.assigned=Qda$class)
APER   <- (1*p_0)/40 + (5*p_1)/60
APER # 0.046875



##### PUNTO C #####
new.data <- data.frame(Humidity=26,CO2=9)
predict(qda1,new.data)

#Occupied: 1
# 0.008318664 0.9916813


##### PUNTO D #####
k <- 5
x <- seq(min(d[,1]), max(d[,1]), length=400)
y <- seq(min(d[,2]), max(d[,2]), length=400)
xy <- expand.grid(x,y)
knn_d = knn(train = d[,1:2], test =xy , cl = d$X , k = k , prob = T)

attr = attributes(knn_d) 
attr
knn.class <- (knn_d == '0')
knn.B <- ifelse(knn.class==1, attributes(knn_d)$prob, 1 - attributes(knn_d)$prob)

knn_for_pred = knn(train = d[,1:2], test = d[,1:2], cl = d$X, k = k , prob = T)
err_rate = sum(knn_for_pred != d$X)/length(d$X)
err_rate #0.06

table(class.assigned = knn_for_pred, class.true = d$X)

x11()
plot(d[,1:2], col = d$X+1, pch = 20)
z = as.numeric(knn_d)
contour(x, y, matrix(z, 400), levels=c(1.5, 2.5), drawlabels=F, add=T)
colors = unique(d$X) +1
legend("topleft", legend=unique(d$X)+1, fill=colors, cex=.7)





