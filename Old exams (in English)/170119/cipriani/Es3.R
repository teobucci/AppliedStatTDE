rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("trading.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
qda.iris <- qda(d[,1:2], d$gain)
qda.iris


#Prior probabilities of groups:
##    0         1 
#0.4661355 0.5338645 

#Group means:
#    google      apple
#0 -3.736068 -0.8300940
#1  5.116269  0.7081866

Qda.iris <- predict(qda.iris, d[,1:2])


# Plot partition
x11()
plot(d[,1:2], main='Plot', pch=20)
points(d[d$gain==0,1:2], col='red', pch=20)
points(d[d$gain==1,1:2], col='green', pch=20)
legend("topright", legend=levels(d$gain), fill=c('red','green'), cex=.7)

points(qda.iris$means, pch=4,col=c('red','green') , lwd=2, cex=1.5)
x  <- seq(min(d[,1]), max(d[,1]), length=200)
y  <- seq(min(d[,2]), max(d[,2]), length=200)
xy <- expand.grid(google=x, apple=y) #### CHANGE THIS ONE

z  <- predict(qda.iris, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)





##### PUNTO B #####
# Compute the estimate of the AER by leave-out-out cross-validation 
QdaCV.iris <- qda(d[,1:2], d$gain, CV=T)
table(class.true=d$gain, class.assignedCV=QdaCV.iris$class)
errorsqCV <- (QdaCV.iris$class != d$gain)
AERqCV   <- sum(errorsqCV)/length(d$gain)
AERqCV # 0.3227092
# Still better than trivial classifier (0.5)



##### PUNTO C #####
data.new <- data.frame(google=-3,apple=-1.2)
predict(qda.iris,data.new)

# NO: 0
#    0        1
# 0.698252 0.301748


