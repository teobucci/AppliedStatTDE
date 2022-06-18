rm(list=ls())
data <- read.table('occupancy.txt', header=T)
n <- dim(data)[1]
p <- dim(data)[2]
head(data)

data$X <- factor(data$X)
levels(data$X)

# ------------------------- point 1

occ.y <- which(data$X == '0')
occ.n <- which(data$X == '1')

# LDA

# verify assumptions 1) e 2): 

# 1) normality (univariate) within the groups
mcshapiro.test(data[occ.y,1:2])$p # 0.8544 ok
mcshapiro.test(data[occ.n,1:2])$p # 0.4292 ok

# 2) same covariance structure (= same covariance matrix Sigma)
S1 <- cov(data[occ.y,1:2])
S2 <- cov(data[occ.n,1:2])

# Qualitatively:
round(S1,digits=1)
round(S2,digits=1)

par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))

# F
# very different, let's go with QDA


par(mfrow=c(1,1))
plot(data[,1:2], col = data$X)

library(MASS)
prior <- c(15/24, 9/24)
qda.trad <- qda(data[,1:2], data$X, prior=prior)
qda.trad

plot(data[,1:2], main='data', pch=20, col = data$X)
points(qda.trad$means, col=c('black','red'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(data[,1]), max(data[,1]), length=200)
y  <- seq(min(data[,2]), max(data[,2]), length=200)
xy <- expand.grid(x, y)

z  <- predict(qda.trad, xy)$post
z1 <- z[,1] - pmax(z[,2])    
z2 <- z[,2] - pmax(z[,1])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


# ------------------------- point 2

# APER
Qda.pred <- predict(qda.trad, data[,1:2])
misc <- table(class.true=data$X, class.assigned=Qda.pred$class)
misc

APER  <- misc[1,2]*prior[1]/sum(misc[1,]) + misc[2,1]*prior[2]/sum(misc[2,])
# 0.046875

# ------------------------- point 3

new <- data.frame(Humidity = 26, CO2 = 9)
predict(qda.trad, new)$posterior
predict(qda.trad, new)$class
# occupancy

# ------------------------- point 4

library(class)
data.knn <- knn(train = data[,1:2], test = xy, k = 5, cl = data$X)
plot(xy, col=data.knn)
plot(data[,1:2], col = data$X, pch = 19)
z <- as.numeric(data.knn)
contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)

# error

data.knn.train <- knn(train = data[,1:2], test = data[,1:2], k = 5, cl = data$X)
misclassif.error <- length(which(data$X != data.knn.train)) / n
misclassif.error


































