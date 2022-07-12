rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d1 <- read.table("beans.txt",header=T)
n <- nrow(d)


d <- d1[,1:8]

##### PUNTO A #####

x11()
boxplot(d, las=1, col='red', main='Boxplot',grid=T, center=T)
# scale
d <- scale(d)


pca <- princomp(d, scores=T)
pca 
summary(pca)

##### PUNTO B #####

load <- pca$loadings
a = 2 # number of principal components to be interpreted, change accordingly

x11()
par(mar = c(1,4,0,2), mfrow = c(a,1))
for(i in 1:a)
    barplot(load[,i], ylim = c(-1, 1))

# PC1: weighted mean of everything except eccentricity and roundnes
# PC2: high eccentricity low roundness mainly


x11()
biplot(pca, scale=0, cex=.7)


plot(pca$scores[,1], pca$scores[,2], col=as.numeric(as.factor(d1$Type))+1)
legend("topright", legend=c('Adzuki','Black-eyed','Cannellini'), fill=c(2,3,4), cex=.7)

# The first PC is dividing very well the types of beans in the three categories

##### PUNTO C #####
x <- data.frame(pca$scores[d1$Type=='cannellini',1:2])

x.mean   <- sapply(x,mean)
x.cov    <- cov(x)
x.invcov <- solve(x.cov)
n <- nrow(x) 
p <- ncol(x)  # CHANGE TO DIMENSION OF MULTIVARIETY 

alpha <- 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
plot(x, asp = 1)
ellipse(x.mean[], shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
points(x.mean[1], x.mean[2], pch = 16, col ='red', cex = 1.5) # We add a red point in correspondence of the sample mean
# { m \in R^2 s.t. n * (x.mean-m)' %*% (x.cov)^-1 %*% (x.mean-m) < cfr.fisher }


# Characterize region
# Center:
x.mean # -2.9513269  0.4281307 

# Directions of the principal axes:
eigen(x.cov/n)$vectors

# 0.2570354 -0.9664020
# 0.9664020  0.2570354


# Length of the semi-axes of the ellipse:
r <- sqrt(cfr.fisher)
r*sqrt(eigen(x.cov/n)$values) # 0.3847169 0.1666413
