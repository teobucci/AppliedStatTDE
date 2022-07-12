rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("demogorgons.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
plot(d)
d.e <- dist(d, method='euclidean')


#### CLUSTERING
###-------------------
d.ea <- hclust(d.e, method='average')


#### DENDROGRAMS
###-------------------
plot(d.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')


plot(d.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(d.ea, k=2)
#### Look also at the scatter plot before deciding where to cut




#### CUTTING
###-------------------
# Fix k=2 clusters:
cluster.ea <- cutree(d.ea, k=2) # euclidean-average

coph.ea <- cophenetic(d.ea)

ea <- cor(d.e, coph.ea)
ea



#### NUMEROSITY OF CLUSTERS
###-------------------
g <- 2
i1 <- which(cluster.ea==1)
i2 <- which(cluster.ea==2)
ng <- c(length(i1),length(i2)) 
ng 
N <- sum(ng)

plot(d,col=cluster.ea)







##### PUNTO B #####

mcshapiro.test(d[i1,]) # 0.6228
mcshapiro.test(d[i2,]) # 0.8584
clust <- rep(0,n)
d.new <- cbind(d,clust) # creating categorical feature with cluster labels
d.new$clust[i2] = 1

bartlett.test(d.new[,1:2], d.new$clust) # 0.5642
d.new$clust_fact <- as.factor(d.new$clust)

fit <- manova(as.matrix(d.new[,1:2]) ~ d.new$clust_fact) # 2.2e-16 ***
summary.manova(fit,test="Wilks")

fit$coefficients
#                          lon        lat
#(Intercept)       85.9741972 39.4606028
#d.new$clust_fact1  0.5135979  0.5166185

##### PUNTO C #####

#### CENTROIDS
###-------------------
centroids <- apply(d, dim(d)[2], function (x) tapply (x, cluster.ea, mean))
centroids

d1 <- d[i1,]
d2 <- d[i2,]

n1 <- dim(d1)[1]  # 36
n2 <- dim(d2)[1] # 61
p <- dim(d)[2]  #  2
d1.mean   <- sapply(d1,mean)
d1.cov    <- cov(d1)
d1.invcov <- solve(d1.cov)

d2.mean   <- sapply(d2,mean)
d2.cov    <- cov(d2)
d2.invcov <- solve(d2.cov)


# Significance level
alpha   <- .05

# Fisher quantile
cfr.fisher1 <- ((n1-1)*p/(n1-p))*qf(1-alpha,p,n1-p)
cfr.fisher2 <- ((n2-1)*p/(n2-p))*qf(1-alpha,p,n2-p)

#### CONFIDENCE/REJECTION REGION
# Ellipsoidal confidence region with confidence level (1-alpha)100%
plot(d, asp=1, pch=1, main='scatter plot of data',col=cluster.ea)

# Ellipse centered around our sample mean
ellipse(d1.mean, d1.cov/n1, sqrt(cfr.fisher1), col = 'black', lty = 2, lwd=2, center.cex=1)
ellipse(d2.mean, d2.cov/n2, sqrt(cfr.fisher2), col = 'red', lty = 2, lwd=2, center.cex=1)








