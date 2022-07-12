rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table(".txt",header=T)
n <- nrow(d)



##### PUNTO A #####
d.e <- dist(d, method='euclidean')
# Unorder data
misc <- sample(n)
d <- d[misc,]
d.e <- dist(d, method='euclidean')

#### CLUSTERING
###-------------------
d.es <- hclust(d.e, method='single')
d.ea <- hclust(d.e, method='average')
d.ec <- hclust(d.e, method='complete')
clustw <- hclust(d.e, method='ward.D2')
plot(clustw, hang=-0.1, labels=FALSE, main='ward', xlab='', sub='')




#### DENDROGRAMS
###-------------------
x11()
par(mfrow=c(1,4))
plot(d.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(d.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(d.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(clustw, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')



# Dendrograms 3
plot(clustw, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clustw, k=3)

#### CUTTING
###-------------------
# Fix k=3 clusters:
cluster.ew <- cutree(clustw, k=3) # euclidean-ward


#### NUMEROSITY OF CLUSTERS
###-------------------
g <- 3
i1 <- which(cluster.ew==1)
i2 <- which(cluster.ew==2)
i3 <- which(cluster.ew==3)
ng <- c(length(i1),length(i2), length(i3)) 
ng 
N <- sum(ng) #   25 18 32


plot(d, col = cluster.ew+1)
legend("topright", legend=c(1,2,3), fill=c(1,2,3)+1)




##### PUNTO B #####
# Assuming same cov. structure
# Checking multivariate gaussianity within groups
mcshapiro.test(d[i1,]) # 0.3928
mcshapiro.test(d[i2,]) # 0.8392
mcshapiro.test(d[i3,]) # 0.9308


fit <- manova(as.matrix(d) ~ cluster.ew)
summary.manova(fit, test="Wilks") # .2e-16 *** oh yes


##### PUNTO C #####
g <- 3
p <- 2
alpha <- 0.1
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

n1 <-  length(i1)
n2 <- length(i2)
n3 <- length(i3)
W <- summary.manova(fit)$SS$Residuals
m  <- sapply(d,mean)         # estimates mu
m1 <- sapply(d[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(d[i2,],mean)    # estimates mu.2=mu+tau.2
m3 <- sapply(d[i3,],mean)    # estimates mu.3=mu+tau.3

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
inf13 <- m1-m3 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
sup13 <- m1-m3 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n3) )
inf23 <- m2-m3 - qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )
sup23 <- m2-m3 + qT * sqrt( diag(W)/(n-g) * (1/n2+1/n3) )

CI <- list(group12=cbind(inf12, sup12), group13=cbind(inf13, sup13), group23=cbind(inf23, sup23))
CI


#$group12
#inf12     sup12
#Length -2.705585 -1.849815
#Width  -2.827845 -1.084091

#$group13
#inf13     sup13
#Length -2.6101120 -1.871151
#Width   0.1190064  1.624745

# group 1 significantly different from 2 and 3 in all variables
# group 2 and 3 ar different in width but not in length (as shown by plot)

#$group23
#inf23     sup23
#Length -0.370756 0.4448921
#Width   1.996844 3.6588441


