rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d1 <- read.table("buoys.txt",header=T)
n <- nrow(d)

d <- d1[,1:2]

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

plot(d) # 3 clusters it seems


#### DENDROGRAMS
###-------------------

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
N <- sum(ng) #  245 331 176


centroids <- apply(d, dim(d)[2], function (x) tapply (x, cluster.ew, mean))
centroids

#     Long      Lat
#1 14.57181 42.44749
#2 14.68326 42.50844
#3 14.41417 42.41184

plot(d, col = cluster.ew+1)
legend("topright", legend=c(1,2,3), fill=c(1,2,3)+1)




##### PUNTO B #####
Ps <- c(shapiro.test(d1$DO[i1])$p,
        shapiro.test(d1$DO[i2])$p,
        shapiro.test(d1$DO[i3])$p)
Ps
# 0.6450336 0.3008030 0.7329140

bartlett.test(d1$DO, cluster.ew) # 0.4049
# assumptions verified

fit <- aov(DO ~ cluster.ew,data=d1)
summary(fit) # 0.087  Not a lot of difference tbh at level 5%

