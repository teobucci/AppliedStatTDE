rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("sequoia.txt",header=T)
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



# Dendrograms 2 clusters etc
x11()
par(mfrow=c(1,3))
plot(d.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(d.es, k=2)
plot(d.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(d.ec, k=2)
plot(d.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(d.ea, k=2)
#### Look also at the scatter plot before deciding where to cut


plot(clustw, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clustw, k=5)

#### CUTTING
###-------------------
# Fix k=3 clusters:
cluster.ew <- cutree(clustw, k=5) # euclidean-ward


#### NUMEROSITY OF CLUSTERS
###-------------------
g <- 5
i1 <- which(cluster.ew==1)
i2 <- which(cluster.ew==2)
i3 <- which(cluster.ew==3)
i4 <- which(cluster.ew==4)
i5 <- which(cluster.ew==5)
ng <- c(length(i1),length(i2), length(i3), length(i4), length(i5)) 
ng 
N <- sum(ng) #  77 25 82 98 12

# oppure
table(cluster.ew)





#### CENTROIDS
###-------------------
centroids <- apply(d, dim(d)[2], function (x) tapply (x, cluster.ew, mean))
centroids


plot(d, col = cluster.ew+1)

# Older trees (> height > diam) are fewer because of fires

##### PUNTO B #####
alpha <- 0.1
k <- 10 # number of CI
g <- 5 # number of clusters
IC={}
Ps={}
for(i in 1:g){
    X <- d[cluster.ew==i,2] # i need only the major axis
    n <- length(X)
    Ps <- c(Ps,shapiro.test(X)$p)
    x.mean   <- mean(X)
    x.cov    <- var(X)
    
    ICmean <- c(inf    = x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
                center = x.mean,
                sup    = x.mean + sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1))
    
    ICvar <- c(inf     = x.cov*(n-1) / qchisq(1 - alpha/(2*k), n-1),
               center  = x.cov,
               sup     = x.cov*(n-1) / qchisq(alpha/(2*k), n-1))
    
    IC <- rbind(IC,
                ICmean,
                ICvar)
}
Ps #0.4091165 0.9899046 0.8147612 0.1966434 0.7798557
IC

#inf     center        sup
#ICmean  6.73439942  6.9515584  7.1687175
#ICvar   0.35457803  0.5201817  0.8236806
#ICmean  7.71385634  8.0752000  8.4365437
#ICvar   0.21981455  0.4172677  1.0129666
#ICmean  7.17075311  7.3829268  7.5951005
#ICvar   0.36562759  0.5304950  0.8268407
#ICmean  5.90812307  6.1117347  6.3153463
#ICvar   0.41784804  0.5885135  0.8798950
#ICmean 11.60553121 11.8991667 12.1928021
#ICvar   0.04409681  0.1072629  0.4532428


# Che ci sarebbe da commentare? ci sta con quello che si vede nel plot
