rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("running.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
plot(d)
d.e <- dist(d, method='euclidean')

misc <- sample(n)
d <- d[misc,]
d.e <- dist(d, method='euclidean')



#### CLUSTERING
###-------------------
d.es <- hclust(d.e, method='single')
d.ea <- hclust(d.e, method='average')
d.ec <- hclust(d.e, method='complete')


#### DENDROGRAMS
###-------------------
x11()
par(mfrow=c(1,3))
plot(d.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(d.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(d.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

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


#### CUTTING
###-------------------
# Fix k=2 clusters:
cluster.ec <- cutree(d.ec, k=2) # euclidean-complete:
cluster.es <- cutree(d.es, k=2) # euclidean-single
cluster.ea <- cutree(d.ea, k=2) # euclidean-average


#### COPH COEFFICIENTS
###-------------------
coph.es <- cophenetic(d.es)
coph.ec <- cophenetic(d.ec)
coph.ea <- cophenetic(d.ea)

# compute cophenetic coefficients
es <- cor(d.e, coph.es)
ec <- cor(d.e, coph.ec)
ea <- cor(d.e, coph.ea)

c("Eucl-Single"=es,"Eucl-Compl."=ec,"Eucl-Ave."=ea)

# Eucl-Single Eucl-Compl.   Eucl-Ave. 
# 0.8960131   0.8883862   0.9063052


#### NUMEROSITY OF CLUSTERS
###-------------------
g <- 2
i1 <- which(cluster.es==1)
i2 <- which(cluster.es==2)
ng <- c(length(i1),length(i2)) 
ng 
N <- sum(ng)
# 79 1 

# oppure
table(cluster.es)



#### CENTROIDS
###-------------------
centroids <- apply(d, dim(d)[2], function (x) tapply (x, cluster.es, mean))
centroids
#   S.Mar.Paraggi Paraggi.Portofino
#1      35.36899          20.70038
#2      21.21000           9.73000


plot(d, col=cluster.es)




##### PUNTO B #####


plot(d, col=cluster.ea)
# Let's use average instead

g <- 2
i1 <- which(cluster.ea==1)
i2 <- which(cluster.ea==2)
ng <- c(length(i1),length(i2)) 
ng 
N <- sum(ng)
# 31 49

# coph 0.9063052


centroids <- apply(d, dim(d)[2], function (x) tapply (x, cluster.ea, mean))
centroids

# v S.Mar.Paraggi Paraggi.Portofino
#1      27.03742          15.18903
#2      40.35102          23.96327


##### PUNTO C #####

alpha <- 0.05
k <- 4 # number of CI
g <- 2 # number of clusters
IC={}
Ps={}
for(i in 1:g){
    X <- d[cluster.ea==i,1] # i need only the major axis
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
Ps # 0.06681324 0.78151890
IC

#             inf    center       sup
#ICmean 26.150178 27.037419 27.924661
#ICvar   1.963832  3.455780  7.329399
#ICmean 39.306307 40.351020 41.395734
#ICvar   5.019222  7.939764 14.102182




