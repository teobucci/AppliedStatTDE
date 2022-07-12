rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("geisha.txt",header=T)
n <- nrow(d)



##### PUNTO A #####

#### COMPUTING DISTANCE
###-------------------
d.e <- dist(d, method='euclidean')

# Unorder data
misc <- sample(160)
d <- d[misc,]
d.e <- dist(d, method='euclidean')

#### CLUSTERING
###-------------------
d.es <- hclust(d.e, method='single')

#### DENDROGRAMS
###-------------------
x11()
plot(d.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

# Dendrograms 2 clusters etc
x11()
plot(d.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(d.es, k=2)
#### Look also at the scatter plot before deciding where to cut

#### CUTTING
###-------------------
cluster.es <- cutree(d.es, k=2) # euclidean-single

#### COPH COEFFICIENTS
###-------------------
coph.es <- cophenetic(d.es)
# compute cophenetic coefficients
es <- cor(d.e, coph.es)
es 
# 0.8773591

# numerosity
table(cluster.es)
#   1   2 
# 159   1 

#### CENTROIDS
centroids <- apply(d, dim(d)[2], function (x) tapply (x, cluster.es, mean))
centroids

#  duration     time
# 1 66.01946 54.31678
# 2 89.00000 12.90000

plot(d, col=cluster.es)
# very unsatisfying










##### PUNTO B #####
d.e <- dist(d, method='euclidean')

# Unorder data
misc <- sample(150)
d <- d[misc,]
d.e <- dist(d, method='euclidean')

#### CLUSTERING
###-------------------
d.ea <- hclust(d.e, method='average')

#### DENDROGRAMS
###-------------------
x11()
plot(d.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

# Dendrograms 2 clusters etc
x11()
plot(d.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(d.ea, k=2)
#### Look also at the scatter plot before deciding where to cut

#### CUTTING
###-------------------
cluster.ea <- cutree(d.ea, k=2) # euclidean-average

#### COPH COEFFICIENTS
###-------------------
coph.ea <- cophenetic(d.ea)
# compute cophenetic coefficients
ea <- cor(d.e, coph.ea)
ea 
# 0.88

# numerosity
table(cluster.ea)
#   1   2 
# 70   80

#### CENTROIDS
centroids <- apply(d, dim(d)[2], function (x) tapply (x, cluster.ea, mean))
centroids

#duration     time
#1 90.03857 45.16143
#2 45.29000 61.81000

plot(d, col=cluster.ea)





##### PUNTO C #####
# Succesful group = group 1  (70)


#### BONFERRONI CONFIDENCE FOR CLUSTERING (+ gaussianity)
###-------------------

# 1) univariate, just the mean/variable of one variable in different clusters
# MEDIA + VARIANZA
alpha <- 0.1
k <- 4# number of CI
g <- 2 # number of clusters
IC={}
Ps={}
for(i in 1:2){
    X <- d[cluster.ea==1,i] # 1 group, cycle for each feature
    n <- length(X)
    Ps <- c(Ps,shapiro.test(X)$p)
    x.mean   <- mean(X)
    x.cov    <- var(X)
    
    ICmean <- c(inf    = x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
                center = x.mean,
                sup    = x.mean + sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1))
  
    IC <- rbind(IC,
                ICmean)
}
Ps
IC

t1 <- d[cluster.ea==1,]
t2 <- d[cluster.ea==2,]
n1 <- dim(t1)[1] 
n2 <- dim(t2)[1] 
p  <- dim(t1)[2] 

# we compute the sample mean, covariance matrices and the matrix Spooled
t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)

alpha <- 0.1
IC_diff_means <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(k*2), n1+n2-2))
IC <- rbind(IC,
              IC_diff_means)

IC

#           inf    center       sup
# ICmean    87.91978  90.39420  92.86862
# ICmean    42.42696  45.50290  48.57883
# duration -48.48860 -45.35223 -42.21586
# time      12.46414  16.09340  19.72265




