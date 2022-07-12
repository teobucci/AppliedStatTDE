rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("diamonds.txt",header=T)
n <- nrow(d)
d <- data.frame(scale(d))

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

plot(d) # 2 clusters it seems


#### DENDROGRAMS
###-------------------

plot(clustw, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')



# Dendrograms 2
plot(clustw, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(clustw, k=2)

#### CUTTING
###-------------------
# Fix k=2 clusters:
cluster.ew <- cutree(clustw, k=2) # euclidean-ward


#### NUMEROSITY OF CLUSTERS
###-------------------
g <- 2
i1 <- which(cluster.ew==1)
i2 <- which(cluster.ew==2)
ng <- c(length(i1),length(i2)) 
ng 
N <- sum(ng) #   297 38


plot(d, col = cluster.ew+1)
legend("topright", legend=c(1,2), fill=c(1,2)+1)

perc1 <- 297/(297+38) #0.88
perc2 <- 38/(297+38) #0.11



##### PUNTO B #####
t1 <- d[i1,]
t2 <- d[i2,]
n1 <- dim(t1)[1] # n1=3
n2 <- dim(t2)[1] # n2=4
p  <- dim(t1)[2] # p=2

# we compute the sample mean, covariance matrices and the matrix Spooled
t1.mean <- sapply(t1,mean)
t2.mean <- sapply(t2,mean)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
# we compare the matrices
list(S1=t1.cov, S2=t2.cov, Spooled=Sp)



alpha   <- .05
Spinv   <- solve(Sp)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)

IC <- cbind(t2.mean-t1.mean - sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2),
            t2.mean-t1.mean,
            t2.mean-t1.mean + sqrt(diag(Sp)*(1/n1+1/n2)) * qt(1 - alpha/(p*2), n1+n2-2))
IC


#Diameter 1.554819 1.867600 2.180381
#Carats   2.612517 2.792096 2.971674


