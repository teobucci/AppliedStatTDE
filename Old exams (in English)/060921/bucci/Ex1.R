rm(list=ls())
data <- read.table('pinnanobilis.txt', header=TRUE)
head(data)

n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point 1

# distance matrix
dist.matrix <- dist(data, method='euclidean')

# clustering
cluster.dl <- hclust(dist.matrix, method = "complete")
names(cluster.dl) # $order

# plot dendrogram
plot(cluster.dl, main = "euclidean-complete", hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")
# with k cluster
rect.hclust(cluster.dl, k = 2)

# cut the cluster
cluster.dl.cut <- cutree(cluster.dl, k = 2)

# how many
table(cluster.dl.cut)

# plot distinction
plot(data, col = cluster.dl.cut+21, pch = 19)

# cophenetic matrix
coph.matrix <- cophenetic(cluster.dl)
# cophenetic coefficient
coph.coeff <- cor(dist.matrix, coph.matrix)
coph.coeff
# 0.8956969

# ------------------------- point 2

# 1)  normality (multivariate) in each group (3 tests)
Ps <- NULL
g <- 2
for(i in 1:g)
    Ps <- c(Ps, mcshapiro.test(data[which(cluster.dl.cut == i),1:2])$p) 
Ps
# 0.5332 0.2320 ok

# covariance ok by text

fit <- manova(as.matrix(data) ~ cluster.dl.cut)
summary.manova(fit,test="Wilks")
# 2.2e-16 they have an effect on the features

fit$coefficients
#                   height     width
# (Intercept)     52.57204 18.142163
# cluster.dl.cut -18.00458 -5.481244

# Model: X.ij = mu + tau.i + eps.ij; eps.ij~N_p(0,Sigma), X.ij, mu, tau.i in R^2

# ------------------------- point 3

g <- 2
i1 <- which(cluster.dl.cut==1)
i2 <- which(cluster.dl.cut==2)
ng <- c(length(i1),length(i2)) 
ng
N <- sum(ng)

alpha <- 0.1
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)
W <- summary.manova(fit)$SS$Residuals
m  <- sapply(data,mean)         # estimates mu
m1 <- sapply(data[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(data[i2,],mean)    # estimates mu.2=mu+tau.2
inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/ng[1]+1/ng[2]) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/ng[1]+1/ng[2]) )
CI <- list(cbind(inf12, sup12))
CI

# purple are both larger and higher, confirmed by the CI



























