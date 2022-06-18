# -----------
# EXERCISE 2
# -----------

rm(list=ls())
data <- read.table('streaming.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point a

# d = distance
# l = linkage

# distance matrix
dist.matrix <- dist(data, method='euclidean')

# plot dist matrix
image(1:n, 1:n, as.matrix(dist.matrix), main = "metrics: NAME OF METRIC", asp = 1, xlab = "i", ylab = "j")

# clustering
cluster.dl <- hclust(dist.matrix, method = "single")

# plot dendrogram
plot(cluster.dl, main = "distance-linkage", hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")
# with k cluster
rect.hclust(cluster.dl, k = 3)

# cut the cluster
cluster.dl.cut <- cutree(cluster.dl, k = 3)


# ------------------------- point b

# how many
table(cluster.dl.cut)

# plot distinction
plot(data, col = cluster.dl.cut, pch = 19) # or col = cluster.dl (+1 for changing colors)

# cophenetic matrix
coph.matrix <- cophenetic(cluster.dl)
# cophenetic coefficient
coph.coeff <- cor(dist.matrix, coph.matrix)

# ------------------------- point c

alpha <- 0.05
k <- 6 # number of CI
g <- 3 # number of clusters

IC={}
Ps={}
for(i in 1:g){
    X <- data[cluster.dl.cut==i,1] # min
    Y <- data[cluster.dl.cut==i,2] # artists
    n <- length(X)
    Ps <- c(Ps,shapiro.test(X)$p)
    Ps <- c(Ps,shapiro.test(Y)$p)
    x.mean <- mean(X)
    y.mean <- mean(Y)
    x.cov <- var(X)
    y.cov <- var(Y)
    
    ICmeanMin <- c(inf    = x.mean - sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1),
                center = x.mean,
                sup    = x.mean + sqrt(x.cov/n) * qt(1 - alpha/(2*k), n-1))
    
    ICmeanArtist <- c(inf    = y.mean - sqrt(y.cov/n) * qt(1 - alpha/(2*k), n-1),
                center = y.mean,
                sup    = y.mean + sqrt(y.cov/n) * qt(1 - alpha/(2*k), n-1))
    
    IC <- rbind(IC,
                ICmeanMin,
                ICmeanArtist)
}
Ps
IC











































































































































