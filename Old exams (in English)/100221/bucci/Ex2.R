data <- read.table('rice.txt', header=T)
n <- dim(data)[1]
p <- dim(data)[2]
head(data)

# ------------------------- point 1

dist.matrix <- dist(data, method='euclidean')
cluster.dl <- hclust(dist.matrix, method='complete')

plot(cluster.dl, main = "euclidean-complete", hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")
rect.hclust(cluster.dl, k = 3)

# cut the cluster
cluster.dl.cut <- cutree(cluster.dl, k = 3)

# how many
table(cluster.dl.cut)

i1 <- which(cluster.dl.cut == 1)
i2 <- which(cluster.dl.cut == 2)
i3 <- which(cluster.dl.cut == 3)

colMeans(data[i1,])
colMeans(data[i2,])
colMeans(data[i3,])

# plot distinction
plot(data, col = cluster.dl.cut+1, pch = 19)


# ------------------------- point 2

# cophenetic matrix
coph.matrix <- cophenetic(cluster.dl)
# cophenetic coefficient
coph.coeff <- cor(dist.matrix, coph.matrix)
coph.coeff

# is not very good so change complete to single

dist.matrix <- dist(data, method='euclidean')
cluster.dl <- hclust(dist.matrix, method='single')

plot(cluster.dl, main = "euclidean-single", hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")
rect.hclust(cluster.dl, k = 3)

# cut the cluster
cluster.dl.cut <- cutree(cluster.dl, k = 3)

# how many
table(cluster.dl.cut)

i1 <- which(cluster.dl.cut == 1)
i2 <- which(cluster.dl.cut == 2)
i3 <- which(cluster.dl.cut == 3)

colMeans(data[i1,])
colMeans(data[i2,])
colMeans(data[i3,])

# mean of major_axis
m1 <- mean(data[i1,1])
m2 <- mean(data[i2,1])
m3 <- mean(data[i3,1])

# var of major_axis
v1 <- var(data[i1,1])
v2 <- var(data[i2,1])
v3 <- var(data[i3,1])


# plot distinction
plot(data, col = cluster.dl.cut+1, pch = 19)

# ------------------------- point 3

# Bonferroni for mean and variance

shapiro.test(data[i1,1])$p.value
shapiro.test(data[i2,1])$p.value
shapiro.test(data[i3,1])$p.value
# Gaussianity is satisfied

alpha <- 0.05
k <- 6 # number of CI
g <- 3 # number of clusters

IC={}
Ps={}
for(i in 1:g){
    X <- data[cluster.dl.cut==i,1] # i need only the major axis
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

Ps
IC

# inf      center         sup
# ICmean 0.5780689138 0.620547619 0.663026324
# ICvar  0.0058723856 0.009860400 0.019196333
# ICmean 0.4083724058 0.431277778 0.454183150
# ICvar  0.0004965239 0.001060565 0.003261042
# ICmean 0.6898297338 0.711857143 0.733884552
# ICvar  0.0005855364 0.001189229 0.003286645








































