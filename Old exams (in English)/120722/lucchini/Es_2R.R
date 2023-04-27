##### Problem 2 ####

remove(list=ls())
graphics.off()
load('mcshapiro.test.RData')
df = read.table("demogorgons.txt", header = TRUE)

###### Point a ####
n = dim(df)[1]
misc = sample(n)
df = df[misc,]


choosen_metric = "euclidean"
choosen_linkage = "average"

## if 2D scatterplot
plot(df, pch = 16)

df_e= dist(df, method=choosen_metric) # see help dist for methods
df_es = hclust(df_e, method=choosen_linkage) # see help hclust for methods

# cophenetic Matrix and coefficient
coph_es = cophenetic(df_es)
es = cor(df_e, coph_es)
es #  0.8930955
#coeff vicino ad 1 è buono, altrimenti hai rumore nel dendogram

par(mfrow= c(1,2))
image(as.matrix(df_e), main=choosen_metric, asp=1 )
image(as.matrix(coph_es), main=choosen_linkage, asp=1 )

# dendrogram
par(mfrow= c(1,1))
plot(df_es, main=paste(choosen_metric, choosen_linkage), hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(df_es, k=2) # ok 
rect.hclust(df_es, k=3)

# cutting tree
n_cluster = 2
cluster_es = cutree(df_es, k=n_cluster) 
dim_cluster = NULL
for (i in 1:n_cluster){
  dim_cluster = c(dim_cluster, length(cluster_es[which(cluster_es == i)]))
}
dim_cluster   # 61 36

# centroids
centroids = apply (df, dim(df)[2], function (x) tapply (x, cluster_es, mean))
centroids

#     lon      lat
# 1 86.4878 39.97722
# 2 85.9742 39.46060


# Colored Scatterplot
# x11()
plot(df, pch = 16, col = cluster_es+1)
points(centroids, pch = 4, cex = 2, lwd = 2) # col = c(1:n_cluster+1)


###### Point b ####
df$pos = as.factor(cluster_es)

id_factor = c(3)
id_categories = c(1,2)

value = df[, id_categories]
groups = df[, id_factor]

p_cat = length(id_categories)   # number of categories / columns
g = length(levels(groups))
treat   = levels(groups)
n = length(value[,1])

# May add boxplots by groups or category

### Verify the assumptions:
# 1)  normality (multivariate) in each group (3 tests)
Ps = NULL
for(i in 1:g)
  Ps = c(Ps, mcshapiro.test(df[which(groups == treat[i]),id_categories])$p) # look for big p-value 
Ps

# 0.8388 0.6132


# 2) same covariance structure (= same covariance matrix Sigma)
Smatrixes = list()
for (level in levels(groups)){
  Smatrixes[[length(Smatrixes)+1]] = cov(value[groups == level,])
}

color_mat = matrix(unlist(Smatrixes), ncol = p_cat, byrow = T)
par(mfrow=c(1,length(Smatrixes)))
for (i in 1:length(Smatrixes)){
  image(Smatrixes[[i]], col=heat.colors(100),main=paste('Cov S',i, sep=''), asp=1, axes = FALSE, breaks = quantile(color_mat, (0:100)/100, na.rm=TRUE))
}
# Note: We can verify the assumptions a posteriori on the residuals of 
#       the estimated model 

bartlett.test(value, groups) # 0.5642 OK var


# One-way MANOVA 
fit = manova(as.matrix(value) ~ groups)
summary.manova(fit,test="Wilks")
# 2.2e-16 *** DIFFERENCE

# Exact tests for p<=2 or g<=3 already implemented in R
# Reject H0  (equal means) if small p-value (OK)

fit$coefficients
sd(fit$residuals) # 0.1217148


summary.aov(fit)

# BOTH ARE DIFFERENT

###### Point c ####
df_1 = df[which(df$pos==1),1:2]
df_2 = df[which(df$pos==2),1:2]

df_mean_1 = sapply(df_1, mean)
df_cov_1 = cov(df_1)
df_invcov = solve(df_cov_1)

alpha = 0.05
n1 = dim(df_1)[1]
p = dim(df_1)[2]
cfr.fisher_1 = ((n1-1)*p/(n1-p))*qf(1-alpha,p,n1-p)

delta.0 = rep(0.0, p)
# Ellipsoidal confidence region with confidence level 99%

df_mean = sapply(df_2, mean)
df_cov = cov(df_2)
df_invcov = solve(df_cov)

n = dim(df)[1]
p = dim(df)[2]
cfr.fisher = ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)



library(car)

# Plot ellipse
x11()
plot(df[,1:2], pch = 16, col = cluster_es+1)
points(centroids, pch = 4, cex = 2, lwd = 2) # col = c(1:n_cluster+1)
ellipse(df_mean_1, df_cov_1/n1, radius = sqrt(cfr.fisher_1), add=T,lwd=3, col='blue') 
ellipse(df_mean, df_cov/n, radius = sqrt(cfr.fisher), add=T,lwd=3, col='blue') 










