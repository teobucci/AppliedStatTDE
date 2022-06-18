# -----------
# EXERCISE 1
# -----------

rm(list=ls())
data <- read.table('nutrients.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point a

boxplot(data, las=2, col='gold')
# scaled because of different order of magnitude in the data
data.scaled <- scale(x=data)
boxplot(data.scaled, las=2, col='gold')


pc <- princomp(data.scaled, scores=T)
pc
summary(pc)


load.wea <- pc$loadings
load.wea[,1:3]





# ------------------------- point b

par(mfcol = c(3,1))
for(i in 1:3) barplot(load.wea[,i], ylim = c(-1, 1), main=paste("PC",i))

# ------------------------- point c

par(mfcol = c(1,1))
biplot(pc)
# top right: a lot of kcal and sugar

# ------------------------- point d

layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc, las=2, main='Principal components', ylim=c(0,4))
barplot(sapply(data,sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e2), ylab='Variances')
plot(cumsum(pc$sd^2)/sum(pc$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data),labels=1:ncol(data),las=2)

pc$sd^2/sum(pc$sd^2)
cumsum(pc$sd^2)[1:2]/sum(pc$sd^2)


# ------------------------- point e

x.mean <- colMeans(data)
x.var <- sapply(data, FUN = sd)

new <- c(
    Energy_kcal=400,
    Protein_g=9,
    Fat_g=5,
    Carb_g=100,
    Sugar_g=30,
    Fiber_g=4
)

new <- (new - x.mean) / x.var

pc.proj <- new %*% pc$loadings[,1:2]
pc.proj

par(mfrow=c(1,1))
plot(pc$scores)
points(pc.proj, col=2, pch=16, cex=2)




































































































































