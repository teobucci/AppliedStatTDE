rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d_all <- read.table("dnd_monsters.txt",header=T)
n <- nrow(d_all)

d <- data.frame(d_all[,1:8])


##### PUNTO A #####
boxplot(d, las=1, col='red', main='Boxplot',grid=T)
d <- scale(d)


pca <- princomp(d, scores=T)
pca 
summary(pca)

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pca$sdev^2, las=2, main='Principal components', ylim=c(0,10), ylab='Variances')
barplot(sapply(d,sd)^2, las=2, main='Original variables', ylim=c(0,5), ylab='Variances')
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variability', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(d),labels=1:ncol(d),las=2)



##### PUNTO B #####


# Loadings
load <- pca$loadings
a = 2 # number of principal components to be interpreted, change accordingly

par(mar = c(1,4,0,2), mfrow = c(a,1))
for(i in 1:a)
    barplot(load[,i], ylim = c(-1, 1))





##### PUNTO C #####

plot(pca$scores[,1], pca$scores[,2], col=as.factor(d_all$size))
legend("topright", legend=levels(as.factor(d_all$size)), fill=c(1,2,3,4,5,6))





##### PUNTO D #####
library(e1071)
d_tiny <- data.frame(pca$scores[d_all$size=="Tiny",1:2])
d_huge <- data.frame(pca$scores[d_all$size=="Huge",1:2])
d <- rbind(d_tiny, d_huge)
d$size <- rep(0,26+40) # creating a cat. variable for size (tiny=0)
d$size[27:66] = 1 # huge=1

# Fit the Support Vector Classifier (kernel = "linear")
# given a cost C
svmfit <- svm(as.factor(d$size)~. , data=d , kernel ='linear', cost =1, scale =FALSE )
summary(svmfit)

x11()
par(mfrow=c(1,2))
plot(svmfit, d, col =c('salmon', 'light blue'), pch=19, asp=1)


new <- data.frame(armor.class=14,hit.points=50,strength=19,dexterity=10,constitution=16,intelligence=8,wisdom=12,charisma=13)
# Projecting on the space of first 2 PCs
x.mean <- colMeans(d)
x.var <- sapply(d, FUN = sd)

new <- (new - x.mean) / x.var
pc.proj <- as.matrix(new) %*% pca$loadings[,1:2]
pc.proj

ypred <- predict(svmfit,pc.proj)
ypred #1 huge

