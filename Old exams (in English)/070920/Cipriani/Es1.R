rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("weather.txt",header=T)
n <- nrow(d)

dmeans <- sapply(d, mean)
dcovs <- sapply(d, sd)

##### PUNTO A #####
x11()
boxplot(d, las=1, col='red', main='Boxplot',grid=T)
d <- data.frame(scale(d))
# scaling is necessary
pca <- princomp(d, scores=T)
pca 
summary(pca)


# 3 loadings
load <- pca$loadings
a = 3 # number of principal components to be interpreted, change accordingly

x11()
par(mar = c(1,4,0,2), mfrow = c(a,1))
for(i in 1:a)
    barplot(load[,i], ylim = c(-1, 1))

##### PUNTO B #####

#### BIPLOT
# Biplot
x11()
biplot(pca, scale=0, cex=.7)





##### PUNTO C #####

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pca$sdev^2, las=2, main='Principal components', ylab='Variances')
barplot(sapply(d,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(pca$sdev^2)/sum(pca$sdev^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variability', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(d),labels=1:ncol(d),las=2)


# elbow at 3
cumsum(pca$sdev[1:3]^2)/sum(pca$sdev^2)



##### PUNTO D #####

#(d)
data.aug1 <- data.frame(MeanTemp = 30, MinTemp = 23, MaxTemp = 36, DewPoint = 22,
                        Humidity = 65, Visibility = 19, MeanWind = 5, MaxWind = 15)

data.aug1 <- (data.aug1 - dmeans)/dcovs

scores.aug1 <- t(pca$loadings)%*%t(as.matrix(data.aug1))
scores.aug1

x11()
plot(pca$scores[,1],pca$scores[,2],col='grey',pch=19,xlab='Comp.1',ylab='Comp.2')
points(scores.aug1[1],scores.aug1[2],col='black',pch=19) 



new_obs <- c(MeanTemp = 30, MinTemp = 23, MaxTemp = 36, DewPoint = 22,
                      Humidity = 65, Visibility = 19, MeanWind = 5, MaxWind = 15)

new_obs_scaled = new_obs - sapply(d, mean) #scala new_obs se si è stdizzato all'inizio il dataset!
data_sd = sapply(d,sd)
new_obs_scaled = new_obs_scaled/data_sd #per tutte le componenti di new_obs_scaled!

loads <- pca$loading
loads
new_scores = t(loads[,1:2])%*%new_obs_scaled
#new_scores = t(loads[,1:2])%*%new_obs

x11()
plot(scores[,1],scores[,2], main='Plot scores', pch=20)
points(new_scores[1], new_scores[2], cex = 2, pch = 20 , col = 'green')





