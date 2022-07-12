rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("areaC.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
fm <- lm(Total ~ ., data = d)
summary(fm) 

coefficients(fm) 
sum(residuals(fm)^2)/fm$df # 2527736



##### PUNTO B #####
par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(fm)) #0.039
vif(fm)

# not very gaussian, not very homoschedastic 





##### PUNTO C #####
speed.pc <- princomp(d[,1:7], scores=TRUE)
summary(speed.pc)
speed.pc$load

# keep first 2 pcs
load <- speed.pc$loadings
a = 2 # number of principal components to be interpreted, change accordingly

x11()
par(mar = c(1,4,0,2), mfrow = c(a,1))
for(i in 1:a)
    barplot(load[,i], ylim = c(-1, 1))
# 1st: petrol, 2nd: diesel (most used cars)


# Now we estimate the model by inserting the PCs instead of the 
# original regressors 
# Model: y = b0 + b1*PC1+ b2*PC2 + eps, eps~N(0,sigma^2)
fm.pc <- lm(Total ~ speed.pc$scores[,1:2] + Weekend, data =d)
summary(fm.pc) 

par(mfrow=c(2,2))
plot(fm.pc) # a little better
shapiro.test(residuals(fm.pc)) # 0.015 a little worse

##### PUNTO D #####
# Weekend can go away
fm.pc2 <- lm(Total ~ speed.pc$scores[,1:2], data=d )
summary(fm.pc2)
# keep 2nd pc at level 90 %