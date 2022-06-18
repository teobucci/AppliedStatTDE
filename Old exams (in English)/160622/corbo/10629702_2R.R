##################EX2
rm(list=ls())
musi <- read.table('musicCountry.txt', header=TRUE)
head(musi)
n <- dim(musi)[1]
library(MASS)
priorp=c(0.1,0.9)
qda.ort <- qda(musi[,1:2], musi[,3], prior = priorp)
qda.ort

mcshapiro.test(musi[which(musi[,3]=='US'),1:2])
mcshapiro.test(musi[which(musi[,3]=='Germany'),1:2])
pred <- predict(qda.ort, musi[,1:2])
table(pred$class, musi[,3])

varu=cov(musi[which(pred$class=='US'),1:2])
varu
varg=cov(musi[which(pred$class=='Germany'),1:2])
varg
x11()
plot(musi[,1], musi[,2], col = (factor(musi$release.country)))
points(qda.ort$means, pch=4,col=c('black','red') , lwd=2, cex=1.5)

x  <- seq(min(musi[,1]), max(musi[,1]), length=200)
y  <- seq(min(musi[,2]), max(musi[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(qda.ort, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

classCV <- qda(musi[,1:2], musi[,3], CV = TRUE, prior=priorp)


table(class.true=musi[,3], class.assignedCV=classCV$class)

errors <- (classCV$class != musi[,3])
APER.qdaCV <- sum(errors)/n

APER=9*0.1/36+4*0.9/152
APER     #0.04868421

x <- data.frame(seq(min(musi[,1]), max(musi[,1]),100),seq(min(musi[,2]), max(musi[,2])),100)
predict(qda.ort, x)$posterior


newdatum <- data.frame(price  = 50, average.length = 3.5)
np=predict(qda.ort, newdatum)
np  #US

#(d)
library(e1071)
y <- rep(0,n)
y[which(musi$release.country=='US')] <- 1
x11()
plot(musi[,1], musi[,2],pch=20, col=as.character(y+1))

set.seed (1)
tune.out <- tune(svm,y~.,data=dat ,kernel = 'linear',
                 ranges =list(cost=c(0.001 , 0.01, 0.1, 1,10,100) ))
summary(tune.out)

# Extract the best model from the result of tune
bestmod <- tune.out$best.model
summary(bestmod)
x11()
plot(bestmod , dat, col =c('salmon', 'light blue'), pch=19)

newdatum <- data.frame(x.price  = 50, x.average.length = 3.5)
ypred = predict(bestmod,newdatum)

table(true.label=dat$y, assigned.label =ypred )
APER.svm<- (27+17)/n  #0.2933333
APER.svm