##################EX4
rm(list=ls())
listening <- read.table('listening.txt', header=TRUE)
head(listening)

library(fda)

x11()
matplot(t(listening), type = 'l')
# da questo grafico preliminare mi sembra di intravedere tra distinti andamenti tra le mie stazioni
data_W <- t(listening)
abscissa <- 1:365
m=3
nbasis <- 6:100
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
    basis <- create.bspline.basis(rangeval=c(0,365), nbasis[i],m)
    gcv[i] <- smooth.basis(abscissa, data_W, basis)$gcv
}
x11()
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbase=nbasis[which.min(gcv)]   #14
abline(v = nbasis[which.min(gcv)], col = 2)

basis <- create.bspline.basis(rangeval=c(0,365), nbase,m)

functionalParbis <- fdPar(fdobj=basis, lambda=1e2)

Xsster <- smooth.basis(abscissa, data_W, functionalParbis)
Xss0bis <- eval.fd(abscissa, Xsster$fd, Lfd=0)
x11()
plot(abscissa,data_W,xlab="t",ylab="observed data")
points(abscissa,Xss0ter ,type="l",col="red",lwd=1)
points(abscissa,Xss0bis ,type="l",col="green",lwd=1)
points(abscissa,Xss0best ,type="l",col="blue",lwd=2)

data_W.fd.1 <- Data2fd(y = data_W,argvals = abscissa,basisobj = basis)
x11()
plot.fd(data_W.fd.1)

data_W.fd.1$coefs[1:3,1]

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

pca_W.1$values[1:5]
# 1868.863549  279.044590   27.987432    3.556465    1.482923

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=131 are non-null
x11()
plot(pca_W.1$values[1:14],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:14]/sum(pca_W.1$values),xlab='j',ylab='CPV')

cumsum(pca_W.1$values)[1:5]/sum(pca_W.1$values)
# first three FPCs
x11()
par(mfrow = c(1,2))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')

# la prima componente principale ci mostra delle misurazioni ove la temperatura è bassa nei primi tre mesi
# dell'anno, e che la vede risalire fino ad un massimo attorno ad ottobre
# la seconda, al contrario, ci mostra un andamento opposto con un minimo di temperature raggiunte attorno a 
# luglio ed un massimo tra febbraio e marzo
x11()
par(mfrow=c(1,2))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)

x11()
plot(pca_W.1$scores[,1],pca_W.1$scores[,2], pch = 16, xlab = 'PC1', ylab = 'PC2')


