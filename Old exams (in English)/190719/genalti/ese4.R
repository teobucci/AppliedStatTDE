rm(list = ls())
df <- read.table('watertemp.txt')
head(df)
dim(df)

matplot(t(df[,-366]), type = 'l')
# da questo grafico preliminare mi sembra di intravedere tra distinti andamenti tra le mie stazioni

data_W <- t(df[,-366])
time <- 1:365

basis.1 <- create.fourier.basis(rangeval=c(0,365),nbasis=45)
data_W.fd.1 <- Data2fd(y = data_W,argvals = time,basisobj = basis.1)
plot.fd(data_W.fd.1)

data_W.fd.1$coefs[1:3,1:2]
#        Station1  Station2
# const 295.32834 272.04330
# sin1   33.16819  27.71720
# cos1  -35.75822 -33.38612

pca_W.1 <- pca.fd(data_W.fd.1,nharm=5,centerfns=TRUE)

pca_W.1$values[1:5]
# 1868.863549  279.044590   27.987432    3.556465    1.482923

# scree plot
# pca.fd computes all the 365 eigenvalues, but only the first 
# N-1=131 are non-null
plot(pca_W.1$values[1:45],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:45]/sum(pca_W.1$values),xlab='j',ylab='CPV')

# first three FPCs
par(mfrow = c(1,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
abline(h=0,lty=2)
plot(pca_W.1$harmonics[3,],col=2,ylab='FPC3')
dev.off()

# la prima componente principale ci mostra delle misurazioni ove la temperatura è bassa nei primi tre mesi
# dell'anno, e che la vede risalire fino ad un massimo attorno ad ottobre
# la seconda, al contrario, ci mostra un andamento opposto con un minimo di temperature raggiunte attorno a 
# luglio ed un massimo tra febbraio e marzo

par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2,3), expand=0, cycle=FALSE)
dev.off()

par(mfrow=c(1,1))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2], col = factor(df[,366]), pch = 16, xlab = 'PC1', ylab = 'PC2')
legend('topleft',c('Deep', 'Medium', 'Surface'), col = c('black','red','green'), pch = 16)
dev.off()

# ho conferma che esistono tre andamenti ben distinti tra le temperature nelle mie stazioni
# vediamo come le prime due componenti principali bene separano le tre profondit? diverse a cui vengono misurate
# le profondit?, in particolare vediamo come in profondit? (deep, in nero) la prima componente principale assuma
# bassi valori, in media profondit? valori pi? alti e poi ancora in superficie.
# questo mi suggerisce che i tre andamenti che vedevamo nelle misurazioni sono dovuti alle differenti profondit?
# a cui le stazioni misurano, e che la prima componente principale ci spiega questo fenomeno diminuendo con 
# l'aumentare la profondit?







