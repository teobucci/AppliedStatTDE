rm(list = ls()) 
load('mcshapiro.test.RData')
df = read.table("listening.txt", header = TRUE)

##### Problem 4 #####
library(fda)
library(fields)

dim(data)
# 129 365

data = as.data.frame(t(df)) # MATRIX time series per colonne 
par(mfrow)
matplot(data,type='l',main='Number',ylab='listening',xlab='day')

time = 1:365

# Better to use a b-spline basis
n_bas = 100
degree = 3
order = degree + 1
x_max = dim(data)[1]
time = 1:365
basis = create.bspline.basis(rangeval=c(0,x_max),nbasis=n_bas, norder=order)
data_fd = Data2fd(y=as.matrix(data),argvals = time, basisobj = basis)
plot.fd(data_fd, main="B-splines")

plot(basis)


data_fd$coefs[1:3,1]    
# bspl4.1  bspl4.2  bspl4.3
# 24.76659 23.66195 21.46086 



## PCA for Functionals 
pca= pca.fd(data_fd, nharm=5, centerfns=TRUE)

pca$varprop[1:5]
# 0.68147817 0.12866794 0.10885970 0.02665162 0.01977973
sum(pca$varprop[1:5])
# 0.9654372

# scree plot
layout(cbind(1,2))
plot(pca$values,xlab='j',ylab='Eigenvalues')
plot(cumsum(pca$values)/sum(pca$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

# we can 


# first two FPCs
x11()
layout(cbind(1,2))
plot(pca$harmonics[1,],col=1,ylab='FPC1',ylim=c(-0.1,0.08))
abline(h=0,lty=2)
plot(pca$harmonics[2,],col=2,ylab='FPC2',ylim=c(-0.1,0.08))


# plot of the FPCs as perturbation of the mean
layout(cbind(1,2))
plot(pca, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)



# scatter plot of the scores and outlier
layout(cbind(1,1))
plot(pca$scores[,1],pca$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
plot(pca$scores[,1],pca$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2",xlim=c(-400,250))
text(pca$scores[,1],pca$scores[,2],dimnames(data_W)[[2]], cex=1)
# Plot of lines
layout(1)
matplot(eval.1,type='l')
lines(eval.1[,35],lwd=4, col=2) #temperature profile for Resolute
