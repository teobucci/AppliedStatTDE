#PB 1)

#a) 
plot(pinnanobilis)

HC <- hclust(dist(pinnanobilis, method='euclidean'), method = 'complete')
x11()
plot(HC, hang=-0.1, sub='', labels=F, xlab='') #evident from the plot of the dendogram that we should cluster 
                                              #our dataset in two groups 

rect.hclust(HC, k=2)


#b)
pag <- cutree(HC, k=2) #divisione in due gruppi 
table(pag)
which(pag==2)

x11()
plot(pinnanobilis , col=pag+1, asp=1, pch=16, lwd=2)

i1 <- which(pag=='1')
i2 <- which(pag=='2')

p  <- 2
g <- 2
n1 <- table(pag)[1]
n2 <- table(pag)[2]
n <- n1+n2

load("D:/ingmat/applied statistic/esami/mcshapiro.test.RData")
library(car)

mcshapiro.test(pinnanobilis[pag=='1',])
mcshapiro.test(pinnanobilis[pag=='2',]) #normality assumptions verified in both groups

fit <- manova(as.matrix(pinnanobilis) ~ as.factor(pag))
summary.manova(fit,test="Wilks") #high evidence to say memebership to a group influences the mean 
summary.aov(fit) #both widthand height are responsible 

# Estimate variances
W <- summary.manova(fit)$SS$Residuals/80
W2 <- diag(t(fit$res) %*% fit$res)/(n-g) 

m  <- sapply(pinnanobilis,mean)         # estimates mu
m1 <- sapply(pinnanobilis[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(pinnanobilis[i2,],mean)    # estimates mu.2=mu+tau.2

# Estimate tau.i, beta.j:
tau_h  <- mean(pinnanobilis[,1][pag==1]) - m_height # tau.1.1
tauC_w  <- mean(pinnanobilis[,2][pag==1]) - m_width # tau.1.2

tau_2_h <- mean(pinnanobilis[,1][pag==2]) - m_height
tau_2_w <- mean(pinnanobilis[,2][pag==2]) - m_width


#point_c)
### Via Bonferroni
alpha <- 0.1
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
inf12
sup12
#the two intervals do not contain the 0, and there's a higher difference in height between the two groups rather
#than in width, but both are significant (as we can see our clustering in fact separates the variables based on the height - horizontal axis)

#PB 3)

#a) 
attach(boats)
material <- as.factor(boats$material)

# price = b.0g + b1*wood + b1*length + b2*power + b3*draught + b4*crew + b5*year + eps 
# bo.fiber = b0 
#bo.wood = b0 + b1 

# E[eps]=0, Var(eps)=sigma^2

fit <- lm(price ~ material + length + power + draught + crew + year)
summary(fit)

#parameters in the summary 
# intercept for fiberglass = -11070
#intercept for wood = -11070 + 437.4 = -10632.5

sum(residuals(fit)^2)/fit$df  #variance 

par(mfrow=c(2,2))
plot(fit)

shapiro.test(residuals(fit)) #hyphotesis of omoschedasticity and zero mean of residuals (+normality)

dev.off()

#b) 
# 1. the variable dimension;
linearHypothesis(fit,
                 rbind(c(0,0,1,0,0,0,0),
                       c(0,0,0,1,0,0,0),
                       c(0,0,0,0,1,0,0)),
                 c(0,0,0)) #very low p-valu -- coefficients related to dimension are significant 

# 2. the variable quality;
linearHypothesis(fit,
                 rbind(c(0,1,0,0,0,0,0),
                       c(0,0,0,0,0,1,0)),
                       c(0,0)) #very low p-value -- coefficients related to dimension are significant
#3 variabile draught;
linearHypothesis(fit,
                 rbind(c(0,0,0,0,1,0,0)),
                       0)  #remove the single variabile draught from the model 

#d) 
fit2 <- lm(price ~ material + length + power + crew + year)
summary(fit2)

#update the parameters as above 

#e) 
new_data <- data.frame(material='fiberglass',length=10,power=1070,crew=1,year=2015,draught=1.5)
IP <- predict(fit2, newdata=new_data, interval='prediction', level=1-0.05)
IP


#EX 4) 

#a) 
library(fda)


data <- wind
data <- as.matrix(data)
data <- t(data)
head(data)
dim(data)
matplot(data,type='l',main='Porto Cervo',xlab='Day',ylab='Wind')

time <- 1:24

basis <- create.bspline.basis(rangeval=c(0,24),nbasis=12,norder=3)
data_L.fd <- Data2fd(y = data,argvals = time ,basisobj = basis)
plot.fd(data_L.fd, main="B-splines")
data_L.fd$coefs


#b) 
pca_W.1 <- pca.fd(data_L.fd,nharm=3,centerfns=TRUE)

# scree plot

plot(pca_W.1$values[1:12],xlab='j',ylab='Eigenvalues')
plot(cumsum(pca_W.1$values)[1:12]/sum(pca_W.1$values),xlab='j',ylab='CPV',ylim=c(0.8,1))

#automatic plot of the command fpca
par(mfrow=c(1,3))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)

# First three FPCs
x11()
layout(cbind(1,2,3))
plot(pca_W.1$harmonics[1,],col=1,ylab='FPC1')
plot(pca_W.1$harmonics[2,],col=2,ylab='FPC2')
plot(pca_W.1$harmonics[3,],col=3,ylab='FPC3')

#C) 
#automatic plot of the command fpca
par(mfrow=c(1,2))
plot.pca.fd(pca_W.1, nx=100, pointplot=TRUE, harm=c(1,2), expand=0, cycle=FALSE)


#first - weighted average with heavier weights in the central part of the day 
#second - contrast between first 15 hour of the day and the rest of the evening 



#D) 
par(mfrow=c(1,2))
plot(pca_W.1$scores[,1],pca_W.1$scores[,2],xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
points(pca_W.1$scores[35,1],pca_W.2$scores[35,2],col=2, lwd=4)

plot(pca_W.1$scores[,1],pca_W.1$scores[,2],type="n",xlab="Scores FPC1",
     ylab="Scores FPC2",xlim=c(-400,250))
text(pca_W.1$scores[,1],pca_W.1$scores[,2],dimnames(data)[[2]], cex=1)

#day 1 has high value of wind intensity in the first part of the day and generally an above the mean measurement of wind speed throughout the day (especially around half of the day) 