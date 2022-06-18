# -----------
# EXERCISE 2
# -----------

rm(list=ls())
data <- read.table('orthopaedics.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point a

data$norm_abnorm <- factor(data$norm_abnorm)
abnorm <- which(data$norm_abnorm == 'AB')
norm   <- which(data$norm_abnorm == 'NO')

# verify assumptions 1) e 2): 
# 1) normality (univariate) within the groups
mcshapiro.test(data[abnorm, 1:2])
mcshapiro.test(data[norm, 1:2])

# 2) equal variance (univariate)
bartlett.test(data[abnorm, 1:2], data[norm, 1:2])
# very different covariance structure
# let's go with QDA since normality is guaranteed

# model for the data
# X|Li ~ N_p(m_i , cov)

x.mean <- colMeans(data[1:2])
# incidence      tilt 
# 57.00027  16.01644 

x.cov <- cov(data[1:2])
#           incidence     tilt
# incidence 241.69316 85.77978
# tilt       85.77978 81.03458

ab.prior <- 0.35
no.prior <- 1-0.35
prior <- c(ab.prior,no.prior)
prior

qda.trad <- qda(data[,1:2], data$norm_abnorm, prior=prior)
qda.trad
# Group means:
#     incidence     tilt
# AB  63.56038 19.58847
# NO  51.26018 12.89092


# Region
par(mfrow=c(1,1))
plot(data[,1:2], main='data', pch=20, col = data$norm_abnorm)
points(qda.trad$means, col=c('black','red'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(data[,1]), max(data[,1]), length=200)
y  <- seq(min(data[,2]), max(data[,2]), length=200)
xy <- expand.grid(x,y)

z  <- predict(qda.trad, xy)$post  
z1 <- z[,1] - pmax(z[,2])    
z2 <- z[,2] - pmax(z[,1])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

# ------------------------- point b

QdaCV <- qda(data[,1:2], data$norm_abnorm, CV=TRUE, prior = prior)  # specify the argument CV
misc <- table(class.true=data$norm_abnorm, class.assignedCV=QdaCV$class)

AERCV  <- misc[1,2]*ab.prior/sum(misc[1,]) + misc[2,1]*no.prior/sum(misc[2,])
AERCV
# 0.225

# ------------------------- point c

new <- data.frame(incidence = 60, tilt = 0)
answer <- predict(qda.trad, new)
answer
# AB

# ------------------------- point d

library(e1071)
svmfit <- svm(norm_abnorm ~ ., data=data, kernel ='linear', gamma = 1, cost = 0.1)
summary(svmfit)

# Plot the SVM
plot(svmfit , data, col =c('salmon', 'light blue'), pch=19, asp=1)

predict(svmfit, newdata = new, data=data)
# NO











































































































































