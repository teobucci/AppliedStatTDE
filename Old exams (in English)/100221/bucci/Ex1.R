data <- read.table('shopping.txt', header=T)
head(data)
n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point 1

# Confidence region for the mean of level 95%
# CR for the mean (ellipsoidal region) 
# { m \in R^4(?) t.c. n * (x.mean-m)' %*% (x.cov)^-1 %*% (x.mean-m) < cfr.fisher }

alpha <- 0.05
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)  

# Test of Gaussianity
mcshapiro.test(data) # ok normality

# Center:
x.mean   <- colMeans(data)
x.cov    <- cov(data)
x.invcov <- solve(data)

# Directions of the principal axes:
eigen(x.cov/n)$vectors

# Radius
r <- sqrt(cfr.fisher)

# Length of the semi-axes of the ellipse:
r*sqrt(eigen(x.cov/n)$values) 

# identify and discuss possible issues (if any)?
# men and women purchases could be not independent of the accesses

# ------------------------- point 2

data$total <- data$men + data$women

x.mean   <- colMeans(data)
x.cov    <- cov(data)
x.invcov <- solve(data)
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)  
p <- dim(data)[2]

T2 <- cbind(inf = x.mean - sqrt(cfr.fisher*diag(x.cov)/n),
            center = x.mean, 
            sup = x.mean + sqrt(cfr.fisher*diag(x.cov)/n))
T2

# ------------------------- point 3


head(data)

test <- data$total - data$accesses * 0.2
t.test(test, alternative = 'greater', conf.level = 0.95)
# p-value = 0.005305
# We have evidence to state that more than 20% converts into a purchase







