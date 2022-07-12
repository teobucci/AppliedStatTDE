rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("rent.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
fm <- lm(price ~ .*two.bathrooms, data = d)
summary(fm) 

coefficients(fm)  # beta_i
sum(residuals(fm)^2)/fm$df  # estimate of sigma^2

##### PUNTO B #####

par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(fm))
vif(fm)

##### PUNTO C #####
x <- model.matrix(price ~ .*two.bathrooms, data = d)[,-1]
y <- d$price
lambda.grid <- 45
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 

coef.lasso <- predict(fit.lasso, s=45, type = 'coefficients')
coef.lasso 
selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]

# Let's set lambda via cross validation
lambda.grid <- 10^seq(2,0,length=100)
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)


coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 
selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]



##### PUNTO D #####
data.new <- data.frame(footage=30,age=5,renovation=5,transport=300,center=1000,
                       supermarket=500,park=100,two.bathrooms=FALSE, footage.two.bathroomsTRUE = 0,
                       age.two.bathroomsTRUE = 0,
                       renovation.two.bathroomsTRUE = 0,
                       transport.two.bathroomsTRUE = 0,
                       center.two.bathroomsTRUE = 0,
                       supermarket.two.bathroomsTRUE = 0,
                       park.two.bathroomsTRUE = 0)

predict(fit.lasso, newx=as.matrix(data.new), s=bestlam.lasso, type = 'response')

### OPPURE (più semplice)
data.new <- data.frame(inter = 1,footage=30,age=5,renovation=5,transport=300,center=1000,
                       supermarket=500,park=100)
price <- coef.lasso[1:8]%*%t(as.matrix(data.new))
price

