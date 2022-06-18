rm(list=ls())
data <- read.table('toxicity.txt', header=TRUE)
head(data)

# ------------------------- point 1

mod1 <- lm(tox ~ . , data = data)
summary(mod1)
mod1$coefficients
sigma2 <- sum(mod1$residuals^2)/mod1$df
sigma2
shapiro.test(mod1$residuals) # p-value = 0.1866 ok
par(mfrow = c(2,2))
plot(mod1)
library(car)
vif(mod1)

# ------------------------- point 2

Z0   <- data.frame(C1=100,C2=0.7,C3=2,C4=4,C5=1.4,C6=3)
alpha <- 0.05
IC <- predict(mod1, Z0, interval='confidence',level=1-alpha) 
IC

# ------------------------- point 3

# Build the matrix of predictors
x <- model.matrix(tox ~ ., data = data)[,-1]
# Build the vector of response
y <- data$tox

library(glmnet)
lambda.grid <- seq(0.01,1,length = 100)
set.seed(1)
cv.lasso <- cv.glmnet(x,y, lambda=lambda.grid, nfolds = 10)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
# 0.07
par(mfrow=c(1,1))
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)


best.lasso <- glmnet(x, y, lambda = bestlam.lasso, alpha = 1) # 1 = Lasso
coef.lasso <- predict(best.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso
# (Intercept) 30.4792004
# C1           0.1454854
# C2           0.3119046
# C3           0.6826488
# C4          -6.8585675
# C5           0.2029185
# C6           5.8663341

# ------------------------- point 3

predict(best.lasso,s=bestlam.lasso,as.matrix(Z0), type="class")
# 37.06019






