rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')
library(glmnet)
d <- read.table("toxicity.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
fm <- lm(tox ~ ., data = d)

summary(fm) 
coefficients(fm)  # beta_i
# (Intercept)          C1          C2          C3          C4          C5 
#30.1570460   0.1491506   0.6567033   0.7284042  -7.0463329   0.4727555 
#C6 
#5.9804451 

sum(residuals(fm)^2)/fm$df  # 12.87133

##### Assumption: Eps ~ N(0, sigma^2)
#####-------------------------------------------
x11()
par(mfrow=c(2,2))
plot(fm) # ok
shapiro.test(residuals(fm)) # 0.1866

vif(fm) # good
#C1       C2       C3       C4       C5       C6 
#2.120964 1.041027 1.592528 1.491481 1.029484 1.042851 


##### PUNTO B #####

data.new <- data.frame(C1=100, C2=0.7, C3=2, C4=4, C5=1.4, C6=3)
conf <- predict(fm, data.new, interval='prediction', level = 0.95)

conf
#fit      lwr      upr
#37.40647 29.93171 44.88122

##### PUNTO C #####

x <- model.matrix(tox~.,data=d)[,-1]
y <- d$tox
lambda.grid <- 10^seq(0,-2,length=100)
fit.lasso <- glmnet(x,y,lambda = lambda.grid) # default: alpha=1 -> lasso 
# [note: if alpha=0 -> ridge regression]

x11()
plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

# using CV
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso  # best lambda =  0.178865

x11()
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 

#(Intercept) 30.6077073
#C1           0.1396967
#C2           .        
#C3           0.6256898
#C4          -6.5777359
#C5           .        
#C6           5.7050770


##### PUNTO D #####
new <- predict(fit.lasso, s=bestlam.lasso, newx = as.matrix(data.new))
new # 36.63304

