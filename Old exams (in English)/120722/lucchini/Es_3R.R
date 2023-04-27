##### Problem 3 ####

remove(list=ls())
load('mcshapiro.test.RData')
df = read.table("rent.txt", header = TRUE)
df$two.bathrooms = as.factor(df$two.bathrooms)

colnames(df)

###### Point a ####
mod = lm(price ~ two.bathrooms + 
           footage:two.bathrooms + 
               age:two.bathrooms + 
        renovation:two.bathrooms + 
         transport:two.bathrooms +
            center:two.bathrooms +
       supermarket:two.bathrooms +
              park:two.bathrooms, data = df)
summary(mod)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    2950.82579  485.38799   6.079 1.71e-08 ***
# two.bathroomsTRUE                76.45448  749.35912   0.102  0.91892
# two.bathroomsFALSE:footage       23.86056    1.58780  15.027  < 2e-16 ***
# two.bathroomsTRUE:footage        25.50310    1.50318  16.966  < 2e-16 ***
# two.bathroomsFALSE:age          -10.04729    4.88797  -2.056  0.04215 *
# two.bathroomsTRUE:age             5.34187    4.54048   1.176  0.24189
# two.bathroomsFALSE:renovation    -7.99892   18.16029  -0.440  0.66045
# two.bathroomsTRUE:renovation      2.70081   17.10305   0.158  0.87481
# two.bathroomsFALSE:transport      0.70526    1.18230   0.597  0.55203
# two.bathroomsTRUE:transport      -0.17971    1.22098  -0.147  0.88325
# two.bathroomsFALSE:center        -0.11016    0.02268  -4.856 3.91e-06 ***
# two.bathroomsTRUE:center         -0.07404    0.02637  -2.808  0.00588 **
# two.bathroomsFALSE:supermarket   -0.25634    1.01536  -0.252  0.80114
# two.bathroomsTRUE:supermarket     0.27011    1.02672   0.263  0.79297
# two.bathroomsFALSE:park          -0.17230    0.12889  -1.337  0.18398
# two.bathroomsTRUE:park           -0.35496    0.14485  -2.450  0.01581 *

###### Point b ####

library(car)
vif(mod) #  NON ok

# plot summary lm
par(mfrow = c(2,2))
plot(mod)  # OK
par(mfrow=c(1,1))

# controllo normalità dei residui
shapiro.test(mod$residuals)  #  0.6531 OK

# array of betas
coefs = coef(mod)  
summary(mod)$sigma   # 462.8797


###### Point c ####
library(glmnet)
attach(df)
x = model.matrix(price ~ two.bathrooms + 
                   footage:two.bathrooms + 
                   age:two.bathrooms + 
                   renovation:two.bathrooms + 
                   transport:two.bathrooms +
                   center:two.bathrooms +
                   supermarket:two.bathrooms +
                   park:two.bathrooms)[,-1] # matrix of predictors
detach(df)
y = df$price # vector of response
fit.lasso = glmnet(x,y, lambda = 45, alpha=1) # alpha=1 -> lasso 


# Get the coefficients for the model
coef.lasso = predict(fit.lasso, type = 'coefficients')
coef.lasso 

# s0
# (Intercept)                    2954.28242488
# two.bathroomsTRUE                 .         
# two.bathroomsFALSE:footage       20.59126191
# two.bathroomsTRUE:footage        24.33863521
# two.bathroomsFALSE:age           -4.25396034
# two.bathroomsTRUE:age             0.84029839
# two.bathroomsFALSE:renovation     .         
# two.bathroomsTRUE:renovation      .         
# two.bathroomsFALSE:transport      .         
# two.bathroomsTRUE:transport       .         
# two.bathroomsFALSE:center        -0.09434332
# two.bathroomsTRUE:center         -0.02832479
# two.bathroomsFALSE:supermarket    .         
# two.bathroomsTRUE:supermarket     .         
# two.bathroomsFALSE:park           .         
# two.bathroomsTRUE:park           -0.13420861


# Let's set lambda via CV
lambda.grid = 1:100
cv.lasso = cv.glmnet(x,y,alpha=1,nfolds=10,lambda=lambda.grid)

bestlam.lasso = cv.lasso$lambda.min
bestlam.lasso  # 11

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# best model
best_mod = glmnet(x,y, lambda = 11, alpha=1) # alpha=1 -> lasso 
coef.lasso = predict(best_mod, type = 'coefficients')
coef.lasso 

# s0
# (Intercept)                    3076.30865895
# two.bathroomsTRUE                 .         
# two.bathroomsFALSE:footage       23.01042330
# two.bathroomsTRUE:footage        25.29765737
# two.bathroomsFALSE:age           -9.09855389
# two.bathroomsTRUE:age             4.54539917
# two.bathroomsFALSE:renovation    -4.11485707
# two.bathroomsTRUE:renovation      .         
# two.bathroomsFALSE:transport      .         
# two.bathroomsTRUE:transport       .         
# two.bathroomsFALSE:center        -0.10960538
# two.bathroomsTRUE:center         -0.06355008
# two.bathroomsFALSE:supermarket    .         
# two.bathroomsTRUE:supermarket     .         
# two.bathroomsFALSE:park          -0.10227143
# two.bathroomsTRUE:park           -0.29203071


###### Point d ####
# new = data.frame(price=0, footage = 30, age = 5, renovation = 5, transport = 300, center = 1000, supermarket = 500, park = 100, two.bathrooms="FALSE")
# attach(new)
# x_test = model.matrix(price ~ two.bathrooms + 
#                    footage:two.bathrooms + 
#                    age:two.bathrooms + 
#                    renovation:two.bathrooms + 
#                    transport:two.bathrooms +
#                    center:two.bathrooms +
#                    supermarket:two.bathrooms +
#                    park:two.bathrooms)[,-1] # matrix of predictors
# detach(new)
# esito <- predict(cv.lasso, s=bestlam.lasso, newx = as.matrix(new), type="response")
# esito # 7.17543
# 

new = data.frame(price=0, footage = 30, age = 5, renovation = 5, transport = 300, center = 1000, supermarket = 500, park = 100, two.bathrooms="FALSE")
num <-c(1,0,30, 0, 5, 0, 5, 0, 300, 0, 1000, 0, 500, 0, 100,0)
t(as.matrix(coef.lasso)) %*% num   # 3580.722












