rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("garden.txt",header=T)
n <- nrow(d)
# ~


##### PUNTO A #####
model <- lm(extension ~ ., data = d)
summary(model)

model$coefficients
#(Intercept)       carps       maple      cherry 
#1442.86188    16.34107    28.69118    13.12381 
#stones 
#14.02307 

summary(model)$sigma^2
# 98669.49


# HP
shapiro.test(residuals(model)) #p-value = 0.8567
par(mfrow=c(2,2))
plot(model) # OKS

##### PUNTO B #####
### Test (Fisher):
# H0: (beta2, beta3) == (0, 0) vs H1: (beta2, beta3) != (0, 0)
linearHypothesis(model, rbind(c(0,0,1,0,0), 
                           c(0,0,0,1,0)), c(0,0))  # 2.2e-16 ***


# H0: (beta0, beta4) == (0, 0) vs H1: (beta0, beta4) != (0, 0)
linearHypothesis(model, rbind(c(0,1,0,0,0), 
                              c(0,0,0,0,1)), c(0,0)) # 3.822e-15 ***


##### PUNTO C #####
# The hypotheses at point B verify that couple of features
# together are significant, but single feature may not be as significant
# Let's remove one-at-the-time the ones with higher p-value: cherries

model2 <- lm(extension ~ . - cherry, data = d)
summary(model2)

model3 <- lm(extension ~ . - cherry - carps, data = d)
summary(model3)

# R^2 still high 


##### PUNTO D #####

model3$coefficients
summary(model3)$sigma^2

#(Intercept)       maple      stones 
#468.81898    45.49751    27.60341 
#[1] 98270.28


