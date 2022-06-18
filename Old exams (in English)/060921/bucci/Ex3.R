rm(list=ls())
data <- read.table('boats.txt', header=TRUE)
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point 1

### Model:
### price = beta0 + beta1*length + beta2*power + beta3*draught + beta4*crew + beta5*year + beta6*material + Eps
### (linear in the parameters!)

### Assumptions:
## 1) Parameter estimation: E(Eps) = 0  and  Var(Eps) = sigma^2 
## 2) Inference:            Eps ~ N(0, sigma^2)

fit1 <- lm(price ~ length + power + draught + crew + year + material, data = data)
summary(fit1)
fit1$coefficients
sum(residuals(fit1)^2)/fit1$df  # estimate of sigma^2
shapiro.test(fit1$residuals)$p # ok
# 0.2437909
vif(fit1) # high => highly collinear with the other variables in the model
par(mfrow=c(2,2))
plot(fit1)

# ------------------------- point 2

linearHypothesis(fit1, rbind(c(0,1,0,0,0,0,0),
                             c(0,0,0,1,0,0,0),
                             c(0,0,0,0,0,0,1)),c(0,0,0))
# 2.2e-16
# size matter

# ------------------------- point 3

linearHypothesis(fit1, rbind(c(0,0,0,0,1,0,0),
                             c(0,0,0,0,0,0,1)),c(0,0))

# 2.2e-16
# accessory matter

# ------------------------- point 4

# without drought
fit2 <- lm(price ~ length + power + crew + year + material, data = data)
summary(fit2)

fit2$coefficients
#  (Intercept)        length         power          crew          year  materialwood 
#-1.157826e+04  3.205435e+02  9.921118e-02  6.520817e+02  5.046175e+00  4.363763e+02 

summary(fit2)$sigma^2 # estimate of sigma^2
# 20917.79

shapiro.test(fit2$residuals)$p # ok
# 0.2263398

par(mfrow=c(2,2))
plot(fit2)

# ------------------------- point 5

new <- data.frame(length = 10,
                  power = 1070,
                  draught = 1.5,
                  crew = 1,
                  year = 2015,
                  material = 'fiberglass')
predict(fit2, newdata = new, interval = 'prediction', level = 1-0.05)
#      fit      lwr      upr
# 2553.457 2244.648 2862.265



















