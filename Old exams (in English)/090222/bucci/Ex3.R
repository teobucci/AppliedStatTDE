# -----------
# EXERCISE 3
# -----------

rm(list=ls())
data <- read.table('wine.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

library(MASS)
library(car)
library(rgl)

# ------------------------- point a


data$type <- factor(data$type)

fit1 <- lm(alcohol ~ type + type:sugar, data = data)
summary(fit1)
sigma <- summary(fit1)$sigma
sigma

fit1$coefficients
# beta0Red   -1.2533849 
# beta0Rose  -1.2533849-0.4585272
# beta0White -1.2533849+0.5529508
# beta1Red    0.7272803
# beta1Rose   0.6621416
# beta1White  0.5962484

shapiro.test(fit1$residuals)
par(mfrow=c(2,2))
plot(fit1)

# ------------------------- point b

summary(fit1)
# 2.2e-16 reject
# there is a significant dependence

linearHypothesis(fit1, rbind(c(0,0,0,1,0,0),
                             c(0,0,0,0,1,0),
                             c(0,0,0,0,0,1)),c(0,0,0))

# 2.2e-16 reject
# there is a significant dependence

# ------------------------- point c

linearHypothesis(fit1, rbind(c(0,1,0,0,0,0),
                             c(0,0,1,0,0,0)),c(0,0))
# don't reject (p = 0.5486) so there is evidence to say that type intercept (beta0g) coef are zero

fit2 <- lm(alcohol ~ type:sugar, data = data)
summary(fit2)
shapiro.test(fit2$residuals) # ok
par(mfrow=c(2,2))
plot(fit2)

# ------------------------- point d

new <- data.frame(type = 'Red', sugar = 20)
alpha <- 0.01
answer <- predict(fit2, newdata = new, interval = 'prediction', level = 1-alpha)
answer











































































































































