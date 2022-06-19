library(car)
library(MASS)
library(class)
library(sp)
library(lattice)
library(geoR)
library(gstat)
rm(list = ls())
setwd('C:/Users/gianm/Desktop/TDEApplied/Exam 15 Giugno')
load('mcshapiro.test.RData')
df <- read.table("airfoil.txt")
head(df)

d <- ifelse(df$velocity == 'L', 0,1)
D <- cbind(df, velocity = d)
df$velocity <- d

mod <- lm(sound ~ frequency + velocity + frequency:velocity, data = df)
summary(mod)
plot(df[,1],df[,2], pch = 16, xlab = 'Frequency', ylab= 'Sound', col = factor(df[,3]))
coefs <- coef(mod)

beta.00 <- coefs[1]
beta.01 <- coefs[1] + coefs[3]
beta.10 <- coefs[2]
beta.11 <- coefs[2] + coefs[4]
sigma <-sd( mod$residuals)

beta.00
beta.01
beta.10
beta.11
sigma

vif(mod)
shapiro.test(mod$residuals)
par(mfrow = c(2,2))
plot(mod)

linearHypothesis(mod, rbind(c(0,1,0,0),c(0,0,0,1)),c(0,0) )# 2.043e-11
linearHypothesis(mod, rbind(c(0,0,1,0),c(0,0,0,1)), c(0,0))#7.229e-14
linearHypothesis(mod, c(0,0,0,1), 0) #0.2906


mod <- lm(sound ~ frequency + velocity, data = df)
summary(mod)
plot(df[,1],df[,2], pch = 16, xlab = 'Frequency', ylab= 'Sound', col = factor(df[,3]))
coefs <- coef(mod)

beta.00 <- coefs[1]
beta.01 <- coefs[1] + coefs[3]
beta.10 <- coefs[2]
sigma <-sd( mod$residuals)

beta.00
beta.01
beta.10
sigma

vif(mod)
shapiro.test(mod$residuals)
par(mfrow = c(2,2))
plot(mod)

newdat <- data.frame(frequency = 15000, velocity = 1)
guess <- predict(mod, newdat, interval = 'confidence')
guess

