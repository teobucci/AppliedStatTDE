rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("focaccia.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
d$wd <- ifelse(d$day == 'weekday', 1,0)

fm <- lm(kg ~ wd + wd:t + t, data = d)
summary(fm)

fm$coefficients
#(Intercept)(wend)      wd       t (wend)       wd:t 
#108.6863770      -73.4140787   1.6302864   0.5043036 

summary(fm)$sigma^2 # 185.3416

# beta0.0 = 108.68 intercept for weekends
# beta0.1 = 108.68 -73.41 intercept for weekdays
# beta1.0 = 1.6303 coeff for weekends
# beta1.1 = 1.6303 + 0.504 coeff for weekdays
# sigma   = 13.61 error std.dev


par(mfrow=c(2,2))
plot(fm) # oki
shapiro.test(residuals(fm)) # 0.3363


##### PUNTO B #####
# H0: (beta0.1, beta1.1) == (0, 0) vs H1: (beta0.1, beta1.1) != (0, 0)
linearHypothesis(fm, rbind(c(0,1,0,0), 
                           c(0,0,0,1)), c(0,0)) # 8.246e-15 ***

linearHypothesis(fm, rbind( c(0,0,0,1)), c(0)) #  0.07309 no at level 5%


##### PUNTO C #####
fm2 <- lm(kg ~ wd + t, data = d)
summary(fm2)


fm2$coefficients
#(Intercept)          wd           t 
#  99.241803  -59.127870    1.992468 

# b0.0 = 99.24
# b0.1 = 99.24-59.13
# b1 = 1.9924

summary(fm2)$sigma^2 # 196.4292


##### PUNTO D #####

new <- data.frame(t=61, wd = 1)
predict(fm2, new) # 161.6545 

