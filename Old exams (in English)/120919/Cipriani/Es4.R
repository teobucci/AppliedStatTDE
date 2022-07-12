rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("mickey.txt",header=T)
n <- nrow(d)
d$we <- ifelse(d$day.of.the.week == 'weekend', 1,0)

d$sec <- (1 + cos((4*pi*d$day)/365))

##### PUNTO A #####
fm <- lm(waiting.time ~ we + sec:we + sec, data = d)
summary(fm) 
coefficients(fm)

#(Intercept)          we         sec      we:sec 
#16.881694   17.454313   15.505605   -2.482794 

# alpha.0 = 16.88
# alpha.1 = 16.88 + 17.45
# beta.0 = 15.505
# beta.1 = 15.05-2.48

# waiting times increase during weekends
# tourists increase waiting times but less during weekends

summary(fm)$sigma^2 # 70.76414

par(mfrow=c(2,2))
plot(fm) # ok
shapiro.test(residuals(fm)) # 0.31


##### PUNTO B #####
# H0: (alpha1, beta1) == (0, 0) vs H1: (alpha1, beta1) != (0, 0)
linearHypothesis(fm, rbind(c(0,1,0,0), 
                           c(0,0,0,1)), c(0,0)) 
# 2.2e-16 yes


##### PUNTO C #####
# the interaction between day of the year and weekend is not relevant at level 0.95
# p-value  0.155  


fm2 <- lm(waiting.time ~ we + sec, data = d)
summary(fm2) 
coefficients(fm2)

# alpha.0 = 17.44577
# alpha.1 = 17.44 + 15.07
# beta = 14.918

summary(fm2)$sigma^2  # 71.06641



##### PUNTO D #####
# maximum when cos = 1 and weekend
# 4pi * t/365 = 2n* t <- t = 365

data.new <- data.frame(we = 1, sec = 2)
predict(fm2,data.new, interval = 'confidence', level=0.95)

#        fit      lwr      upr
#1 62.35217 59.72521 64.97913
# 60 is in the conf. interval <-  YES there is statistical evidence

##### PUNTO E #####
data.new2 <- data.frame(we = 0, sec = 1 + cos((4*pi*238)/365))
predict(fm2,data.new2, interval = 'prediction', level=0.95)

#     fit      lwr      upr
#1 27.38931 10.73324 44.04538
