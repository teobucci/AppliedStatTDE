rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("airfoil.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
d$vel <- ifelse(d$velocity == 'L', 0,1) # low 0 high 1
fm <- lm(sound ~ vel + vel:frequency + frequency, data = d)
summary(fm) 


coefficients(fm)  # beta_i
#(Intercept)           vel     frequency   vel:frequency 
#1.233592e+02  1.185642e+02  9.761214e-03  2.514790e-03 

# beta0.L = 1.23e02
# beta0.H = 1.23e02 + 1.18e02
# beta1.L = 9.76e-03
# beta1.H = 9.76e-03 + 2.51e-03

sum(residuals(fm)^2)/fm$df # 1922.932


par(mfrow=c(2,2))
plot(fm) # ok
shapiro.test(residuals(fm)) #0.5268
vif(fm) #     4.703664      1.778714      4.691128 

##### PUNTO B #####
linearHypothesis(fm, rbind(c(0,0,1,0), 
                           c(0,0,0,1)), c(0,0))  #2.043e-11 ***

linearHypothesis(fm, rbind(c(0,1,0,0), 
                           c(0,0,0,1)), c(0,0))  #7.229e-14 ***

linearHypothesis(fm, rbind( 
                           c(0,0,0,1)), c(0)) #  0.2906 Nope



##### PUNTO C #####
fm2 <- lm(sound ~ vel + frequency, data = d)
summary(fm2) 

coefficients(fm2)  # beta_i
#(Intercept)           vel     frequency  
#111.27032303 144.44084722   0.01079504

# beta0.L = 111.27
# beta0.H = 111.27 + 144.44
# beta1 = 0.011


sum(residuals(fm2)^2)/fm$df # 1970



##### PUNTO D #####
new <- data.frame(frequency = 15000, vel = 1)
predict(fm2, new, level=0.95, interval='confidence')

#     fit      lwr      upr
# 417.6368 393.8384 441.4352

