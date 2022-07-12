rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("tattoo.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
d$method <- ifelse(d$method == 'handmade', 1,0)

############### LINEAR MODELS #############
######------------------------------------
fm <- lm(price ~ method + dimension*method + ncolors*method, data = d)
summary(fm) 

coefficients(fm)  # beta_i
# alpha.2 16.6836237
# alpha.1 16.6836237 -4.8658803
# beta.2 8.4875665 
# beta.1 8.4875665 + 7.2498514
# gamma.2 2.5615185
# gamma.1 2.5615185 + 0.9924536 

sum(residuals(fm)^2)/fm$df  # estimate of sigma^2
# 68.20368



##### PUNTO B #####
par(mfrow=c(2,2))
plot(fm) # ok
shapiro.test(residuals(fm)) # 0.24
vif(fm) # meh


# H0: (beta1, beta2) == (0, 0) vs H1: (beta1, beta2) != (0, 0)
linearHypothesis(fm, rbind(c(0,1,0,0,0,0), 
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0)) 
# 2.2e-16 ***


##### PUNTO C #####
linearHypothesis(fm, rbind(c(0,0,0,1,0,0), 
                           c(0,0,0,0,0,1)), c(0,0)) 
# 2.916e-11 ***



##### PUNTO D #####
linearHypothesis(fm, rbind(c(0,1,0,0,0,0), 
                           c(0,0,0,0,0,1)), c(0,0))  # 0.4734
fm2 <- lm(price ~ dimension:method + ncolors, data = d)
summary(fm2) 

# alpha 93.6571 
# gamma  3.3332
# beta.1 4.36856

sum(residuals(fm)^2)/fm$df # 68.35813

##### PUNTO E #####

data.new <- data.frame(dimension=0,ncolors=0, method=0)
predict(fm2,data.new,level=0.95,interval='confidence')
#  fit      lwr      upr
#93.65706 88.33382 98.98029

data.new2 <- data.frame(dimension=6.5,ncolors=1, method=1)
predict(fm2,data.new2,level=0.95,interval='confidence')
#  fit      lwr      upr
# 125.3852 121.4934 129.277
