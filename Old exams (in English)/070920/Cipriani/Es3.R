rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("leaven.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
d$time_sq <- d$time^2
fm <- lm(volume ~ time + time_sq + yeast:time + yeast:time_sq, data = d)
summary(fm)

fm$coefficients
#   (Intercept)            time         time_sq 
#   0.98395224      0.01410756      0.01173395 
#   time:yeastsd   time_sq:yeastsd 
#   0.05293471       -0.01223433 
shapiro.test(fm$residuals) #-value = 0.9284

par(mfrow=c(2,2))
plot(fm) # nice
vif(fm) # oki

##### PUNTO B #####
linearHypothesis(fm, rbind(c(0,0,0,1,0), 
                           c(0,0,0,0,1)), c(0,0)) 

#Hypothesis:
#time:yeastsd = 0
#time_sq:yeastsd = 0
#< 2.2e-16


linearHypothesis(fm, rbind(c(0,0,1,0,1)), c(0)) 

#Hypothesis:
#time_sq:yeastsd = 0
# < 2.2e-16

# The beta for sourdough:time^2 is significantly different from 0
# The degree of polynomial for brewer is NOT
# higher than the one for sourdough

##### PUNTO C #####

# Dependence on time not relevant for brewer
fm2 <- lm(volume ~ time_sq + yeast:time + yeast:time_sq, data = d)
summary(fm2)

# Non funziona, questo andava fatto per
# forza con le dummies...



##### PUNTO D #####
# Sordough seems better given coefficients
new_obs <- data.frame(time=2,yeast="sd",time_sq=4)
Conf <- predict(fm, new_obs, interval='confidence', level=1-0.05)  
Conf


#  fit      lwr      upr
# 1.116035 1.095064 1.137007
