rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("airport.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
############### LINEAR MODELS #############
######------------------------------------
fm <- lm(duration ~ time.of.the.day + distance*time.of.the.day, data = d)
summary(fm) 

coefficients(fm)  # beta_i
#        (Intercept)          time.of.the.day16-20 
#   17.2283433                   -16.1704029 
#time.of.the.day6-10              distance 
#-3.8908612                     1.1574155 
#time.of.the.day16-20:distance  time.of.the.day6-10:distance 
#0.2930864                     0.5829262 

sum(residuals(fm)^2)/fm$df  #  30.79203


##### Inference on the parameters
##### Assumption: Eps ~ N(0, sigma^2)
#####-------------------------------------------
par(mfrow=c(2,2))
plot(fm) # ok
shapiro.test(residuals(fm)) #0.07
vif(fm) # nih



##### PUNTO B ##### 

linearHypothesis(fm, rbind(c(0,1,0,0,0,0), 
                           c(0,0,1,0,0,0),
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0,0))  #2.2e-16 ***

linearHypothesis(fm, rbind(
                           c(0,0,0,1,0,0),
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0))  #2.2e-16 ***



##### PUNTO C #####


fm2 <- lm(duration ~ distance:time.of.the.day, data = d)
summary(fm2) 



##### PUNTO D #####

data.new <- data.frame(distance=57,time.of.the.day='6-10')
predict(fm2,data.new,level=0.99,interval='confidence')

# 112.4174 110.4296 114.4052
# leave 110 min before 9.30 <-  take 7.30 bus
