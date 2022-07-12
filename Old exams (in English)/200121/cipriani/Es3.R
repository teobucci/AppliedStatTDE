rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("bikes.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
d$day <- ifelse(d$day == 'No Holiday', 0,1)

############### LINEAR MODELS #############
######------------------------------------
fm <- lm(bike_count ~ day + mean_temp*day + mean_wind*day, data = d)
summary(fm) 


coefficients(fm)  # beta_i
#(Intercept)      0.000146 ***
#    day         0.583733    
#  mean_temp     0.001052 ** 
# mean_wind       0.324552    
#day:mean_temp   0.446198    
#day:mean_wind   0.779072    

# beta0.0 = 0.000146
# beta0.1 = 0.000146 + 0.583733    
# beta1.0 = 0.001052
# beta1.1 = 0.001052 + 0.446198    
# beta2.0 = 0.324552   
# beta2.1 = 0.324552 + 0.779072   


sum(residuals(fm)^2)/fm$df  # estimate of sigma^2
# sigma^2 = 2075365


##### PUNTO B #####
par(mfrow=c(2,2))
plot(fm) # ok
shapiro.test(residuals(fm)) # 0.2133
vif(fm) # not bad except day:wind


# H0: (beta1.0, beta1.1, beta2.0,beta2.1) == (0, 0, 0, 0) vs H1: (beta1.0, beta1.1, beta2.0, beta2.1) != (0, 0, 0, 0)
linearHypothesis(fm, rbind(c(0,0,1,0,0,0), 
                           c(0,0,0,1,0,0),
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0,0)) 
# 0.0002863 there is dependence on weather at 5%
linearHypothesis(fm, rbind(c(0,1,0,0,0,0), 
                           c(0,0,0,0,1,0),
                           c(0,0,0,0,0,1)), c(0,0,0)) 

#  0.009922 there is also dep. on day

##### PUNTO C #####
# there is an overall dep. on holiday and weather
# but single interaction or values may not be important
# for example wind could be much less significant than temp

linearHypothesis(fm, rbind(c(0,0,0,1,0,0), 
                           c(0,0,0,0,0,1)), c(0,0))  #  0.5748
# let's try removing interaction day:mean_wind (also higly collinear) and wind overall

fm2 <- lm(bike_count ~ day + mean_temp*day, data = d)
summary(fm2) 

# interaction mean_temp and day still not significant

fm3 <- lm(bike_count ~ day + mean_temp, data = d)
summary(fm3) 

coefficients(fm3)
# beta0.0 2456.94294
# beta0.1 2456.94294+1423.79191
# beta1 97.49175 

sum(residuals(fm)^2)/fm$df  #2075365

##### PUNTO D #####
predict(fm3, data.frame(mean_temp=2,mean_wind=3,day=1), interval='prediction',level=0.95)

#    fit      lwr      upr
#4075.718 1121.848 7029.589


