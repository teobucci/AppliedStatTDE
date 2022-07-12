rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("revenues.txt",header=T)
n <- nrow(d)

duomo = data.frame(x=514711.6, y=5033903.0)
coordinates(d) <- c('x','y')

bubble(d,'revenue',do.log=TRUE,key.space='bottom')

##### PUNTO A #####
v.gls <- variogram(revenue ~ 1+population,d)
# no anis
plot(variogram(revenue ~ 1+population,d),pch=19) 
# y anis
plot(variogram(revenue ~ 1+population, d, alpha = c(0, 45, 90, 135)),pch=19) # no anis?


# fit
v <- variogram(revenue ~ 1+population, d)
v.fit <- fit.variogram(v, vgm(400, "Gau", 2000))  
plot(v, v.fit, pch = 19)

g.no <- gstat(id = 'sights', formula = revenue ~ 1+population, data = d, model = v.fit)
g.no$model # MODEL ESTIMATED FOR DELTA(Si)

#   model    psill    range
#1   Exp   580.0814 720.8052



# estimating
x0 = data.frame(x=20,y=30,population=0)
coordinates(x0) <- c("x","y")
a0 <- predict(g.no, x0, BLUE = TRUE)$sights.pred

#a0 <- predict(g.no, d[1,], BLUE = TRUE)$sights.pred
#a0
#a0 <- predict(g.no, d[2,], BLUE = TRUE)$sights.pred
#a0
#d[2,]$population
#a0 + a1 * 4034 = 56.00481
#a0 + a1* 4022 = 55.75239

#a1*(4034-4022) = 56.00481-55.75239
#a1*12 = 0.25242
#a1 = 0.021035
#a0 = -28.85038

a0 # -28.84911

x0 = data.frame(x=20,y=30,population=1)
coordinates(x0) <- c("x","y")
a1 <- predict(g.no, x0, BLUE = TRUE)$sights.pred 
a1 <- a1-a0
a1 #  0.02103468

# Model assumptions: universal kriging
# The universal Kriging model assumes z_s = m_s + delta_s for any
#  s in D (domain) where m_s is called drift and describes the non 
#  constant spacial mean variation. Moreover we assume E[delta_s] = 0 for
#   any s in D (so that E[z_s] = m_s) and that 
#   Cov(z_s1,z_s2) = Cov(delta_s1,delta_s2) for any pair. 
#   We also assume that C(•) is known and that m_s follows a linear model m_s(t) = sum_l=0^L a_l(t) f_l(s) for s in D and t in T, where f are 
#   known functions of s and a_l are coefficients independent from the spacial location.


##### PUNTO B #####
fm <- lm(population ~ distance, data = d)
summary(fm) 

par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(fm)) # 0.4431

dist = as.numeric(sqrt((duomo[1]-514703.8)^2 + (duomo[2]- 5035569.3)^2))
dist # 1666.318 
Z0.new <- data.frame(distance = dist)
Conf <- predict(fm, Z0.new, interval='confidence', level=1-0.05)  
Conf

pop <-  6132.345

x0 = data.frame(x=514703.8,y=5035569.3,population=pop)
coordinates(x0) <- c("x","y")
pred <- predict(g.no, x0, BLUE = FALSE)$sights.pred 
pred # 974.96322


##### PUNTO C #####
pred <- predict(g.no, x0, BLUE = FALSE)
pred

# 227.7854
# No we're using universal kriging, the variance is largely underestimated,
# and not very informative since it's estimated from estimated parameters


