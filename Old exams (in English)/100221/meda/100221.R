#ex 4 

#a) 
data <- read.table('hotels.txt',header=TRUE)
attach(data)
car <- read.table('hotels.txt',header=TRUE)

# create dummy: 0 = urban, 1 = vegetation
DUMMY <- rep(0,length(winter))
DUMMY[which(winter=='yes')] <- 1
data <- data.frame(cbind(x,y,DUMMY,distance,price))
names(data) <- c('x','y','winter','distance','price')
coordinates(data) <- c('x','y')



v <- variogram(price ~ 1, data = data)
plot(v)
v.fit1 <- fit.variogram(v, vgm(5000, "Sph", 500))
plot(v, v.fit1, pch = 3)
v.fit1

g.tr <- gstat(formula = price ~ 1, data = data, model = v.fit1)
predict(g.tr, data[1,], BLUE = TRUE) #a0

#second model 
v2 <- variogram(price ~ distance + winter + winter:distance, data = data)
plot(v2)
v.fit2 <- fit.variogram(v2, vgm(1000, "Sph", 500))
plot(v2, v.fit2, pch = 3)
v.fit2

data

hot.gstat <- gstat(id = 'price', formula = price ~ distance + winter + winter:distance,
                     data = data, nmax = 100, model=v.fit2, set = list(gls=1))
hot.gstat

# Estimate the variogram from GLS residuals:
?variogram.gstat
v.gls<-variogram(hot.gstat)
plot(v.gls)

v.gls.fit <- fit.variogram(v.gls, vgm(1000, "Sph", 500))
plot(v.gls, v.gls.fit, pch = 19)

# Update gstat object with variogram model
hot.gstat <- gstat(id = 'price', formula = price ~ distance + winter + winter:distance,
                     data = data, nmax = 100, model=v.gls.fit, set = list(gls=1))
hot.gstat

predict(hot.gstat, data[2,], BLUE = TRUE)
predict(hot.gstat, data[1,], BLUE = TRUE)

a1 = (predict(hot.gstat, data[2,], BLUE = TRUE)$price.pred - predict(hot.gstat, data[1,], BLUE = TRUE)$price.pred)/(car[2,4]-car[1,4])
a0= predict(hot.gstat, data[2,], BLUE = TRUE)$price.pred - a1*car[2,4] #use original table

a12 = (predict(hot.gstat, data[5,], BLUE = TRUE)$price.pred - predict(hot.gstat, data[3,], BLUE = TRUE)$price.pred)/(car[5,4]-car[3,4])
a02= predict(hot.gstat, data[5,], BLUE = TRUE)$price.pred - a1*car[5,4] #use original table

s0.new <- as.data.frame(matrix(c(342399.74,5072272.75,1,248.29),1,4))
names(s0.new) <- c('x','y','winter','distance')
coordinates(s0.new) <- c('x','y')
predict(hot.gstat, s0.new)
#this is price per single night - non stationary model 