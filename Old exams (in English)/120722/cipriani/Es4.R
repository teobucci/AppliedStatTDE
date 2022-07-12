rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("temperatures.txt",header=T)
n <- nrow(d)
library(gstat)
library(sp)           ## Data management
library(lattice)      ## Data management

coordinates(d) <- c('x','y')


##### PUNTO A #####
v.gls <- variogram(temperature ~ year,d)

# no anis
plot(variogram(temperature ~  year,d,  cutoff = 7000),pch=19) 
# y anis
plot(variogram(temperature ~ year, d, alpha = c(0, 45, 90, 135)),pch=19)

# doing fit
# VGM(PSILL, MODEL, RANGE, (NUGGET)) don't put nugget if you dont want it
v <- variogram(temperature ~ year, d,  cutoff = 7000)
v.fit <- fit.variogram(v, vgm(0.08, "Sph", 2000))  
plot(v, v.fit, pch = 19)

g.no <- gstat(id = 'sights', formula = temperature ~ year, data = d, model = v.fit)
g.no$model # MODEL ESTIMATED FOR DELTA(Si)

x0 = data.frame(x=20,y=30,year=2003)
coordinates(x0) <- c("x","y")
a0 <- predict(g.no, d[1,], BLUE = TRUE)$sights.pred #2.74
a0

x0 = data.frame(x=20,y=30,year=2022)
coordinates(x0) <- c("x","y")
a1 <- predict(g.no, d[128,], BLUE = TRUE)$sights.pred 
a1



##### PUNTO B #####
v.gls <- variogram(temperature ~ park,d)

# no anis
plot(variogram(temperature ~  park,d),pch=19) 
# y anis
plot(variogram(temperature ~ park, d, alpha = c(0, 45, 90, 135)),pch=19)

# doing fit
# VGM(PSILL, MODEL, RANGE, (NUGGET)) don't put nugget if you dont want it
v <- variogram(temperature ~ park, d)
v.fit <- fit.variogram(v, vgm(5, "Sph",900))  
plot(v, v.fit, pch = 19)

g.no <- gstat(id = 'sights', formula = temperature ~ park, data = d, model = v.fit)
g.no$model # MODEL ESTIMATED FOR DELTA(Si)

x0 = data.frame(x=20,y=30,park=0)
coordinates(x0) <- c("x","y")
a0 <- predict(g.no, x0, BLUE = TRUE)$sights.pred #2.74
a0

x0 = data.frame(x=20,y=30,park=1)
coordinates(x0) <- c("x","y")
a1 <- predict(g.no, x0, BLUE = TRUE)$sights.pred 
a1







##### PUNTO C #####

## See pdf




##### PUNTO D #####
# Rerun point A before this
x1 = data.frame(x=513852.78,y=5035411.95,year=2022)
coordinates(x1) <- c("x","y")
pr <- predict(g.no, x1)
pr





