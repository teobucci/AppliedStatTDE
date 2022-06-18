# -----------
# EXERCISE 4
# -----------

rm(list=ls())
data <- read.table('colours.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

data$colour <- factor(data$colour)

library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

# ------------------------- point a

coordinates(data) <- c('x','y') # set the coordinates correctly

form <- formula(revenue ~ 1)

v.t <- variogram(form, data=data)
plot(v.t,pch=19)

v.no <- variogram(form , data=data)
v.fit1 <- fit.variogram(v.no, vgm(40, "Sph", 900))    ###vgm(sill, 'type', range, nugget)
plot(v.no, v.fit1, pch = 3)
v.fit1




# ------------------------- point b

formula2 <- formula(revenue ~ colour)

v.no2 <- variogram(formula2, data=data)
plot(v.no2,pch=19)
v.fit2 <- fit.variogram(v.no2, vgm(5, "Sph", 1200))    ###vgm(sill, 'type', range, nugget)
plot(v.no2, v.fit2, pch = 3)
v.fit2

g <- gstat(formula = formula2, data = data, model = v.fit2)

predict(g, data[1,], BLUE = T)$var1.pred # red
predict(g, data[2,], BLUE = T)$var1.pred # yellow
predict(g, data[3,], BLUE = T)$var1.pred # orange

# ------------------------- point c

# model 2 because the first one doesn't explain spacial variability since it's almost a horizontal line


# ------------------------- point d

datum.r <- data.frame(x = 514811.55, y = 5037308.54, colour = 'red')
datum.y <- data.frame(x = 514811.55, y = 5037308.54, colour = 'yellow')
datum.o <- data.frame(x = 514811.55, y = 5037308.54, colour = 'orange')

coordinates(datum.r)=c('x','y')
coordinates(datum.y)=c('x','y')
coordinates(datum.o)=c('x','y')

guess <- predict(g, datum.r)
guess

guess <- predict(g, datum.y)
guess

guess <- predict(g, datum.o)
guess








































































































































