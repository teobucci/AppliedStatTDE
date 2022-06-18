# -----------
# EXERCISE 4
# -----------

rm(list=ls())
data <- read.table('walesharks.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]


library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

# ------------------------- point a

coordinates(data) <- c('x','y')
data$log.sights <- log(data$sights)

v.t <- variogram(log.sights ~ log.chlorofill , data=data)
plot(v.t,pch=19)

v.fit1 <- fit.variogram(v.t, vgm(0.5, "Exp", 100000))    ###vgm(psill, 'type', range, nugget)
plot(v.t, v.fit1, pch = 3)
v.fit1

g.t <- gstat(formula = log.sights ~ log.chlorofill , data = data, model = v.fit1)

# predict hotels[1,]
# Estimate the mean: use the argument 'BLUE=TRUE' otherwise the observation
y1 <- predict(g.t, data[1,], BLUE = TRUE)$var1.pred
y2 <- predict(g.t, data[2,], BLUE = TRUE)$var1.pred

a1 = (y2-y1)/(data[2,]$log.chlorofill - data[1,]$log.chlorofill)
a0 = y1 - a1*data[1,]$log.chlorofill

# ------------------------- point b



datum <- as.data.frame(t(matrix(c(253844.8,385997.7))))
names(datum)=c('x','y')
coordinates(datum)=c('x','y')


v.t2 <- variogram(log.chlorofill ~ 1, data=data)
plot(v.t2,pch=19)

v.fit2 <- fit.variogram(v.t2, vgm(3, "Sph", 50000))    ###vgm(psill, 'type', range, nugget)
plot(v.t2, v.fit2, pch = 3)
v.fit2

g.t2 <- gstat(formula = log.chlorofill ~ 1 , data = data, model = v.fit2)
pred.log.sight <- predict(g.t2, data[1,], BLUE = TRUE)$var1.pred


datum.pred <- as.data.frame(t(matrix(c(253844.8,385997.7, pred.log.sight))))
names(datum.pred)=c('x','y','log.chlorofill')
coordinates(datum.pred)=c('x','y')
predict(g.t, datum.pred, BLUE = FALSE)$var1.pred
# 5.689641

# ------------------------- point c

predict(g.t, datum.pred, BLUE = FALSE)$var1.var
# 0.4041272
# the variance is not representative, since it does not account for the fact that sigma is unknown










































































































































