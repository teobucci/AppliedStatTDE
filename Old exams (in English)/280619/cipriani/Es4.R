rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("montserrat.txt",header=T)
n <- nrow(d)

##### PUNTO A #####

coordinates(d) <- c('x','y')
bubble(d,'speed',do.log=TRUE,key.space='bottom')

top <- data.frame(x=402476, y=4605558, distance=0)

### FITTING VARIOGRAM (ANISOTROPY AND NOT)
### -----------------------
v1.gls <- variogram(speed ~ 1,d)
v2.gls <- variogram(speed ~ 1+distance,d)

# no anis
plot(variogram(speed ~ 1,d),pch=19) # doesn't stabilize
plot(variogram(speed ~ 1+distance,d),pch=19) # much better
# y anis
plot(variogram(speed ~ 1, d, alpha = c(0, 45, 90, 135)),pch=19)
plot(variogram(speed ~ 1+distance, d, alpha = c(0, 45, 90, 135)),pch=19) # ok anisotropy



##### PUNTO B #####
# doing fit
# VGM(PSILL, MODEL, RANGE, (NUGGET)) don't put nugget if you dont want it
v <- variogram(speed ~ 1+distance, d)
v.fit <- fit.variogram(v, vgm(8, "Sph", 25))  
plot(v, v.fit, pch = 19)



### CREATING GSTAT OBJECT
### -----------------------
g.no <- gstat(id = 'sights', formula = speed ~ 1+distance, data = d, model = v.fit)
g.no$model # MODEL ESTIMATED FOR DELTA(Si)

#   model    psill    range
#    Sph   8.052111 28.32686

# Seems good




##### PUNTO C #####

x0 <- data.frame(x=20, y=30, distance=0)
coordinates(x0) <- c("x","y")
a0 <- predict(g.no, x0, BLUE = TRUE)$sights.pred
a0 # 49.15826

x0 = data.frame(x=20,y=30,distance=1)
coordinates(x0) <- c("x","y")
a1 <- predict(g.no, x0, BLUE = TRUE)$sights.pred - a0
a1 # -0.1019888



##### PUNTO D #####


coordinates(top) <- c("x","y")
pred <- predict(g.no, top, BLUE = FALSE)
pred

#   coordinates sights.pred sights.var
#                 52.49601   2.128603



