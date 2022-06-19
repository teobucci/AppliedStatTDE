library(car)
library(MASS)
library(class)
library(sp)
library(lattice)
library(geoR)
library(gstat)
rm(list = ls())
setwd('C:/Users/gianm/Desktop/TDEApplied/Exam 15 Giugno')
load('mcshapiro.test.RData')
df <- read.table("revenues.txt")
head(df)
coordinates(df)=c('x','y')
head(df)
v.t=variogram(revenue ~ population, data=df)
plot(v.t,pch=19)

v.fit2 <- fit.variogram(v.t, vgm(1000, "Gau", 400))
plot(v.t, v.fit2, pch = 3)
v.fit2

g.t <- gstat(formula = revenue ~ population, data = df, model = v.fit2)

D.s0=c(514711.6,5033903.0) # duomo

predict(g.t, df[1,], BLUE = TRUE)$var1.pred 
predict(g.t, df[1,], BLUE = TRUE)$var1.pred - predict(g.t, df[1,], BLUE = TRUE)$var1.var

mod <- lm(population ~ distance, data = df)
summary(mod)
shapiro.test(mod$residuals)

par(mfrow = c(2,2))
plot(mod)

brera <- c(514703.8,5035569.3) # brera
dist <- sqrt((brera[1]-D.s0[1])^2+(brera[2]-D.s0[2])^2)
dist
predict(mod, data.frame(distance = dist))
popul <- 6132.345

s0 = data.frame(514703.8,5035569.3,popul)
names(s0)=c('x','y','population')
coordinates(s0)=c('x','y')

guess <- predict(g.t, s0)
guess

