##### Problem 4 ####

graphics.off()
remove(list=ls())
load("mcshapiro.test.RData")
df = read.table("temperatures.txt", header = TRUE)
df$park = as.factor(df$park)
df$year = as.factor(df$year)

###### Point a ####

n = dim(df)[1]
p = dim(df)[2]

library(sp)           ## data management
library(lattice)      ## data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

## gls to estimate parameter

coordinates(df) = c('x','y')


v.t = variogram(temperature ~ year , data=df)
plot(v.t,pch=19)

# Check assumprion on isotropy
plot(variogram(temperature ~ year, df, alpha = c(0, 45, 90, 135)),pch=19) # OK

# Choose values from the previous plot
v.fit1 = fit.variogram(v.t, vgm(0.07, "Sph", 2000))    ###vgm(psill, 'type', range, nugget)
plot(v.t, v.fit1, pch = 3)
v.fit1

#   model      psill    range
#    Sph 0.07875781 2145.213

g.t = gstat(formula = temperature ~ year , data = df, model = v.fit1, set = list(gls=1)) #


# Estimate the mean: use the argument 'BLUE=TRUE' otherwise the observation
a_2003 = predict(g.t, df[1,], BLUE = TRUE)$var1.pred  # 35.66398
a_2022 = predict(g.t, df[65,], BLUE = TRUE)$var1.pred  # 30.98347



###### Point b ####

v.t_2 = variogram(temperature ~ park , data=df)
plot(v.t_2,pch=19)

# Check assumprion on isotropy
plot(variogram(temperature ~ park, df, alpha = c(0, 45, 90, 135)),pch=19) # OK

# Choose values from the previous plot
v.fit2 = fit.variogram(v.t_2, vgm(5, "Sph", 500))    ###vgm(psill, 'type', range, nugget)
plot(v.t_2, v.fit2, pch = 3)
v.fit2

# model    psill    range
#  Sph 5.675135 305.2353

g.t_2 = gstat(formula = temperature ~ park , data = df, model = v.fit2, set = list(gls=1)) #


# Estimate the mean: use the argument 'BLUE=TRUE' otherwise the observation
b_park = predict(g.t_2, df[3,], BLUE = TRUE)$var1.pred  # 33.21578
b_no_park = predict(g.t_2, df[1,], BLUE = TRUE)$var1.pred  # 33.38337



###### Point c ####

# model b is almost stationary so does not explain the variance
# model a is better


###### Point d ####
s0 = c(513852.78, 5035411.95)
new = data.frame(x=s0[1], y = s0[2], temperature=0, year=2022, park=0)
new2 = data.frame(x=s0[1], y = s0[2], year=as.factor(2003), park=as.factor(2))

df_new = data
df_new[129,]=new
df_new$park = as.factor(df_new$park)
df_new$year = as.factor(df_new$year)
coordinates(df_new)=c('x','y')
predict(g.t, df_new[129,], BLUE = FALSE)  # 31.24885 0.02190595 Universal
coordinates(new2)=c('x','y')
predict(g.t, new2, BLUE = FALSE) # 31.24885 0.02190595 Universal


