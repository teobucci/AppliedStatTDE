rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("piadeina.txt",header=T)
n <- nrow(d)

##### PUNTO A #####
fm <- lm(Sales ~ ., data = d)
summary(fm) 

coefficients(fm)  # beta_i
#(Intercept)             Day.of.WeekMon 
#39.55618551                27.98968044 
#Day.of.WeekThu             Day.of.WeekTue 
#58.43811623                75.52314017 
#Day.of.WeekWed                 Bread.Sold 
#47.62706893                -1.65050062 
#Wraps.Sold              Sandwich.Sold 
#-0.00945242                 2.58290498 
#Focaccia.Sold               Piadina.Sold 
#-1.69625190                 7.87008186 
#Chips.Sold                Juices.Sold 
#1.25517303                -1.75502867 
#Total.Soda.and.Coffee.Sold Max.Daily.Temperature 
#1.16381621                -0.20117773 

sum(residuals(fm)^2)/fm$df  #600.0329 sigma_2

par(mfrow=c(2,2))
plot(fm) # ok
shapiro.test(residuals(fm)) # 0.1216
vif(fm) # good






##### PUNTO B #####

x <- model.matrix(Sales ~ ., data = d)[,-1]
y <- d$Sales

fit.lasso <- glmnet(x,y, lambda = 5) # default: alpha=1 -> lasso 
coef.lasso <- predict(fit.lasso, s=5, type = 'coefficients')
coef.lasso 

#s1
#(Intercept)                29.5749012
#Day.of.WeekMon              .        
#Day.of.WeekThu              0.5874999
#Day.of.WeekTue             10.4478449
#Day.of.WeekWed              .        
#Bread.Sold                  .        
#Wraps.Sold                  0.3740994
#Sandwich.Sold               0.2264938
#Focaccia.Sold               .        
#Piadina.Sold                5.4463405
#Chips.Sold                  .        
#Juices.Sold                 .        
#Total.Soda.and.Coffee.Sold  1.9728695
#Max.Daily.Temperature       .   


##### PUNTO C #####
lambda.grid <- seq(100,0,length=100)
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 
# [note: if alpha=0 -> ridge regression]

plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso # 3.030303
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 


#s1
#(Intercept)                25.2109176
#Day.of.WeekMon              .        
#Day.of.WeekThu              7.1066961
#Day.of.WeekTue             16.9691128
#Day.of.WeekWed              .        
#Bread.Sold                  .        
#Wraps.Sold                  0.3810540
#Sandwich.Sold               0.5031305
#Focaccia.Sold               .        
#Piadina.Sold                6.0608916
#Chips.Sold                  0.2865334
#Juices.Sold                 .        
#Total.Soda.and.Coffee.Sold  1.8987155
#Max.Daily.Temperature       .        


