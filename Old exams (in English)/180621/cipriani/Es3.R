rm(list=ls())
graphics.off()
load('mcshapiro.test.RData')

d <- read.table("students.txt",header=T)
n <- nrow(d)



##### PUNTO A #####
d$gender <- ifelse(d$gender == 'male', 1,0)

fm <- lm(watchtv ~ ., data = d)
summary(fm) 

coefficients(fm)  # beta_i
#(Intercept)        gender          age 
#6.4329080684  0.4381438789+int  0.1948575363 
#     height      distance         siblings 
#-0.1077501870  0.0005002154     0.7107673587 
# computertime exercisehours      musiccds 
#0.1901958654  0.0460659418       0.0033897185 
# playgames 
#0.1434152000 

sum(residuals(fm)^2)/fm$df  # 24.18147

par(mfrow=c(2,2))
plot(fm) # nice
shapiro.test(residuals(fm)) # 0.01178 no :(
vif(fm) # low

##### PUNTO B #####

x <- model.matrix(watchtv ~ ., data = d)[,-1]
y <- d$watchtv

fit.lasso <- glmnet(x,y, lambda = 0.3) 
# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=0.3, type = 'coefficients')
coef.lasso 

#s1
#(Intercept)   4.0894018365
#gender        .           
#age           0.0494139480
#height        .           
#distance      0.0003598352
#siblings      0.4526208912
#computertime  0.1422680784
#exercisehours .           
#musiccds      0.0012825007
#playgames     0.0230406929


##### PUNTO C #####


# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- 10^seq(0,-2,length=100)
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 
# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso # 0.359
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 

#(Intercept)   5.0317740011
#gender        .           
#age           0.0193852543
#height        .           
#distance      0.0003296616
#siblings      0.3971891322
#computertime  0.1336928323
#exercisehours .           
#musiccds      0.0008448082
#playgames     0.0015840953



##### PUNTO D #####
data.new = data.frame(gender=1, age=21,height=73,distance=100,siblings=1,computertime=10,
                     exercisehours=2,musiccds=35,playgames=4)

predict(fit.lasso, newx=as.matrix(data.new), s=bestlam.lasso, type = 'response')
# 7.241853


##### PUNTO E #####