rm(list = ls()) 
load('mcshapiro.test.RData')
df = read.table("danceability.txt", header = TRUE)

df$genre = as.factor(df$genre)

df2 = df[,1:4]

##### Problem 3 #####
mod = lm(danceability ~ ., data = df2)
summary(mod)

# R2 adj = 0.1261 

library(car)
vif(mod) # hight values means colinearity

# loudness   energy    tempo 
# 2.282505 2.279888 1.001943 


# plot summary lm
par(mfrow = c(2,2))
plot(mod)

# controllo normalità dei residui
shapiro.test(mod$residuals)

## 0.6654

# array of betas
coefs = coef(mod)  
sigma = sd(mod$residuals)

# Intercept    loudness      energy       tempo 
# 9.18676786  0.09930218  0.07258300 -0.00888967

# sigma 0.9314841

# try to remove covariates
linearHypothesis(mod, rbind(c(0,1,0,0), c(0,0,1,0)), c(0,0))

# p = 2.112e-06 reject and keep at 5%
linearHypothesis(mod, rbind(c(0,1,0,0)), c(0))    # 0.0603
linearHypothesis(mod, rbind(c(0,0,1,0)), c(0))    # 0.07311

# keep all

library(ggplot2)
library(insight)
library(lattice)
library(lme4)

lmm1 = lmer(danceability ~ loudness + energy + tempo + (1|genre), 
            data = df)
summary(lmm1)


# Fixed Effects and 95% CIs
#-------------------------------
confint(lmm1, oldNames=TRUE)
fixef(lmm1)

# The fixed effects tell us there is a negative effect of being male on achievement, 
# and on average, students with higher escs are associated to higher achievement scores.


# Variance components
#--------------------
# One thing that's new compared to the standard regression output is the estimated 
# variance/standard deviation of the school effect.
# This tells us how much, on average, achievement bounces around as we move from school to school. 
# In other words, even after making a prediction based on student covariates, each school has its
# own unique deviation, and that value (in terms of the standard deviation) is the estimated 
# average deviation across schools. 

print(vc <- VarCorr(lmm1), comp = c("Variance", "Std.Dev."))
help(get_variance)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b

# Another way to interpret the variance output is to note percentage of the student variance out 
# of the total, i.e. the Percentage of Variance explained by the Random Effect (PVRE).
# This is also called the intraclass correlation (ICC), because it is also an estimate of the within 
# cluster correlation.
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

## 0.1046363

ranef(lmm1)

# The dotplot shows the point and interval estimates for the random effects, 
# ordering them and highlighting which are significantly different from the mean (0)

dotplot(ranef(lmm1))

# Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
coef(lmm1)
head(coef(lmm1)$school_id)














