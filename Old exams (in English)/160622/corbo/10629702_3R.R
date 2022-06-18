##################EX3
rm(list=ls())
danceability <- read.table('danceability.txt', header=TRUE)
head(danceability)

mod1 <- lm(danceability  ~ loudness    + energy    + tempo , data = danceability)
summary(mod1)

mod1$coefficients
sigma2=sum(mod1$residuals^2)/mod1$df
sigma2
summary(mod1)$sigma  #0.9350057
shapiro.test(mod1$residuals) # p-value = 0.6654 gaussiani
x11()
par(mfrow = c(2,2))
plot(mod1) 

vif(mod1)
#loudness   energy    tempo 
#2.282505 2.279888 1.001943 

library(car)
linearHypothesis(mod1, rbind(c(0,1,0,0),
                             c(0,0,1,0)), c(0,0))   #2.112e-06 ***  *** influisce

mod2 <- lm(danceability  ~  loudness    + tempo , data = danceability)
summary(mod2)

mod2$coefficients
summary(mod2)$sigma
shapiro.test(mod2$residuals) # p-value = 0.6654 gaussiani
x11()
par(mfrow = c(2,2))
plot(mod2) 
lmm1 = lmer(danceability  ~ loudness + tempo + (1|genre), 
            data = danceability)
summary(lmm1)

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
PVRE   #0.1034443

x11()
dotplot(ranef(lmm1))    # the equal length of the interval is due to the fact that all schools have the same number of obs

# Random intercepts and fixed slopes: (beta_0+b_0i, beta_1, beta_2)
coef(lmm1)
head(coef(lmm1)$school_id)


