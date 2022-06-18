# -----------
# EXERCISE 3
# -----------

rm(list=ls())
d <- read.table('danceability.txt', header=TRUE)
load('mcshapiro.test.RData')
head(d)
names(d)

n <- dim(d)[1]
p <- dim(d)[2]

library(MASS)
library(car)
library(rgl)

# ------------------------- point a

fm <- lm(danceability ~ loudness + energy + tempo, data=d)

summary(fm) 

coefficients(fm)  # beta_i

summary(fm)$sigma  # estimate of sigma


# ------------------------- point b

par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))$p

# ------------------------- point c

linearHypothesis(fm, rbind(c(0,1,0,0), 
                           c(0,0,1,0)), c(0,0)) 


# ------------------------- point d


fm2 <- lm(danceability ~ loudness + tempo, data=d)

summary(fm2)

summary(fm2)$sigma


par(mfrow=c(2,2))
plot(fm2)
shapiro.test(residuals(fm2))$p

# ------------------------- point e

library(corrplot)
library(plot.matrix)
library(ggplot2)
library(insight)
library(lattice)
library(lme4)
library(nlme)

lmm1 = lmer(danceability ~ loudness + tempo + (1|genre), data = d)
summary(lmm1)

sigma2_eps <- as.numeric(get_variance_residual(lmm1))
sigma2_eps
sigma2_b <- as.numeric(get_variance_random(lmm1))
sigma2_b

PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE # intraclass correlation >20% high



plot(lmm1)

par(mfrow=c(1,2))
# 1) Assessing Assumption on the within-group errors
qqnorm(resid(lmm1))
qqline(resid(lmm1), col='red', lwd=2)

# 2) Assessing Assumption on the Random Effects
qqnorm(unlist(ranef(lmm1)$genre), main='Normal Q-Q Plot - Random Effects for Primary School')
qqline(unlist(ranef(lmm1)$genre), col='red', lwd=2)

# ------------------------- point f

ranef(lmm1)
dotplot(ranef(lmm1))


































































































































