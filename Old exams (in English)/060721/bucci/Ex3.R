# -----------
# EXERCISE 3
# -----------

rm(list=ls())
data <- read.table('pc.txt', header=TRUE)
load('mcshapiro.test.RData')
head(data)
names(data)

n <- dim(data)[1]
p <- dim(data)[2]

# ------------------------- point a

fit1 <- lm(price ~ freq:OS + cache_acc:OS, data = data)
summary(fit1)
summary(fit1)$sigma^2
# 11497.83
fit1$coefficients

# ------------------------- point b

shapiro.test(fit1$residuals) # ok
par(mfrow=c(2,2))
plot(fit1)

linearHypothesis(fit1, rbind(c(0,1,0,0,0,0,0),
                             c(0,0,1,0,0,0,0),
                             c(0,0,0,1,0,0,0),
                             c(0,0,0,0,1,0,0),
                             c(0,0,0,0,0,1,0),
                             c(0,0,0,0,0,0,1)),c(0,0,0,0,0,0))

# reject --> the OS is significant

# ------------------------- point c

linearHypothesis(fit1, rbind(c(0,0,0,0,1,0,0),
                             c(0,0,0,0,0,1,0),
                             c(0,0,0,0,0,0,1)),c(0,0,0))

# don't reject --> the access time to the cache is not significant

# ------------------------- point d

fit2 <- lm(price ~ freq:OS, data = data)
summary(fit2)

# ------------------------- point e

new <- data.frame(freq = 3.2, OS = 'Windows' )
alpha <- 0.1
answer <- predict(fit2, newdata = new, interval = 'confidence', level = 1-alpha)
answer









































































































































