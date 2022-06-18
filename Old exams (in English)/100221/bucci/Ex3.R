data <- read.table('landslides.txt', header=T)
n <- dim(data)[1]
p <- dim(data)[2]
head(data)

# ------------------------- point 1

fit1 <- lm(rate ~ ., data = data)
summary(fit1)
sum(residuals(fit1)^2)/fit1$df  # estimate of sigma^2
shapiro.test(fit1$residuals)
par(mfrow=c(2,2))
plot(fit1)

# ------------------------- point 2

fit2 <- lm(rate ~ . -hardness, data = data)
summary(fit2)
shapiro.test(fit2$residuals)
par(mfrow=c(2,2))
plot(fit2)

# ------------------------- point 3

# H0: coarse - 2 fine = 0
# H1: coarse - 2 fine =/ 0
library(car)
linearHypothesis(fit2, rbind(c(0,0,1,-2)),c(0))
# we don't reject H0, thus coarse - 2 fine = 0
# p = 0.9836
# we can remove coras as it's a linear function of fine
fit3 <- lm(rate ~ rain + fine, data = data)
summary(fit3)

# ------------------------- point 4

new <- data.frame(rain = 700, fine = 8)
alpha <- 0.01
predict(fit3, newdata = new, interval = 'confidence', level = 1-alpha)
# fit      lwr      upr
# 30.54783 30.26596 30.82971













