rm(list = ls()) 
load('mcshapiro.test.RData')
df = read.table("musicCountry.txt", header = TRUE)

##### Problem 2 #####
values = df[,1:2]
groups = as.factor(df[,3])


# verify assumptions 1)
# 1) normality within the groups
p_val = NULL
for (i in levels(groups)){
  p_val = c(p_val, mcshapiro.test(values[which(groups==i),])$p)
}
p_val  # 0.1296 0.9864 OK

# 2) equal variance (univariate)
bartlett.test(values, groups)   # p-value < 2.2e-16
### NOt verified so qda

library(MASS)
prior = c(0.1, 0.9)

values_qda = qda(values, groups, prior = prior)
values_qda
# Group means:
#   price average.length
# Germany 59.82982       5.891951
# US      29.74888       4.377556

predict_qda = predict(values_qda, values)


# 1) Compute the APER
G = 2
misc = table(class.true=groups, class.assigned=predict_qda$class)
APER = 0
for(g in 1:G)
  APER = APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]
APER  # 0.03684211

#               class.assigned
# class.true        Germany  US
#           Germany      27   9
#           US            2 150

# 2) Compute the estimate of the AER by leave-one-out cross-validation
cv_lda = qda(values, groups, CV=TRUE, prior = prior)  # specify the argument CV

misc_cv = table(class.true=groups, class.assignedCV=cv_lda$class)
AER = 0
for(g in 1:G)
  AER = AER + sum(misc_cv[g,-g])/sum(misc_cv[g,]) * prior[g]
AER  # 0.04868421


# Plot the partition induced by QDA
par(mfrow=c(1,1))
plot(values, main='Values', pch=20, col = as.numeric(groups)+1)
legend("topright", legend=levels(groups), fill=1:length(levels(groups))+1)
points(values_qda$means, pch=4,col=1:length(levels(groups))+1, lwd=2, cex=1.5)

x  = seq(min(values[,1]), max(values[,1]), length=200)
y  = seq(min(values[,2]), max(values[,2]), length=200)
xy = expand.grid(x, y)

z  = predict(values_qda, xy)$post  # these are P_i*f_i(x,y)  
z1 = z[,1] - z[,2] 
z2 = z[,2] - z[,1]  
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)


## POINT C
colMeans(predict_qda$posterior)

# Germany        US 
# 0.1631317 0.8368683 

## POINT D
new1 = data.frame(price=50, average.length=3.5)

predi = predict(values_qda, new1)

# $class
# [1] US
# Levels: Germany US
# 
# $posterior
# Germany        US
# [1,] 0.07680842 0.9231916

## POINT D
library(e1071)
x = values
y = groups

dat = data.frame(x=x, y=as.factor (y))
svmfit = svm(y~., data=dat , kernel ='linear', cost =10, scale =FALSE)
summary(svmfit)

par(mfrow=c(1,2))
plot(svmfit , dat, col =c('salmon', 'light blue'), pch=19, asp=1)

# support vectors are indicated with crosses
# they are:
svmfit$index

# To set the parameter C we can use the function tune(),
# which is based on cross-validation (10-fold)
tune_out = tune(svm,y~.,data=dat ,kernel = 'linear',
                ranges =list(cost=c(0.001, 0.01, 0.1, 1, 10, 100) )) # add val in list
summary(tune_out)

# Extract the best model from the result of tune
bestmod = tune_out$best.model
summary(bestmod)

## cost:  10 
## Number of Support Vectors:  28

plot(bestmod , dat, col =c('salmon', 'light blue'), pch=19, asp=1)
 
# Prediction for a new observation (command predict())
new2 = data.frame(x=as.matrix(new1))
ypred = predict(bestmod,new2)

## US










