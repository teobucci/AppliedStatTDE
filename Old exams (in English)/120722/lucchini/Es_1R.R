##### Problem 1 ####

remove(list=ls())
load('mcshapiro.test.RData')
df = read.table("dnd_monsters.txt", header = TRUE)
df$size = as.factor(df$size)

###### Point a ####
label = df[,9]
value = df[,-9]

n = dim(value)[1]
p = dim(value)[2]

# Boxplot
boxplot(value, las=2, col='gold')
# Different scale for hit points

value = data.frame(scale(value))

# We perform the PCA on scaled data
pc_value = princomp(value, scores=T)
summary(pc_value)


###### Point b ####

load_value = pc_value$loadings

# graphical representation of the loadings of the first 2 principal components
x11()
par(mfrow = c(2,1))
for(i in 1:2) barplot(load_value[,i], ylim = c(-1, 1))

# filter the most significant loadings
par(mar = c(1,3,0,2), mfrow = c(3,1))
for(i in 1:3) barplot(ifelse(abs(load_value[,i]) < 0.3, 0, load_value[,i]) , ylim = c(-1, 1));abline(h=0)

# Interpretation of the loadings:
# First PCs: weighted average of all except destry
# Second PCs: contrast between dextry and hit, strenght, constitution, with added mean of intellingence charisma and wisdom


###### Point c ####

# scatter around first 2 PC
x11()
par(mfrow = c(1,1))
value_scores = data.frame(pc_value$scores)
plot(value_scores[,1:2], col = (as.numeric(df$size)+1))
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(df$size), fill=2:7,bty='n')


###### Point d ####
df = read.table("dnd_monsters.txt", header = TRUE)
df_svm = df[which(df$size == "Tiny" | df$size == "Huge"),]
df_svm$size = as.factor(ifelse(df_svm$size == "Huge", 1, 0))

library(e1071)
df_tiny = data.frame(pc_value$scores[df$size=="Tiny",1:2])
df_huge = data.frame(pc_value$scores[df$size=="Huge",1:2])
df_svm = rbind(df_tiny, df_huge)
df_svm$size = rep(0,26+40) 
df_svm$size[27:66] = 1      # 1 for huge

# Fit the Support Vector Classifier (kernel = "linear")
svmfit = svm(as.factor(df_svm$size)~. , data=df_svm, kernel = 'linear', cost = 1, scale = FALSE)
summary(svmfit)

x11()
par(mfrow=c(1,2))
plot(svmfit, df_svm, col =c('salmon', 'light blue'), pch=19, asp=1)


new = data.frame(armor.class=14,hit.points=50,strength=19,dexterity=10,constitution=16,intelligence=8,wisdom=12,charisma=13)

# Projecting on the space of first 2 PCs
mean_x = colMeans(df_svm)
var_x = sapply(df_svm, FUN = sd)

new = (new - mean_x) / var_x
pc_proj = as.matrix(new) %*% pc_value$loadings[,1:2]
pc_proj

ypred = predict(svmfit,pc_proj)
# Huge

