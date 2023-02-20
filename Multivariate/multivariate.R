library(MASS)
library(ellipse)
set.seed(1)
X = data.frame(mvrnorm(200, mu=c(0,0,0), Sigma=cm))
colnames(X) = c("z1", "z2", "z3")

means = c(apply(X[,1:2], 2, mean))
plot(X$z1, X$z2, las=1,col='grey')
lines(ellipse(cov(X[,1:2]), centre=means))



arrows(means[1], means[2],
       means[1]+eigen(cm)$vectors[1,1],
       means[2]+eigen(cm)$vectors[2,1],
       code=2, length=0.1, lwd=2)

arrows(means[1], means[2],
       means[1]+eigen(cm)$vectors[1,2],
       means[2]+eigen(cm)$vectors[2,2],
       code=2, length=0.1, lwd=2)

ev=eigen(cm)$values
proportion_of_variance=NULL
for (i in 1:length(ev)) {
  proportion_of_variance=append(proportion_of_variance,ev[i]/sum(ev))
}

(180/pi)*acos(sum(eigen(cm)$vectors[,1]*eigen(cm)$vectors[,2]))

a =eigen(cm)$vectors
b = solve(a)
c= diag(ev)

# a %*% c %*% b

dim(as.matrix(X))
dim(as.matrix(eigen(cm)$vectors[,1]))


t1 = as.matrix(X) %*% eigen(cm)$vectors[,1]
t2 = as.matrix(X) %*% eigen(cm)$vectors[,2]
t3 = as.matrix(X) %*% eigen(cm)$vectors[,3]
c(var(X[,1]), var(X[,2]), var(X[,3]))

var(t1) + var(t2) + var(t3)

var(X[,1]) + var(X[,2]) + var(X[,3]) #same result as line 46



pca = princomp(X)
summary(pca)

pca$sdev^2/sum(pca$sdev^2)
eigen(cm)$values/sum(eigen(cm)$values) #same result as line 55

biplot(pca, col=c("grey", "black"), cex=c(.5, 1))


blo=read.csv('blossoms.csv')

Y=data.frame(blo$ASD,blo$GAD,blo$GSD, na.rm = TRUE)
