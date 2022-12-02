rm(list=ls())
set.seed(100)
groups = as.factor(rep(c("Small", "Medium", "Large"), each=50))
x = c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))
#plot(groups, x, las=1, xlab="")



plot(as.numeric(groups) + rnorm(150,0,0.03), x, 
     las=1, xlab="", type="p", col="grey",
     xlim=c(0.5, 3.75), xaxt="n", yaxt="n")
axis(1, 1:3, labels=levels(groups)) #give the labels large medium small on x axis
means = tapply(x, groups, mean) #13+14 draw the black dot for means
points(1:3, means, pch=16, col="black")
par(new=T)
plot(groups, x, at=c(1.3, 2.3, 3.3), boxwex=0.3,  #do the box plot
     xlim=c(0.5, 3.75), xaxt="n", yaxt="n",
     las=1, xlab="", ylab="")

