rm(list=ls())
birds = read.csv("bird_allometry.csv", header=TRUE, sep=",") #if the file had the header, use header=TRUE
head(birds)

names(birds)
birds$Sex = as.factor(birds$Sex) #calculate the data of m and f, so the summary of the birds show the numbers of f and m
summary(birds)

hist(birds$brain_mass) #because the histogram of have really high number and also low, it's best to log them

brain=birds[3] #Cai nay dem thanh mot file khac, thanh column
body=birds[4]

a <- birds$brain_mass #Cai nay thanh vector
b <- birds$body_mass
x <- log(a)
y <- log(b)

m = lm(y~x)




cf = summary(m)$coef
predvals = cf[1,1] + cf[2,1]*x
par(mfrow=c(1,2))
plot(x, y, las=1)
abline(m)





