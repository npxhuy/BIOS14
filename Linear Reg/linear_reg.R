set.seed(85)
x = rnorm(n=200, mean=10, sd=2)
y = 0.4*x + rnorm(200, 0, 1)
#plot(x, y, las=1,
 #    xlab="Leaf length (mm)",
  #   ylab="Leaf width (mm)")

m = lm(y~x)

cf = summary(m)$coef
predvals = cf[1,1] + cf[2,1]*x
par(mfrow=c(1,2))
plot(x, y, las =1)
abline(m)
segments(x, y, x, predvals)
hist(residuals(m), las=1)

summary(m)
cov(y,x)/var(x)

coefs = summary(m)$coef
(coefs[2,1]*(mean(x) + sd(x))) - (coefs[2,1]*mean(x))

cor(x,y)^2

#BOOTSTRAPING
out=NULL #create an empty variable to contains the sloop of lin.reg

frame = data.frame(x,y) #Put x and y into a table, with the variable frame

#boostraping = sampling the data


for (i in 1:1000){ #See chapter 1
  sam = frame[sample(nrow(frame), 200, replace = TRUE),] #sample is a random sampling function
  #nrow a function take the each row in the variable frame, do it 200 times, replace = TRUE because ???
  m = lm(sam$y~sam$x) #lm is linear model, it does the linear regression
  cf = summary(m)$coef #in the summary table of m, take the coefficients part as cf
  out[i] = cf[2,1] #the i value of out, is equal to the table cf (coefficients) row 2 column 1
}

cat(sd(out))






