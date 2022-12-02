# set.seed(1)
# #x = rpois(200, 3) #random density for poisson distribution.
# #hist(x, las=1)
# 
# ###
# 
# x = seq(0, 20, 1)
# y = dpois(x, lambda=1)
# #This function is used for illustration of Poisson density in an R plot. 
# #The function dpois() calculates the probability of a random variable that is available within a certain range.
# 
# plot(x,y, type="b", las=1, xlab="k", ylab="P(x=k)", pch=16, col=1)
# points(x, dpois(x, lambda=3), type="b", pch=16, col=2)
# points(x, dpois(x, lambda=10), type="b", pch=16, col=3)
# 
# #Three plot function above to illustrate the larger the lambda, it would go to normal distribution
# 
# legend("topright", col=1:3, pch=16,
#        legend=c(expression(paste(lambda, " = 1")),
#                 expression(paste(lambda, " = 3")),
#                 expression(paste(lambda, " = 10"))))
# #Chu thich goc phai tren


#####

x2 = rnorm(200, 10, 3)
eta = -2 + 0.2*x2
y2 = ceiling(exp(eta + rpois(200, 0.3)))
par(mfrow=c(1,2))
plot(x2, eta, las=1)
plot(x2, y2, las=1)


m = glm(y2~x2, family="poisson")
summary(m)


plot(x2, y2, las=1, col="darkgrey", pch=16)

xx = seq(min(x2), max(x2), 0.01)
y_hat = predict(m, newdata=list(x2=xx), type="response", se.fit=T)

#predict function to obtain the predicted values on the data scale, and to construct a 95% confidence polygon.

lines(xx, y_hat$fit)
#lines(xx, y_hat$fit+1.96*y_hat$se.fit, lty=2)
#lines(xx, y_hat$fit-1.96*y_hat$se.fit, lty=2)
polygon(c(xx, rev(xx)),
        c(y_hat$fit+1.96*y_hat$se.fit,
          rev(y_hat$fit-1.96*y_hat$se.fit)),
        col = rgb(0,1,0,.5), border = FALSE)

library(lme4)
library(MuMIn)
r.squaredGLMM(m)

1-(m$deviance/m$null.deviance)

