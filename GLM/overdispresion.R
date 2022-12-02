set.seed(1)
x = rnorm(200, 10, 3)
eta = -2 + 0.2*x
y = floor(exp(eta + rnbinom(200, 1, mu=.8)))
par(mfrow=c(1,2))
plot(x, eta, las=1)
plot(x, y, las=1)
##
m = glm(y~x, family="poisson")
summary(m)

# Here the overdispersion is serious, and we can not trust the model estimates. In this case, we need an
# alternative link function that allows the variance to increase more than the mean. The negative binomial
# distribution is a good option.

library(MASS)
m = glm.nb(y~x)
summary(m)
