rm(list=ls())
setwd("~/Documents/GitHub/BIOS14/GLM")
dat = read.csv("Eulaema.csv")

# Example, whether the local abundance of E.nigrita depends on forest cover, while accounting for variation in yearly rainfall
# To account for expected greater abundance of the study of drier areas, included mean annual precipitation as a coveraite in the model (MAP)

# m=glm.nb(dat$Eulaema_nigrita~dat$MAT+dat$forest.)
m = glm.nb(Eulaema_nigrita ~ MAP + forest., data = dat)

summary(m)
plot(dat$forest.,dat$Eulaema_nigrita)

newforest = seq(min(dat$forest.), max(dat$forest.), length.out=200)
newMAP = rep(min(dat$MAP), length(newforest))
y_hat = predict(m, newdata=list(MAP=newMAP,forest.=newforest), type='response')
lines(newforest, y_hat, lwd=2)