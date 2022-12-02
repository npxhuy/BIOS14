rm(list=ls())
setwd("~/Documents/Study/MSc/BIOS13+14/14/Midterm")
blossoms = read.csv('blossoms.csv')

#Homogeneity of regression slopes
library(ggplot2)
ggplot(blossoms, aes(GSD,GA, colour=pop)) + geom_point() + stat_smooth(method=lm) + 
  labs(title='Scatter plot of GA and GSD from different populations ',x="Gland-stigma Distance(mm)",y="Gland Area(mm^2)") #Show the interaction between GA and GSD in different population and their linear regression
#A lot of point fall out from the grey area, the homogen.. may violated

library(car)
md = aov(GA ~ GSD + pop + pop:GSD,data=blossoms)
Anova(md, type='III') #The interaction between GSD and population is statistical significant, then the homogeneity of regression slope is violated
# I will proceed assuming the homogeneity of regression slopes assumption is violated
# Checking indepence of the covarient GSD and the treatment group population
newmd = aov(GSD~pop, data=blossoms)
summary(newmd)
# I will proceed with assumption that we violate the indepenced assumption between the covariate and the treatment group

# goal: examine GA over different population when controlling for the GSD

mdl = lm(GA ~ GSD + pop, data=blossoms)
anova(mdl)
summary(mdl)




