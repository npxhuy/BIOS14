rm(list=ls())
install.packages(c("ggplot2","ggpubr","carData"))
library(ggplot2)
library(ggpubr)
library("car") 
# set working directory
setwd("~/Documents/Study/MSc/BIOS13+14/14 exam")
# read data and remove NA in the data
dat = read.csv("exam2022_part1.csv")
dat<-dat[!is.na(dat$LBW),]

# The mean plot
ggline(dat,x='sp',y='LBW',color='treat',
       add = c("mean_se","jitter"), xlab = "Species", 
       ylab = "Lower bract width (mm)", legend.title="Treatment")

#Two-way ANOVA
twoANOVA = aov(LBW ~ sp*treat, data = dat)
summary(twoANOVA)

# Check the homogeneity of variance assumption
plot(twoANOVA,1)
leveneTest(LBW ~ sp*treat, data=dat)

# Check the normality assumption with Shapiro-Wilk test
plot(twoANOVA,2)
aov_residuals = residuals(object = twoANOVA)
shapiro.test(aov_residuals)

#Post-hoc tests
#TukeyHSD test
TukeyHSD(twoANOVA)



