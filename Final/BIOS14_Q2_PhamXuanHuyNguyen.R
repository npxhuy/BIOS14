rm(list=ls())
install.packages(c("ggplot2","ggpubr","carData","mvtnorm",
                   "survival","MASS","TH.data","multcomp"))
library(ggplot2)
library(ggpubr)
library(carData)
library(car)
library(mvtnorm)
library(survival)
library(MASS)
library(TH.data, warn.conflicts = FALSE)
library(multcomp)

# set working directory
setwd("~/Documents/Study/MSc/BIOS13+14/14 exam")
# Read data and convert data to factor
dat = read.table("exam2022_part2.txt", header=T)
dat$density = as.factor(dat$density)
dat$sex = as.factor(dat$sex)

# Dependent variable: Mass
# Independent variable: Density 
# Covariate: Sex

# One-way ANOVA
one.way = aov(mass ~ density, data=dat)
summary(one.way)

# The mean plot
ggline(dat,x='sex',y="mass",color="density",
       add = c("mean_se","jitter"), xlab = "Sex", 
       ylab = "Mass (kg)", legend.title="Density")

# Fit ANCOVA model
ancova_model <- aov(mass ~ density + sex, data = dat)
Anova(ancova_model, type="III")

# post-Hoc test
postHocs_sex <- glht(ancova_model, linfct = mcp(sex = "Tukey"))
summary (postHocs_sex)
postHocs_density <- glht(ancova_model, linfct = mcp(density = "Tukey"))
summary(postHocs_density)















