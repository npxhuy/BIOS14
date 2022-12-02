``{r}
rm(list=ls())
dat=read.csv('butterflies.csv',header = TRUE)
head(dat)

``
```{r, fig.height=4, fig.width=4, echo=T}

dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
ses = tapply(dat$DevelopmentTime, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means
ses

plot(c(0.97, 1.03), means[1,], ylim=c(18, 40), xlim=c(0.8, 2.2),
     xlab="Larval host", 
     ylab="Developmental time (days)",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))
arrows(c(0.97,1.03), means[1,]-ses[1,], c(0.97,1.03), 
       means[1,]+ses[1,], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[2,]-ses[2,], c(1.97,2.03), 
       means[2,]+ses[2,], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[2,1], lty=2)
segments(1.03, means[1,2], 2.03, means[2,2])
points(c(0.97, 1.03), means[1,], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[2,], pch=c(21, 16), bg="white")
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"), 
       bty="n", pch=c(NA,21,16))

```

```{r}
names(dat)
m = lm(DevelopmentTime~MaternalHost*LarvalHost, data=dat)
anova(m)
summary(m)
```

#library(ggplot2)
#ggplot(dat,aes(dat$LarvalHost,dat$DevelopmentTime))+geom_boxplot()
#ggplot(dat,aes(dat$MaternalHost,dat$DevelopmentTime))+geom_boxplot()


# https://dzchilds.github.io/stats-for-bio/two-way-anova-in-r.html








