rm(list=ls())
data_points <- data.frame('SD'=NULL, 'CV'=NULL)
set.seed(1)
sdlogdata1 = c()
cvdata1 = c()
for (i in 1:1000) {
  x <- rnorm(n=100,mean=5,sd=1)
  sdl <- sd(log(x)) #Calculate the sd of the log of data
  cv <- sd(x)/mean(x) #Calculate CV, sd divided by mean
  data_points[i,'SD'] <- sdl #Adding value
  data_points[i,'CV'] <- cv
  }
#Joan answer
#plot(data_points$CV, data_points$SD)
#lines(1)



#Alternative
sdlogdata = c()
cvdata = c()
for (i in 1:1000) {
  x = rnorm(n=1000, mean=5, sd=1)
  lx= log(x)
  sdlogdata[i]=sd(lx)
  cv=sd(x)/mean(x)
  cvdata[i]=cv
}

plot(sdlogdata, cvdata) #must be put in the same plot function
