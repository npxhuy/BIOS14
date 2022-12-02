#How error in x- and y-variables affect the slope
rm(list=ls())
set.seed(85)
x = rnorm(n=200, mean=10, sd=2)
y = 0.4*x + rnorm(200, 0, 1)


#Add error to x
#Create a vector of standard deviation, from 0 to 5 with step by 0.5
sde = seq(0,2.5,by=0.25)

#A variable slopes as empty value
slopes = NULL
new_slopes=NULL
#We create 11 different data set, to have 11 diffent slopes value
for (i in 1:length(sde)){
  xe=x+rnorm(n=200,mean=0,sd=sde[i]) #Creat x error value with the new sd
  m=lm(y~xe)
  cf = summary(m)$coef
  slopes[i]=cf[2,1] #Take the slope value and append to the slopes variable
  k=1-(sde[i]^2)/var(xe) #calculate K
  new_slopes[i]=cf[2,1]/k #calculate the correct slopes and put it into new_slopes
}

#Error plot
plot(sde, slopes) 

#Plot the correct plot
points(sde, new_slopes, col='blue')

segments(sde,slopes,sde,new_slopes) #Connect the points



