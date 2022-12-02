rm(list=ls())
plants = read.csv('alpineplants.csv')
head(plants)



#everything
m1 = lm(plants$Carex.bigelowii ~ plants$mean_T_winter + plants$max_T_winter + plants$min_T_winter + plants$mean_T_summer + plants$max_T_summer + plants$min_T_summer + plants$snow + plants$soil_moist + plants$light + plants$altitude)
#optional no need plants$ but at the end ",data=plants" instead

pairs(plants, panel = 'panel.smooth')
#This graph show the coleration between different variables, if it showed high dependence (like min T winter and mean winter), we should remove 1 in those two
library(car)
vif(m1)  #The highest value is mean winter and min winter, if the VIF greater than 3 is consider problem
#thus remove the mean of winter 


#would remove mean winter and mean summer
m2 = update(m1, .~. -plants$mean_T_winter-plants$mean_T_summer)




#Do 
vif(m2)

