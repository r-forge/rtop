set.seed(1501)
#-----------------------------
library(rtop)
library(rgdal)
options(error = recover)

# Read directly from shape-files in data directory
rpath = system.file("extdata",package="rtop")
setwd(rpath)
observations = readOGR(".","observations")
predictionLocations = readOGR(".","predictionLocations")
#Finding a few prediction locations of them

# Setting some parameters 
params = list(gDist = TRUE, cloud = TRUE)
# Build an object
rtopObj = createRtopObject(observations,predictionLocations, params = params)
# Fit a variogram (function also creates it)
rtopObj = rtopFitVariogram(rtopObj, iprint = -1)
#checkVario(rtopObj, identify = TRUE)
rtopObj$variogramModel                                                                        
# Check the fit of the variogram 
#xyplot(gamma~dist | as.factor(acl2)+as.factor(acl1), rtopObj$varFit[rtopObj$varFit$np > 5,], cex = sqrt(vff$np)/10, pch = 16)
#plot(rtopObj$varFit$gamma,rtopObj$varFit$gammar)
# Predicting at prediction locations
rtopObj = rtopKrige(rtopObj,cv=TRUE, debug.level = 3)
rtopObj2 = rtopKrige(rtopObj, cv = TRUE,debug.level = 1)
summary(rtopObj2$predictions)
#spplot(rtopObj$predictions,col.regions = bpy.colors(), c("var1.pred","var1.var"))

# Cross-validation
#spplot(rtopObj2$predictions,col.regions = bpy.colors(), c("observed","var1.pred"))
cor(rtopObj2$predictions$observed,rtopObj2$predictions$var1.pred)





set.seed(1501)
#-----------------------------
library(rtop)
#options(error = recover)
rpath = system.file("extdata",package="rtop")
setwd(rpath)
observations = readOGR(".","observations")
predictionLocations = readOGR(".","predictionLocations")

#Combining with intamap - have to be at least 20 location:
ploc = spChFIDs(predictionLocations,as.character(c(21:23)))
observations = rbind(observations,ploc[1:2,])
predictionLocations = predictionLocations[-c(1:2),]
output = interpolate(observations,predictionLocations,optList = list(formulaString = obs~1),
      methodName = "rtop")

output$predictions@data