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

observations = observations[1:30,]
predictionLocations = predictionLocations[1:20,]

observations$obs = observations$QSUMMER_OB/observations$AREASQKM

# Setting some parameters 
params = list(gDist = TRUE, cloud = TRUE)
# Build an object
rtopObj = createRtopObject(observations,predictionLocations, params = params)
# Fit a variogram (function also creates it)
rtopObj = rtopFitVariogram(rtopObj)
#rtopObj = checkVario(rtopObj)
rtopObj$variogramModel                                                                        
rtopObj2 = rtopKrige(rtopObj, cv = TRUE)
rtopObj3 = rtopKrige(rtopObj)
rtopObj4 = rtopKrige(rtopObj2)


summary(rtopObj2$predictions)
summary(rtopObj3$predictions)
summary(rtopObj4$predictions)
all.equal(rtopObj4$predictions, rtopObj3$predictions)
#spplot(rtopObj$predictions,col.regions = bpy.colors(), c("var1.pred","var1.var"))

# Cross-validation
#spplot(rtopObj2$predictions,col.regions = bpy.colors(), c("observed","var1.pred"))
cor(rtopObj2$predictions$observed,rtopObj2$predictions$var1.pred)


#Combining with intamap - have to be at least 20 location:
set.seed(1501)
output = interpolate(observations,predictionLocations,
   optList = list(formulaString = obs~1, gDist = TRUE, cloud = TRUE, nmax = 10), 
      methodName = "rtop")

all.equal(rtopObj4$predictions@data$var1.pred, output$predictions@data$var1.pred)
all.equal(rtopObj4$predictions@data$var1.var, output$predictions@data$var1.var)

