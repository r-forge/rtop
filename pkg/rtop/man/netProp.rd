\name{netProp}
\alias{netProp}
\title{
Propagate values along a river network
}
\description{
Pass values along a river network when the river network has more segments
than the prediction polygons.
}
\usage{
netProp(network, from = "FROMJCT", to = "TOJCT", pred = "pred")
}
\arguments{
\item{network}{ object of class \code{\link[sp:SpatialLines]{SpatialLinesDataFrame}}
            describing the river network}
\item{from}{ name of the column giving the endpoint ID of each line segment }
\item{to}{ name of the column giving the start ID of each line segment }
\item{pred}{ name of column with predictions }
}
\value{
The function will propagate the predictions upwards along the river network. 
The result is a \code{\link[sp:SpatialLines]{SpatialLinesDataFrame}} with 
predictions for all line segments, which can easier be plotted.
}

\author{ Jon Olav Skoien }
\examples{
\dontrun{
rpath = system.file("extdata",package="rtop")
setwd(rpath)
observations = readOGR(".", "obsObero")
predictionLocations = readOGR(".", "predObero")
observations$obs = observations$QSUMMER/observations$AREASQKM

# Setting some parameters 
params = list(geoDist = TRUE, rresol = 25, cloud = FALSE, model = "Sph")
# Build an object
rtopObj = createRtopObject(observations,predictionLocations, formulaString = obs~1,params = params)
# Fit a variogram (function also creates it)
rtopObj = rtopFitVariogram(rtopObj)
# Check the variogram fit
rtopObj = checkVario(rtopObj, cloud = TRUE, identify = TRUE)
# Predicting at prediction locations
rtopObj = rtopKrige(rtopObj)
# Cross-validation
rtopObj = rtopKrige(rtopObj,cv=TRUE)
cor(rtopObj$predictions$observed,rtopObj$predictions$var1.pred)

rnet = readOGR(".", "rnet")
pred = rtopObj$predictions
rnet$pred = 
    pred$var1.pred[match(rnet$TOJCT, pred$JCTID)]

# will only plot for a few discontinous river segments
spplot(rnet, "pred", col.regions = bpy.colors())
rnet = netProp(rnet)
# will show a prediction for all segments
spplot(rnet, "pred", col.regions = bpy.colors())

 }
}
\keyword{plot}
