varMat.SpatialLinesDataFrame = function(object,object2 = NULL,...) {
  if (is(object2,"SpatialPolygonsDataFrame")) object2 = as(object2,"SpatialPolygons")
  varMat(as(object,"SpatialPolygons"), object2, ...)

}



varMat.SpatialLines = function(object, object2 = NULL, variogramModel,
     overlapObs, overlapPredObs, ...) {
     if (!"rstype" %in% names(params)) params$rstype = "regular"
  varMatDefault (object,object2,variogramModel,
     params, overlapObs, overlapPredObs, ...) 
}


rtopVariogram.SpatialLinesDataFrame = function(object, ... ) {
  if (missing(object))  stop("rtopVariogram: Observations are missing")
  obs = object@data
  coordinates(obs) = getSpatialLinesMidPoints(object)
  if ("length" %in% names(object)) {
    obs$length = object$length
  } else  obs$length = SpatialLinesLengths(obs)
  rtopVariogram(obs, ...)
}



rtopDisc.SpatialLinesDataFrame = function(object, params = list(), bb = bbox(object), ...) {
  rtopDisc(as(object,"SpatialLines"), params = params, bb, ...)
}


rtopDisc.SpatialLines = function(object, params = list(), bb = bbox(object), ...) {
  
  if (!"rstype" %in% names(params)) params$rstype = "regular"
  params = getRtopParams(params, observations = object, ...)
  stype = params$rstype
  resol = params$rresol
  if (stype == "random" | stype == "regular") {
    lapply(object@polygons,FUN=function(pol) spsample(pol,resol,stype,offset=c(0.5,0.5)))
  } else stop(paste("Sampling type unknown or not possible for SpatialLines:",stype))
}


rtopKrige.SpatialLinesDataFrame = function(object, predictionLocations = NULL,
    varMatObs, varMatPredObs, varMat, params = list(), formulaString,  
    sel, ...) {
    rtopKrige.default(object, predictionLocations, varMatObs, 
          varMatPredObs, varMat, params, formulaString,  
          sel, ...) 
}

rtopFitVariogram.SpatialLinesDataFrame = function(object, params=list(),...) {
  if (!inherits(params,"rtopParams")) params = getRtopParams(params)
  vario = rtopVariogram(object,params,...)
  rtopFitVariogram(vario = vario, observations = object,params = params,...)
}



findOverlapLines = function(lines1, lines2, debug.level = 1) {
  warning("The definition of overlapping lines is not clear, and it is here assumed that they are never overlapping")
  n1 = length(sapply(lines1@lines,slot,"ID"))
  n2 = length(sapply(lines1@lines,slot,"ID"))
  matrix(0,nrow = n1, ncol = n2)
}


findOverlapOld = function(object1, object2, debug.level = 1) {
  if (inherits(object1,"SpatialLines")) {
    findOverlapLines(object1,object2,debug.level = debug.level)
  } else findOverlapAreas(object1,object2,debug.level = debug.level)
}

