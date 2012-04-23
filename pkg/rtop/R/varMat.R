
# varMat.rtop hardly uses the functions further below, should be shortened

varMat.rtop = function(object, varMatUpdate = FALSE, ...) {
  params = getRtopParams(object$params,  ...)
  observations = object$observations
  nObs = dim(observations@data)[1]
  predictionLocations = object$predictionLocations
  variogramModel = object$variogramModel
  lgDistPred = params$gDistPred
  maxDist = params$maxDist
  debug.level = params$debug.level
  
  obsComp = FALSE
  predComp = FALSE
  if (params$cv && "varMatObs" %in% names(object) && !varMatUpdate) return(object)      
  if (!"varMatObs" %in% names(object) | varMatUpdate) {
    if (!"dObs" %in% names(object) && !(lgDistPred && "gDistObs" %in% names(object))) 
        object$dObs = rtopDisc(observations, params = params) 
    if (lgDistPred) {
      if ("gDistObs" %in% names(object)) {
        gDistObs = object$gDistObs
      } else {
        dObs = object$dObs
        object$gDistObs = gDistObs = gDist(dObs)
      }
      varMatObs = matrix(mapply(FUN = varioEx,gDistObs,MoreArgs=list(variogramModel)),
                          nrow = nObs,ncol = nObs)
      vDiagObs = diag(varMatObs)
      for (ia in 1:(nObs-1)) {
        for (ib in (ia+1):nObs) {
          varMatObs[ia,ib] = varMatObs[ia,ib] - 0.5*(vDiagObs[ia] + vDiagObs[ib])
          varMatObs[ib,ia] = varMatObs[ia,ib]
        }
      }
      object$varMatObs = varMatObs
    } else {
      if ("dObs" %in% names(object)) dObs = object$dObs
      object$varMatObs = varMat(dObs,coor1 = coordinates(observations), 
          variogramModel = variogramModel, debug.level = debug.level, newPar = params)
    }
    obsComp = TRUE
  }
  if (!params$cv && !"varMatPredObs" %in% names(object) && !varMatUpdate) {
#    ftype = ifelse(inherits(predictionLocations,"SpatialPolygons"),"polygons","lines")
    ftype = "polygons"
    nPred = length(sapply(slot(predictionLocations, ftype), function(i) slot(i, "ID")))
    if (!"dPred" %in% names(object) && !(lgDistPred && "gDistPred" %in% names(object))) 
      object$dPred = rtopDisc(predictionLocations, params = params) 
    if (!lgDistPred || !all("gDistPredObs" %in% names(object) &&  "gDistPred" %in% names(object))) {
      dObs = object$dObs
      dPred = object$dPred
    }
    if (lgDistPred) {
      if ("gDistPred" %in% names(object)) {
        gDistPred = object$gDistPred
      } else object$gDistPred = gDistPred = gDist(dPred, diag=TRUE)
      if ("gDistPredObs" %in% names(object)) {
        gDistPredObs = object$gDistPredObs
      } else object$gDistPredObs = gDistPredObs = gDist(dObs,dPred)

      print("Creating prediction semivariance matrix. This can take some time.")
      object$varMatPred = varMatPred = 
                matrix(mapply(FUN = varioEx,gDistPred,MoreArgs=list(variogramModel)),
                          nrow = nPred,ncol = 1)
      varMatPredObs = matrix(mapply(FUN = varioEx,gDistPredObs,MoreArgs=list(variogramModel)),
                          nrow = nObs,ncol = nPred)
      if (is.null(dim(varMatPred)) || dim(varMatPred)[1] != dim(varMatPred)[2]) 
          vDiagPred = varMatPred else vDiagPred = diag(varMatPred)
      for (ia in 1:nObs) {
        for (ib in 1:nPred) 
           varMatPredObs[ia,ib] = varMatPredObs[ia,ib] - 0.5*(vDiagObs[ia] + vDiagPred[ib])
      }
      object$varMatPredObs = varMatPredObs    
    } else {
   # Do full integration over variograms
      object$varMatPred = varMat(dPred,coor1 = coordinates(predictionLocations), 
                    diag=TRUE, variogramModel = variogramModel, debug.level = debug.level, newPar = params)
      object$varMatPredObs = varMat(dObs,dPred,
          coor1 = coordinates(observations),coor2 = coordinates(predictionLocations),
          variogramModel = variogramModel, debug.level = debug.level, newPar = params)
    }
    predComp = TRUE
  }
  if (params$nugget) {
    if (obsComp) {
      if ("overlapObs" %in% names(object)) {
        overlapObs = object$overlapObs
      } else object$overlapObs = overlapObs = findOverlap(observations,observations)
      if (inherits(observations,"SpatialPolygons")) {
        fObs = matrix(rep(observations$area,nObs),ncol=nObs)
#      } else if (inherits(predictionLocations,"SpatialLines")) {
#        fObs = matrix(rep(observations$length,nObs),ncol=nObs)
      }
      sObs = t(fObs)
      nuggObs = matrix(mapply(FUN = nuggEx,
            (1/fObs + 1/sObs -2*overlapObs/(fObs*sObs))/2,
             MoreArgs = list(variogramModel = variogramModel)),ncol = nObs)
      object$varMatObs = object$varMatObs + nuggObs
    }
    if (predComp) {
      if ("overlapPredObs" %in% names(object)) {
        overlapPredObs = object$overlapPredObs
      } else object$overlapPredObs = overlapPredObs = findOverlap(observations,predictionLocations)
      if (inherits(predictionLocations,"SpatialPolygons")) {
        fPredObs = matrix(rep(observations$area,nPred),ncol=nPred)
        sPredObs = t(matrix(rep(predictionLocations$area,nObs),ncol = nObs))
#      } else if (inherits(predictionLocations,"SpatialLines")) {
#        fPredObs = matrix(rep(observations$length,nPred),ncol=nPred)
#        sPredObs = t(matrix(rep(predictionLocations$length,nObs),ncol = nObs))
      }
      nuggPredObs = matrix(mapply(FUN = nuggEx,
            (1/fPredObs + 1/sPredObs -2*overlapPredObs/(fPredObs*sPredObs))/2,
            MoreArgs = list(variogramModel = variogramModel)),ncol= nPred)
      object$varMatPredObs = object$varMatPredObs + nuggPredObs
    }
  }
  object
}
    

varMat.matrix = function(object, variogramModel,  ...) {
  ndim = dim(object)[1] 
  mdim = dim(object)[2]
  matrix(mapply(FUN = varioEx,object,MoreArgs=list(variogramModel)),
                          nrow = ndim,ncol = mdim)
}


varMat.SpatialPolygonsDataFrame = function(object,object2 = NULL,...) {
  if (is(object2,"SpatialPolygonsDataFrame")) object2 = as(object2,"SpatialPolygons")
  varMat(as(object,"SpatialPolygons"), object2, ...)

}




varMat.SpatialPolygons = function(object,object2 = NULL,variogramModel,
       overlapObs, overlapPredObs, ...) {
  varMatDefault(object,object2,variogramModel,
     overlapObs, overlapPredObs, ...) 
}


varMatDefault = function(object1,object2 = NULL,variogramModel,
     overlapObs, overlapPredObs, ...) {
  params = getRtopParams(...)
  d1 = rtopDisc(object1,params) 
  if (!is.null(object2)) d2 = rtopDisc(object2,params) 
  if (params$gDistPred) {
    gDist1 = gDist(d1)
    varMat1 = varMat(gDist1, variogramModel = variogramModel, ...)
  } else {
    varMat1 = varMat(d1, variogramModel = variogramModel, ...)
  }  
  if (is.null(object2)) return(varMat1)
  varMatObs = varMat1
  
  if (params$gDistPred & !is.null(object2)) {
    gDistPred = gDist(d2, diag = TRUE)
    varMatPred = varMat(gDistPred, diag = TRUE, ...)
    gDistPredObs = gDist(d1, d2)
    varMatPredObs = varMat(gDist1,gDistPred,sub1 = diag(varMatObs),sub2 = varMatPred, ...)
  } else {
    varMatPred = varMat(d2,diag=TRUE)
    varMatPredObs = varMat(d1,d2,sub1 = diag(varMatObs),sub2 = varMatPred)
  }
  if (params$nugget) {
    if (missing(overlapObs)) 
      overlapObs = findOverlap(object1,object1)
    if (missing(overlapPredObs))
      overlapPredObs = findOverlap(object1,object2)
    
    if (inherits(object1,"SpatialPolygons")) {
      aObs = sapply(slot(object1, "polygons"), function(i) slot(i, "area"))
      aPred = sapply(slot(object2, "polygons"), function(i) slot(i, "area"))
#    } else if (inherits(object1,"SpatialLines")) {
#      aObs = SpatialLinesLengths(object1)
#      aPred = SpatialLinesLengths(object2)
    }    
    
    nObs = length(aObs)
    nPred = length(aPred)
    
    fObs = matrix(rep(aObs,nObs),ncol=nObs)
    sObs = t(fObs)
    fPredObs = matrix(rep(aPred,nPred),ncol=nPred)
    sPredObs = t(matrix(rep(aPred,nObs),ncol = nObs))
    nuggObs = matrix(mapply(FUN = nuggEx,
            (1/fObs + 1/sObs -2*overlapObs/(fObs*sObs))/2,
             MoreArgs = list(variogramModel = variogramModel)),ncol = nObs)
    nuggPredObs = matrix(mapply(FUN = nuggEx,
            (1/fPredObs + 1/sPredObs -2*overlapPredObs/(fPredObs*sPredObs))/2,
            MoreArgs = list(variogramModel = variogramModel)),ncol = nPred)
    object$varMatObs = object$varMatObs - nuggObs
    object$varMatPredObs = object$varMatPredObs - nuggPredObs
  }
  return(list(varMatObs = varMatObs,varMatPred = varMatPred,varMatPredObs = varMatPredObs))  
}


# Lists are here discretized elements
varMat.list = function(object, object2=NULL, coor1, coor2, maxdist = Inf, 
              variogramModel, diag=FALSE, sub1, sub2, debug.level = 1, ...) {
  d1 = object
  d2 = object2
  if (is.null(d2)) {
    equal = TRUE
    d2 = d1
    if (!missing(coor1)) coor2 = coor1
  } else equal = FALSE
  if (diag) {
    lmat = mapply(vred,a2 = d1,a1 = d1,MoreArgs = list(vredTyp="ind",variogramModel = variogramModel))
    return(lmat)
  }

  ndim = length(d1)
  mdim = length(d2)
  varMatrix = matrix(-999,nrow = ndim,ncol = mdim)

  t0 = proc.time()[[3]]
  for (ia in 1:ndim) {
    t1 = proc.time()[[3]]
    a1 = coordinates(d1[[ia]])
    ip1 = dim(a1)[1]
    first = ifelse(equal,ia,1)
    lorder = c(first:mdim)
    if (!missing(coor1) && ! missing(coor2) && maxdist < Inf) 
         lorder = lorder[spDistsN1(coor2[first:mdim,],coor1[ia,]) < maxdist]
    if (length(lorder) > 0) {
      ld = d2[lorder]
      lmat = mapply(vred,a2 = ld,MoreArgs = list(vredTyp="ind",a1 = a1,variogramModel = variogramModel))
#      lmat = lvmat[[1]]
      if (!equal) varMatrix[ia,] = lmat else {
        varMatrix[ia,lorder] = lmat
        varMatrix[lorder,ia] = lmat
      }
    }
#      print(acdf)
    t2 = proc.time()[[3]]
    if (debug.level > 0) print(paste("varMat - Finished element ",ia," out of ", ndim," in ", round(t2-t1,3),
          "seconds - totally", round(t2-t0)," seconds"))
  }
  if (equal & variogramModel$model != "Gho") {
    vDiag = diag(varMatrix)
    for (ia in 1:(ndim-1)) {
      for (ib in (ia+1):ndim) {
        varMatrix[ia,ib] = varMatrix[ia,ib] - 0.5*(vDiag[ia] + vDiag[ib])
        varMatrix[ib,ia] = varMatrix[ia,ib]
      }
    }
  } else if (!missing(sub1) & !missing(sub2)) {  
    for (ia in 1:ndim) {
      for (ib in 1:mdim) varMatrix[ia,ib] = varMatrix[ia,ib] - 0.5*(sub1[ia] + sub2[ib])
    }
  }     
  varMatrix
}
  
