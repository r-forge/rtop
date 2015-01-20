tskrige = function(object, predictionLocations, varMatObs, varMatPredObs,
            varMat, params = list(), formulaString, sel, wret = TRUE, ...) {

  params = getRtopParams(params, ...)
  cv = params$cv
  if (!missing(varMat) && missing(varMatObs)) {
    if (is.atomic(varMat)) {
      varMatObs = varMat
      if (!cv) stop(paste("Not cross-validation, you must provide either varMatObs",
                          " and varMatPredObs or varMat with the matrices as elements"))
    } else {
      varMatObs = varMat$varMatObs
      varMatPredObs = varMat$varMatPredObs
    }
  }

  if (cv) predictionLocations = object
  ntime = dim(object)[2]
  nspace = dim(object)[1]
  tms = rep(0, nspace)
  for (i in 1:nspace) tms[i] = dim(object[i,])[1]
  if (sum(tms == ntime) == nspace) {
    obj1 = object[,1]
    if (is(predictionLocations, "STSDF")) {
      predLoc = predictionLocations@sp
    } else {
      predLoc = predictionLocations
    }
    ret = rtopKrige.default(obj1, predLoc, varMatObs,
                    varMatPredObs, varMat, params, formulaString, wret = TRUE)#,
                    #sel, ...)
    weight = ret$weight
    obs = as.data.frame(object)
    obs = obs[,c("sp.ID", "timeIndex", as.character(formulaString[[2]]))]
    obs$timeIndex = object@index[,2]
    obs = reshape(obs, v.names = as.character(formulaString[[2]]),
                  idvar = "sp.ID", timevar = "timeIndex", direction = "wide")
    if (params$cv) {
      predictionLocations@data = cbind(predictionLocations@data, data.frame(var1.pred = NA, var1.var = NA))
      indx = predictionLocations@index
      sinds = unique(indx[,1])
      tinds = unique(indx[,2])
      for (istat in 1:length(sinds)) {
        isind = sinds[istat]
        ttinds = which(indx[,1] == isind)
        lweight = weight[istat,]
        wrun = lweight %*% as.matrix(obs[,2:dim(obs)[2]])
        predictionLocations@data$var1.pred[ttinds] = wrun
    }
  } else {
  
  }

  } else { # if all stations have obs from all time steps
  
  }
  predictionLocations
}

