tskrige = function(object, predictionLocations, varMatObs, varMatPredObs,
                   varMat, params = list(), formulaString, sel, wret = TRUE, ...) {
  
  params = getRtopParams(params, ...)
  cv = params$cv
  debug.level = params$debug.level
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
  indx = predictionLocations@index
  sinds = sort(unique(indx[,1]))
  tinds = sort(unique(indx[,2]))
  tms = rep(0, nspace)
  for (i in 1:nspace) tms[i] = dim(object[i,])[1]
  if (sum(tms == ntime) == nspace) { # if all stations have obs from all time steps
    obj1 = object[,1]
    if (is(predictionLocations, "STSDF")) {
      predLoc = predictionLocations@sp
    } else {
      predLoc = predictionLocations
    }
    ret = rtop:::rtopKrige.default(obj1, predLoc, varMatObs,
                            varMatPredObs, varMat, params, formulaString, wret = TRUE)#,
    #sel, ...)
    weight = ret$weight
    wvar = ret$predictions$var1.var
    obs = as.data.frame(object)
    obs = obs[,c("sp.ID", "timeIndex", as.character(formulaString[[2]]))]
    obs$timeIndex = object@index[,2]
    obs = reshape(obs, v.names = as.character(formulaString[[2]]),
                  idvar = "sp.ID", timevar = "timeIndex", direction = "wide")
    predictionLocations@data = cbind(predictionLocations@data, data.frame(var1.pred = NA, var1.var = NA))
    if (interactive() & debug.level == 1 & length(sinds) > 1) pb = txtProgressBar(1, length(sinds), style = 3)
    for (istat in 1:length(sinds)) {
      isind = sinds[istat]
      ttinds = which(indx[,1] == isind)
      lweight = weight[istat,]
      preds = lweight %*% as.matrix(obs[,2:dim(obs)[2]])
      diffs = sweep(obs[,2:dim(obs)[2]], 2, preds )
      var1.yam = t(lweight) %*% (diffs^2)
#      var1.var = t(lweights) %*% ((as.matrix(Obs[iobs,depVar]@data)-as.numeric(var1.pred))^2)
      
      predictionLocations@data$var1.pred[ttinds] = wrun
      predictionLocations@data$var1.var[ttinds] = wvar[istat]
      predictionLocations@data$var1.yam[ttinds] = var1.yam
      if (interactive() & debug.level == 1 & length(sinds) > 1) setTxtProgressBar(pb, istat)
    }
    if (interactive() & debug.level == 1 & length(sinds) >  1) close(pb)
  } else { 
    if (cv) {
      object@sp$sindex = sindex = 1:nspace
      object@time$tindex = tindex = 1:ntime

      spobs = NULL
      depVar = as.character(formulaString[[2]])
      predictionLocations@data = cbind(predictionLocations@data, data.frame(var1.pred = NA, var1.var = NA, var1.yam = NA))
      if (interactive() & debug.level == 1 & ntime >  1) pb = txtProgressBar(1, ntime, style = 3)
      
      for (itime in 1:ntime) {
        ppq = object[,itime]
        if (is.null(spobs) || !isTRUE(all.equal(ppq$sindex, spobs))) {
          spobs = ppq$sindex
          vmat = varMatObs[spobs, spobs]
          ret = rtop:::rtopKrige.default(object = ppq, varMatObs = vmat,
                            params = params, formulaString = formulaString, wret = TRUE, debug.level = 0)#,
          weight = ret$weight
          wvar = ret$predictions$var1.var
        }
        obs = ppq@data[[depVar]]
        preds = weight %*% obs
        diffs = obs - preds
        var1.yam = t(weight) %*% (diffs^2)
        
        itind = tinds[itime]
        ttinds = which(indx[,2] == itind)
        predictionLocations@data$var1.pred[ttinds] = preds
        predictionLocations@data$var1.var[ttinds] = wvar
        predictionLocations@data$var1.yam[ttinds] = var1.yam
        if (interactive() & debug.level == 1 & ntime > 1) setTxtProgressBar(pb, itime)
      }
      if (interactive() & debug.level == 1 & ntime >  1) close(pb)
    }
  }
  predictionLocations
}

