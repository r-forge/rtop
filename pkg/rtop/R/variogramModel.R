rtopVariogramModel = function(model = "Ex1", sill = NULL, range = NULL, exp = NULL,
    nugget = NULL, angle = NULL,
    observations = NULL, formulaString = obs~1) {
if (model == "ex1") {
  if (!is.null(observations)) {
    parInit = findParInit(formulaString, observations, model)$par0
    if (is.null(sill)) sill = parInit[1]
    if (is.null(range)) range = parInit[3]
    if (is.null(exp)) exp = parInit[2]
    if (is.null(nugget)) nugget = parInit[5]
    if (is.null(angle)) angle = parInit[4]
  } else {
    if (is.null(sill)) sill = 1
    if (is.null(range)) range = 1
    if (is.null(exp)) exp = 0
    if (is.null(nugget)) nugget = 0
    if (is.null(angle)) angle = 1
  }
  variogramModel = list(model = model, params = c(sill, exp, range, angle, nugget))
  class(variogramModel) = "rtopVariogramModel"
}
}

updateRtopVariogram.rtop = function(object, ...) {
object$variogramModel = update(object$variogramModel, sampleVariogram = object$variogram,
observations = object$observations, ...)
object
}

updateRtopVariogram.rtopVariogramModel = function(object, action = "mult", ..., checkVario = FALSE, 
sampleVariogram = NULL, observations = NULL){
  variogramModel = object
  dots = list(...)

  if (variogramModel$model == "Ex1") {
    if ("sill" %in% names(dots)) {
      if (action == "mult") {
        variogramModel$params[1] = variogramModel$params[1]*dots$sill
      } else if (action == "add") {
        variogramModel$params[1] = variogramModel$params[1]*dots$sill
      } else if (action == "replace") {
        variogramModel$params[1] = dots$sill
      }
    }
    if ("range" %in% names(dots)) {
      if (action == "mult") {
        variogramModel$params[3] = variogramModel$params[3]*dots$range
      } else if (action == "add") {
        variogramModel$params[3] = variogramModel$params[3]*dots$range
      } else if (action == "replace") {
        variogramModel$params[3] = dots$range
      }
    }
    if ("exp" %in% names(dots)) {
      if (action == "mult") {
        variogramModel$params[2] = variogramModel$params[2]*dots$exp
      } else if (action == "add") {
        variogramModel$params[2] = variogramModel$params[2]*dots$exp
      } else if (action == "replace") {
        variogramModel$params[2] = dots$exp
      }
    }
    if ("angle" %in% names(dots)) {
      if (action == "mult") {
        variogramModel$params[4] = variogramModel$params[4]*dots$angle
      } else if (action == "add") {
        variogramModel$params[4] = variogramModel$params[4]*dots$angle
      } else if (action == "replace") {
        variogramModel$params[4] = dots$angle
      }
    }
     if ("nugget" %in% names(dots)) {
      if (action == "mult") {
        variogramModel$params[5] = variogramModel$params[3]*dots$nugget
      } else if (action == "add") {
        variogramModel$params[5] = variogramModel$params[3]*dots$nugget
      } else if (action == "replace") {
        variogramModel$params[5] = dots$nugget
      }
    }
    if (checkVario) checkVario(variogramModel, sampleVariogram = sampleVariogram, observations = observations)  
  }
  variogramModel    
}





plot.rtopVariogramCloud = function(x,  ...) {
variogramCloud$np = variogramCloud$ord
gstat:::plot.variogramCloud(x, ...)
}
