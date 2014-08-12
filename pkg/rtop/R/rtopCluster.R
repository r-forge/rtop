rtopCluster = function(nclus, ..., action = "start", type) {
  cl = getOption("rtopCluster")
  if (length(cl) > 0 && action != "start") {
    stopCluster(cl)
    options(rtopCluster = NULL)
    if (action == "stop") cl = NULL
  } else if (length(cl) > 0 && action == "start") {
    if (length(list(...)) > 0) clusterEvalQ(cl, ...)
  } else if (action != "stop") {
    require(doParallel)
    if (missing(type) || is.null(type)) {
      cl <- makeCluster(nclus) 
    } else {
      cl <- makeCluster(nclus, type)
    }
    registerDoParallel(cl, nclus)
    if (length(list(...)) > 0) clusterEvalQ(cl, ...)
    options(rtopCluster = cl)    
  }
  cl
}

