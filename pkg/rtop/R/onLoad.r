.onLoad <- function(libname, pkgname) {
  if (suppressMessages(suppressWarnings(require(intamap)))) {
    packageStartupMessage("Loading optional package: intamap \n")
    info = matrix(c("estimateParameters","spatialPredict","methodParameters",
             rep("rtop",3),rep(NA,3)),ncol = 3)
    registerS3methods(info,package = "intamap",env = environment(rtopVariogram))
  }
}

