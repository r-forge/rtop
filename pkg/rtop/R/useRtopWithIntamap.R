useRtopWithIntamap <- function() {
  if (suppressMessages(suppressWarnings(require("intamap")))) {
    packageStartupMessage("Loading optional package: intamap \n")
    info = matrix(c("estimateParameters","spatialPredict","methodParameters",
             rep("rtop",3),rep(NA,3)),ncol = 3)
#    info[3,2] = "rtopVariogramModel"
    registerS3methods(info,package = "intamap",env = environment(rtopVariogram))
  } else {
    warning("intamap has not been installed, please call install.packages(\"intamap\") before calling useRtopWithIntamap() again")
  }
}

