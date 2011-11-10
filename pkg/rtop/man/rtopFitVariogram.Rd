\name{rtopFitVariogram}
\alias{rtopFitVariogram}
\alias{rtopFitVariogram.rtop}
\alias{rtopFitVariogram.SpatialPolygonsDataFrame}
\alias{rtopFitVariogram.SpatialLinesDataFrame}
\alias{rtopFitVariogram.SpatialPointsDataFrame}
\alias{rtopFitVariogram.rtopVariogram}
\alias{rtopFitVariogram.rtopVariogramCloud}
\title{Fit variogram model to sample variogram of data with spatial or line support}
\description{
rtopFitVariogram will fit a variogram model to the estimated binned variogram or cloud variogram of data
with an areal or a line support.
}
\usage{
\method{rtopFitVariogram}{rtop}(object, ...) 
\method{rtopFitVariogram}{SpatialPolygonsDataFrame}(object, params=list(), ...) 
\method{rtopFitVariogram}{SpatialLinesDataFrame}(object, params=list(), ...) 
\method{rtopFitVariogram}{SpatialPointsDataFrame}(object, params=list(), ...) 
\method{rtopFitVariogram}{rtopVariogram}(object, observations, dists = NULL, params=list(), mr = FALSE, ...) 
\method{rtopFitVariogram}{rtopVariogramCloud}(object, observations, dists = NULL, params=list(), mr = FALSE, ...) 
}
\synopsis{ rtopFitVariogram(object, ...) }

\arguments{
\item{object}{object of class \code{rtopVariogram} or \code{rtopVariogramCloud},
or an object with class \code{rtop} that includes the sample variograms.

The object can also be of class \code{\link[sp]{SpatialPolygonsDataFrame}},
\code{\link[sp]{SpatialLinesDataFrame}}, or \code{\link[sp]{SpatialPointsDataFrame}}
with observations. If \code{object} is a \code{\link[sp]{SpatialPointsDataFrame}},
it must have a column with name \code{area} (for areal observations) or
\code{length} (for line observations).
}
\item{observations}{the observations, passed as a Spatial*DataFrame object, if
   object is an \code{rtopVariogram} or \code{rtopVariogramCloud}}
\item{params}{a set of parameters, used to modify the standard parameters for 
      the \code{rtop} package, set in \code{\link{getRtopParams}}. }
\item{dists}{either a matrix with geostatistical distances (created by a call
             to the function \code{\link{gDist}} or a list with the
             elements (areas or lines) discretized (from a call to \code{\link{rtopDisc}}.}
\item{mr}{logical; defining whether the function should return a list with 
          discretized elements and geostatistical distances, even if it was not
          called with an rtop-object as argument.}
\item{...}{Other parameters to functions called from \code{rtopFitVarigoram}}
} 

\value{ 
The function creates an object with the fitted variogram Model (\code{variogramModel}) 
and a \code{\link{data.frame}} (\code{varFit}) with the 
differences between the sample semivariances and the regularized semivariances.
If \code{mr} = TRUE, the function also returns other objects (discretized elements
and geostatistical distances, if created) as a part of the returned object.
If the function is called with an rtop-object as argument, it will return an
rtop-object with \code{variogramModel} and \code{varFit} added to the object,
in addition to other objects created.
}

\note{
There are several options for fitting of the variogramModel, where the parameters
can be set in \code{params}, which is a list of parameters for modification
of the standard parameters of the rtop-package given in a call to 
\code{\link{getRtopParams}}. The first choice is between individual fitting and binned
fitting. This is based on the type of variogram submitted, individual fitting is done
if a cloud variogram (of class \code{rtopVariogramCloud}) is passed as argument, 
and binned fitting if the submitted variogram is of class \code{rtopVariogram}.
If the function is called with an object of class \code{rtop}, having both
\code{variogram} and \code{variogramCloud} among its arguments, the variogram
model is fitted to the variogram which is consistent with the parameter \code{cloud}.
}



\references{ 

\url{http://www.intamap.org/}

}
\author{ Jon Olav Skoien }
\seealso{\code{\link{rtop-package}}
}
\examples{
\dontrun{
library(rgdal)
rpath = system.file("extdata",package="rtop")
setwd(rpath)
observations = readOGR(".","observations")
predictionLocations = readOGR(".","predictionLocations")

# Setting some parameters 
params = list(gDist = TRUE, cloud = TRUE)
# Build an object
rtopObj = createRtopObject(observations,predictionLocations, params = params)
# Fit a variogram (function also creates it)
rtopObj = rtopFitVariogram(rtopObj)
rtopObj$variogramModel
}
}
\keyword{spatial}
