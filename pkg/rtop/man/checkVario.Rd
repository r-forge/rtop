\name{checkVario}
\alias{checkVario}
\alias{checkVario.rtop}
\alias{checkVario.rtopVariogramModel}
\title{ Plot variogram fitted to data with support }
\description{
The function will create diagnostic plots for analysis of the variograms fitted
to sample variograms of data with support
}
\synopsis{ checkVario(object, ...) }
\usage{
\method{checkVario}{rtop}(object, acor = 1, ...) 
\method{checkVario}{rtopVariogramModel}(variogramModel = NULL, 
           sampleVariogram = NULL, observations = NULL, areas = NULL, dists = NULL, acomp = NULL,
           log = "xy", params = list(), compVars = list(), acor = 1,
           legx = NULL, legy = NULL, plotNugg = TRUE, ...) 
}


\arguments{
\item{object}{ object of class \code{rtop} (see \code{\link{rtop-package}}) }
\item{variogramModel}{variogram model of type rtopVariogram}
\item{sampleVariogram}{a sample variogram of the data}
\item{observations}{a set of observations}
\item{areas}{either an array of areas that should be used as examples, or 
        the number of areas per order of magnitude (similar to the parameter \code{amul}
        of the standard parameters, see \code{\link{getRtopParams}}. amul from \code{rtopObj}
        or from the standard parameter set will be used if not defined here.}
\item{dists}{either an array of distances that should be used as examples, or
        the number of distances per order of magnitude(similar to the parameter \code{amul}
        of the standard parameters, see \code{\link{getRtopParams}}. amul from \code{rtopObj}
        or from the standard parameter set will be used if not defined here.}
\item{acomp}{either a matrix with the area bins that should be visualized, or a number 
        giving the number of pairs to show. If a sample variogram is given, the \code{acomp}
        pairs with highest number of pairs will be used}
\item{log}{text variable for log-plots, default to log-log \code{"xy"}, can otherwise be set
        to \code{"x"}, \code{"y"} or \code{""}}
\item{params}{list of parameters to modify the standard parameters of rtopObj or 
        the default parameters found from \code{\link{getRtopParams}}}
\item{compVars}{a list of variograms of \code{gstat}-type for comparison, see 
        \code{\link[gstat:vgm]{vgm}}. The names of the variograms in the list will
        be used in the key.}
\item{acor}{unit correction factor in the key, e.g. to see numbers more easily 
        interpretable for large areas. As an example, ucor = 0.000001 when area
        is given in square meters and should rather be shown as square kilometers.
        Note that this parameter also changes the value of the nugget to the new unit.}
\item{legx}{x-coordinate of the legend for fine-tuning of position, see x-argument of 
\code{\link[graphics]{legend}}}
\item{legy}{y-coordinate of the legend for fine-tuning of position, see y-argument of
\code{\link[graphics]{legend}}}
\item(plotNugg){logical; whether the nugget effect should be added to the plot or not}
\item{...}{arguments to lower level functions}

} 

\value{ 
The function gives diagnostic plots for the fitted variograms, where the
regularized variograms are shown together with the sample variograms and 
possibly also user defined variograms. In addition, if an rtopObject
is submitted, the function will also give plots of the relationship between
variance and area size and a scatter plot of the fit of the observed and
regularized variogram values. The sizes of the dots are relative to the number
of pairs in each group.
}



\author{ Jon Olav Skoien }
\seealso{\code{\link{rtop-package}}}
\examples{
\dontrun{
library(rgdal)
rpath = system.file("extdata",package="rtop")
setwd(rpath)
observations = readOGR(".","observations")
predictionLocations = readOGR(".","predictionLocations")
params = list(cloud = TRUE, gDist = TRUE)
rtopObj = createRtopObject(observations,predictionLocations, params = params)
 # Fit a variogram (function also creates it)
rtopObj = rtopFitVariogram(rtopObj)
checkVario(rtopObj, compVar = list(first = vgm(5e-6, "Sph", 30000,5e-8), second = vgm(2e-6, "Sph", 30000,5e-8)))
rtopObj = checkVario(rtopObj, acor = 0.000001, acomp = data.frame(acl1 = c(2,2,2,2,3,3,3,4,4), acl2 = c(2,3,4,5,3,4,5,4,5)))
rtopObj = checkVario(rtopObj, cloud = TRUE, identify = TRUE, acor = 0.000001)
}
}
\keyword{spatial}