\name{plotDeviationsBoxes}
\alias{plotDeviationsBoxes}
\title{Produces boxplots of the deviations of the observed values from the smoothed values over values of x.}
\description{Produces boxplots of the deviations of the observed values from the smoothed values over values of x.}
\usage{
plotDeviationsBoxes(data, observed, smoothed, x.factor, 
                    x.title = NULL, y.titles = NULL,
                    facet.x = ".", facet.y = ".", 
                    facet.labeller = NULL, 
                    facet.scales = "fixed", 
                    angle.x = 0, 
                    deviations.plots = "absolute", 
                    ggplotFuncs = NULL, printPlot = TRUE, ...)
 }
\arguments{
 \item{data}{A \code{\link{data.frame}} containing the observed and smoothed values 
             from which the deviations are to be computed.}
 \item{observed}{A \code{character} specifying the response variable for which the 
                 observed values are supplied.}
 \item{smoothed}{A \code{character} specifying the smoothed response variable, 
                 corresponding to \code{observed}, for which values are supplied.}
 \item{x.factor}{A \code{\link{character}} giving the \code{\link{factor}} to be 
                plotted on the x-axis.}
 \item{x.title}{Title for the x-axis. If \code{NULL} then set to \code{x}.}
 \item{y.titles}{A \code{\link{character}} giving the titles for the y-axis, 
             one for each plot specified \code{deviations.plots}.}
 \item{facet.x}{A \code{\link{data.frame}} giving the variable to be used to 
                form subsets to be plotted in separate columns of plots. 
                Use \code{"."} if a split into columns is not wanted. For 
                \code{which.plots} set to \code{methodcompare} or \code{dfcompare} 
                \code{facet.x.pf} is ignored.}
 \item{facet.y}{A \code{\link{data.frame}} giving the variable to be used to 
                form subsets to be plotted in separate rows of plots.
                Use \code{"."} if a split into columns is not wanted.}
 \item{facet.labeller}{A \code{ggplot} \code{function} for labelling the 
             facets of a plot produced using the \code{ggplot} function. 
             For more information see \code{ggplot}.}
 \item{facet.scales}{A \code{\link{character}} specifying whether the scales are shared 
             across all facets of a plot ("fixed"), or do they vary across rows (the 
             default, "free_x"), columns ("free_y"), or both rows and columns ("free")?}
 \item{angle.x}{A \code{\link{numeric}} between 0 and 360 that gives the angle of the 
             x-axis text to the x-axis. It can also be set by supplying, in 
             \code{ggplotFuncs}, a \code{theme} function from \code{ggplot2}.}
 \item{deviations.plots}{A \code{\link{character}} specifying whether 
              \code{absolute} and/or \code{relative} deviations are to be plotted.}
 \item{ggplotFuncs}{A \code{list}, each element of which contains the 
                results of evaluating a \code{ggplot} function. 
                It is created by calling the \code{list} function with 
                a \code{ggplot} function call for each element.
                These functions are applied in creating the \code{ggplot} 
                object for plotting.}
 \item{printPlot}{A \code{\link{logical}} indicating whether or not to print the 
                  plots.}
\item{...}{allows passing of arguments to \code{ggplot}.}
}
\value{A list whose components are named \code{absolute} and \code{relative}; a 
       component will contain an object of class "\code{ggplot}" when the plot has been 
       requested using the \code{deviations.plots} argument and a \code{NULL} otherwise. 
       The objects can be plotted using \code{print}.}
\author{Chris Brien}
\seealso{\code{\link{plotSmoothsMedianDevns}}, \code{\link{probeSmooths}}, \code{ggplot}.}
\examples{
data(exampleData)

plotDeviationsBoxes(longi.dat, observed = "PSA", smoothed = "sPSA",
                    x.factor="DAP", facet.x.pf = ".", facet.y= ".", df =5)
}
\keyword{hplot}
\keyword{manip}