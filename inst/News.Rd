\name{news}
\title{News for Package growthPheno}
\section{growthPheno Version 1.0-15 (2019-07-11)}{
  \itemize{
   \item{Change Days to DAP in \code{tomato.dat}.}
   \item{Fix two bugs in probeSmoothing.}
}}
\section{growthPheno Version 1.0-14 (2019-06-05)}{
  \itemize{
   \item{Add Treatments column to tomato.dat.}
   \item{Modify probeSmoothing so that growth rates are caluclated and saved even when they are not plotted.}
}}
\section{growthPheno Version 1.0-13 (2019-05-12)}{
  \itemize{
    \item{Modify the Title and Description of the package, as well as the Overview of the package.}
    \item{Make a vignette using the rice data from Al-Tamimi.}
    \item{Include data from a tomato experiment.}
    \item{This version includes a major revamp of the plotting functions so that all of them begin with plot. The functions anomPlot, corrPlot, imagetimesPlot have been renamed to plotAnom, plotCorrmatrix and plotImagetimes. While the original function continue to work, they will be removed in future versions.}
    \item{The new functions plotDeviationsBoxes and plotMedianDeviations have been added. These two functions are not restricted to comparing observed with smoothed values generated using growthPheno functions.}
    \item{The new function probeSmoothing was created by extensively revising probeDF to facilitate comparison of different \code{smoothing.methods} and \code{df}, including plots of the median of deviations, the latter calculated as observed values minus smoothed values. The function probeDF has been retained for compatability purposes, but with the addition of a warning that probeDF is soft-deprecated. It will be removed in future versions.}
    \item{Replaced getDates with getTimesSubsets, deprecating getDates.}
    \item{Change the argument \code{smoothing.scale} to \code{smoothing.method}, replacing the option \code{identity} with \code{direct} in both fitSpline and splitSplines.}
}}
