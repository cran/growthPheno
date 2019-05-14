\newcommand{\packageVer}{\Sexpr[results=rd,stage=build]{utils::packageDescription("#1", fields="Version")}}
\newcommand{\packageDate}{\Sexpr[results=rd,stage=build]{utils::packageDescription("#1", fields="Date")}}
\name{growthPheno-pkg}
\alias{growthPheno-pkg}
\alias{growthPheno}
\docType{package}
\title{
\packageTitle{growthPheno}
}
\description{\packageDescription{growthPheno}

\bold{Version:} {\packageVer{growthPheno}}

\bold{Date:} \packageDate{growthPheno}
}


\section{Index}{
  For an overview of the use of these functions and an example see below.
  \tabular{ll}{
    (i) Data\cr\cr
    
    \code{\link{exampleData}} 
    \tab A small data set to use in function examples. \cr
    \code{\link{RiceRaw.dat}} 
    \tab Data for an experiment to investigate a rice \cr
    \tab germplasm panel.\cr
    \code{\link{tomato.dat}} 
    \tab Longitudinal data for an experiment to investigate \cr
    \tab tomato response to mycorrhizal fungi and zinc. \cr
    \cr
    (ii) Data frame manipulation\cr\cr
    
    \code{\link{designFactors}} 
    \tab Adds the factors and covariates for a blocked,\cr
    \tab split-plot design.\cr
    \code{\link{getTimesSubset}} 
    \tab Forms a subset of 'responses' in 'data' that\cr
    \tab contains their values for the nominated times.\cr
    \code{\link{importExcel}} 
    \tab Imports an Excel imaging file and allows some\cr
    \tab renaming of variables.\cr
    \code{\link{longitudinalPrime}} 
    \tab Selects a set variables to be retained in a\cr
    \tab data frame of longitudinal data.\cr
    \code{\link{twoLevelOpcreate}} 
    \tab Creates a data.frame formed by applying, for\cr
    \tab each response, abinary operation to the values of \cr
    \tab two different treatments.\cr
    \cr
    (iii) Plots\cr\cr
    
    \code{\link{plotAnom}} 
    \tab Identifies anomalous individuals and produces\cr
    \tab longitudinal plots without them and with just them.\cr
    \code{\link{plotCorrmatrix}} 
    \tab Calculates and plots correlation matrices for a\cr
    \tab set of responses.\cr
    \code{\link{plotDeviationsBoxes}}
    \tab Produces boxplots of the deviations of the observed \cr 
    \tab values from the smoothed values over values of x. \cr
    \code{\link{plotImagetimes}} 
    \tab Plots the time within an interval versus the interval.\cr 
    \tab For example, the hour of the day carts are imaged\cr
    \tab against the days after planting (or some other\cr
    \tab number of days after an event).\cr
    \code{\link{plotLongitudinal}} 
    \tab Plots longitudinal data for a set of indiividuals \cr
    \code{\link{plotMedianDeviations}} 
    \tab Calculates and plots the median of the deviations \cr 
    \tab of the smoothed values from the observed values. \cr
    \code{\link{probeSmoothing}} 
    \tab Compares, for a set of specified values of df and \cr
    \tab different smoothing methods, a response and the smooths \cr
    \tab of it, possibly along with growth rates calculated \cr
    \tab from the smooths.\cr
    \cr
    (iii) Smoothing\cr\cr

    \code{\link{fitSpline}} 
    \tab Produce the fits from a natural cubic smoothing\cr
    \tab spline applied to a response in a 'data.frame', \cr
    \tab and growth rates can be computed using derivatives.\cr
    \code{\link{splitSplines}} 
    \tab Adds the fits, and optionally growth rates computed \cr
    \tab from derivatives, after fitting natural cubic\cr
    \tab smoothing splines to subsets of a response to a\cr
    \tab 'data.frame'.\cr
    \cr
    (iv) Growth rate and WUI calculation\cr\cr
    
    \code{\link{fitSpline}} 
    \tab Produce the fits from a natural cubic smoothing\cr
    \tab spline applied to a response in a 'data.frame', \cr
    \tab and growth rates can be computed using derivatives.\cr
    \code{\link{GrowthRates}} 
    \tab Calculates growth rates (AGR, PGR, RGRdiff) \cr
    \tab between pairs of values in a vector.\cr
    \code{\link{intervalGRaverage}} 
    \tab Calculates the growth rates for a specified\cr
    \tab time interval by taking weighted averages of\cr
    \tab growth rates for times within the interval.\cr
    \code{\link{intervalGRdiff}} 
    \tab Calculates the growth rates for a specified\cr
    \tab time interval.\cr
    \code{\link{splitContGRdiff}} 
    \tab Adds the growth rates calculated continuously\cr
    \tab over time for subsets of a response to a\cr
    \tab 'data.frame'.\cr
    \code{\link{splitSplines}} 
    \tab Adds the fits, and optionally growth rates computed \cr
    \tab from derivatives, after fitting natural cubic\cr
    \tab smoothing splines to subsets of a response to a\cr
    \tab 'data.frame'.\cr
    \code{\link{WUI}} 
    \tab Calculates the Water Use Index (WUI).\cr
    \code{\link{intervalWUI}} 
    \tab Calculates water use indices (WUI) over a\cr
    \tab specified time interval to a data.frame.\cr
    \cr
    (v) General calculations \cr\cr
    
    \code{\link{anom}} 
    \tab Tests if any values in a vector are anomalous\cr
    \tab in being outside specified limits.\cr
    \code{\link{calcLagged}} 
    \tab Replaces the values in a vector with the result\cr
    \tab of applying an operation to it and a lagged value.\cr
    \code{\link{calcTimes}} 
    \tab Calculates for a set of times, the time intervals \cr
    \tab after an origin time and the position of each \cr 
    \tab within a time interval\cr
    \code{\link{cumulate}} 
    \tab Calculates the cumulative sum, ignoring the\cr
    \tab first element if exclude.1st is TRUE.\cr
    \code{\link{intervalValueCalculate}}
    \tab Calculates a single value that is a function of\cr
    \tab an individual's values for a response over a\cr
    \tab specified time interval.\cr
    \code{\link{splitValueCalculate}}   
    \tab Calculates a single value that is a function of\cr
    \tab an individual's values for a response.\cr
    \cr
    (vi) Principal variates analysis (PV A)\cr\cr
    
    \code{\link{intervalPVA}} 
    \tab Selects a subset of variables observed within a\cr
    \tab specified time interval using PVA.\cr
    \code{\link{PVA}} 
    \tab Selects a subset of variables using PVA.\cr
    \code{\link{rcontrib}} 
    \tab Computes a measure of how correlated each\cr
    \tab variable in a set is with the other variable,\cr
    \tab conditional on a nominated subset of them.\cr
  }}
\author{
\packageAuthor{growthPheno}

Maintainer: \packageMaintainer{growthPheno}
}
\keyword{ hplot }
\keyword{ manip }
\keyword{ package }
\seealso{\code{\link[dae:dae-package]{dae}}
}
\section{Overview}{
This package can be used to analyse growth data using splines to follow the time trend and then to extract traits for further analysis. There are tools that aid in choosing the degree of smoothing and the selection of traits. There are also functions for importing and orgainizing the data that are generally applicable, although they do have defaults that  make them particularly adapted to data from a high-throughput phenotyping facility based on a Lemna-Tec Scananalyzer 3D system. 

Data suitable for use with this package consists of columns of data obtained from a set of units (pots, carts or plots) over time. There should be a unique identifier for each unit, which by default is \code{Snapshot.ID.Tag}, and variable giving the Days after Planting for each measurement, by default {\code{Time.after.Planting..d.}}. In some cases, it is expected that there will be a column labelled \code{Snapshot.Time.Stamp}, which reflects the imaging time from which a particular data value was obtained. For imaging data, the carts/pots may be arranged in a grid of Lanes \eqn{\times}{x} Positions. 

Al-Tamimi et al. (2016) describe a seven-step process to produce phenotypic traits from imaging measurements, a subset of which may be applicable to any longitudinal data set. Some minor modifications of this process has resulted in the following eight-step process:

\enumerate{
 \item \strong{Import the data:} Use \code{\link{importExcel}} to import the raw data from the Excel file (xlsx or csv). This step should also involve any editing of the data needed to take account of mishaps during the data collection and the need to remove faulty data (produces \code{raw.dat}). Generally, data can be removed by replacing only values for responses with  missing values (\code{NA}) for carts whose data is to be removed, leaving the identifying information intact.
 \item \strong{Organize the data:} Use \code{\link{longitudinalPrime}} to select a subset of the imaging variables produced by the Lemna Tec Scanalyzer and, if the design is a blocked, split-plot design, use \code{\link{designFactors}} to add covariates and factors that might be used in the analysis (produces the data frame \code{longi.prime.dat}).
 \item \strong{Form derived longitudinal traits} Add derived responses that result in a value for each observation: use \code{\link{splitContGRdiff}} to obtain continuous growth rates i.e. a growth rate for each time of observation, except the first; \code{\link{WUI}} to produce continuous Water Use Indices (WUI) and \code{cumulate} to produce cumulative responses. (Produces the data frame \code{longi.dat}.)
 \item \strong{Exploratory analysis:} Use \code{\link{splitSplines}} to fit splines, using say direct smoothing and an arbitrary value of 5 for  the \code{df}, to smooth the longitudinal trends in the primary traits and calculate continuous growth rates from the smoothed response (added to the data frame \code{longi.dat}). There are two options for calculating continuous smoothed growth rates: (i) by differencing --- use  \code{\link{splitContGRdiff}}; (ii) from the first derivatives of the splines --- in \code{\link{splitSplines}} include \code{1} in the \code{deriv} argument, include \code{"AGR"} in \code{suffices.deriv} and set the \code{RGR} to say \code{"RGR"}.Produce plots of the unsmoothed and smoothed longitudinal data using \code{\link{plotLongitudinal}}.
 \item \strong{Choose the \code{smoothing.method} and \code{df}:} Use \code{\link{probeSmoothing}} to compare the smooths for a number of values of \code{df} and for different \code{smoothing.methods}. If necessary, re-run \code{\link{splitSplines}} with a revised value of \code{df}, replacing replace the initial smoothed primary traits and derived tratis in \code{longi.dat};  replot using \code{\link{plotLongitudinal}}. 
  \item \strong{Clean the data:} Check for anomalies in the data, perhaps employing \code{\link{plotAnom}}. Decide if excluding data is justified. To exclude data, consider whether it is better, especially for the analysis stage, if only the values of the responses are removed by setting them to missing (\code{NA}), rather than removing complete rows of the data.  
 \item \strong{Extract per-cart traits:} These are traits for which there is a single value for each cart, pot or plot (by default, identified by the \code{Snapshot.ID.Tag}). (produces the data frame \code{cart.dat})
 \enumerate{
   \item Initialize a cart data.frame with the factors and covariates for a single observation from all carts. This can be done by subsetting \code{longi.dat} so that there is one entry for each cart.
   \item Use \code{\link{getTimesSubset}} to add traits at specific times to the cart \code{data.frame}, often the first and last day of imaging for each \code{Snapshot.ID.Tag}. The times need to be selected so that there is one and only one observation for each cart. Also form traits, such as growth rates over the whole imaging period, based on these values
   \item Based on the longitudinal plots, decide on the intervals for  which growth rates and WUIs are to be calculated. The growth rates for intervals are calculated from the continuous growth rates, using \code{\link{intervalGRdiff}}, if the continuous growth rates were calculated by differencing, or \code{\link{intervalGRaverage}}, if they were calculated from first derivatives. To calculate WUIs for intervals, use \code{\link{intervalWUI}}, The interval growth rates and WUIs are added to the cart \code{data.frame}.
 }
   \item (Optional) There is also the possibility that, for experiments investigating salinity, the Shoot Ion Independent Tolerance (SIIT) index can be calculated using \code{\link{twoLevelOpcreate}}. 
}

The vignette Rice illustrates this process. Use `vignette("Rice", package = "growthPheno") to access the vignette.

}

\references{
Al-Tamimi, N, Brien, C.J., Oakey, H., Berger, B., Saade, S., Ho, Y. S., Schmockel, S. M., Tester, M. and Negrao, S. (2016) New salinity tolerance loci revealed in rice using high-throughput non-invasive phenotyping. \emph{Nature Communications}, \bold{7}, 13342.
}
