#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' JAGS
#'
#' JAGS -- Just Another Gibbs Sampler -- is software for general purpose Bayesian inference. One can specify a model using JAGS syntax and let JAGS draw samples from the posterior distribution.
#'
#' @param actualExporter, When on, the MCMC samples are written to the export file, and written again each time the analysis is rerun. Toggled with the 'Sync Samples' button.
#'    Defaults to \code{FALSE}.
#' @param aggregatedChains, If checked, the samples of different chains are aggregated in density plots and histograms. If unchecked, there are separate colors per chain.
#'    Defaults to \code{TRUE}.
#' @param autoCorPlot, Plot the autocorrelation of the posterior samples for each parameter selected under 'Show results for these parameters'.
#'    Defaults to \code{FALSE}.
#' @param autoCorPlotLags, The maximum number of lags to show in the autocorrelation plot.
#' @param autoCorPlotType, Whether to display the autocorrelation as a bar at each lag, or as a line that connects subsequent lags.
#' \itemize{
#'   \item \code{"lines"}
#'   \item \code{"bars"}
#' }
#' @param bivariateScatterDiagonalType, Show a density plot or a histogram on the diagonal entries of the scatter plot.
#' \itemize{
#'   \item \code{"density"}
#'   \item \code{"histogram"}
#' }
#' @param bivariateScatterOffDiagonalType, Show a hexagonal bivariate density plot, or a contour plot on the off-diagonal entries of the scatter plot.
#' \itemize{
#'   \item \code{"hexagon"}
#'   \item \code{"contour"}
#' }
#' @param bivariateScatterPlot, Show a matrix plot of all pairs of parameters. Only shows output when more than 1 parameter is sampled.
#'    Defaults to \code{FALSE}.
#' @param burnin, The number of samples to draw from the posterior distribution and immediately discard.
#' @param chains, The number of MCMC chains to run.
#' @param colorScheme, Determines the color scheme of the plots.
#' @param customInference, Each tab specifies a set of custom results (a plot and a table) for one parameter. Up to 10 tabs can be added.
#' @param densityPlot, Show the marginal density of the posterior samples for each parameter selected under 'Show results for these parameters'.
#'    Defaults to \code{FALSE}.
#' @param deviance, Show the Deviance statistic.
#'    Defaults to \code{FALSE}.
#' @param exportSamplesFile, The CSV file to save the MCMC samples to. The samples are only written when 'Sync Samples' is on.
#' @param histogramPlot, Show the marginal histogram of the posterior samples for each parameter selected under 'Show results for these parameters'.
#'    Defaults to \code{FALSE}.
#' @param initialValues, Each row has a 'Parameter' of the model and an 'R Code', its initial value.
#' @param legend, Show a legend in the plots.
#'    Defaults to \code{TRUE}.
#' @param model, Enter the desired model. Columns in the data can be directly referred to. If these contain spaces, then the reference must also contain spaces.
#' @param monitoredParameters, The parameters for which the MCMC samples are stored. Only available when 'Show results for' is set to 'selected parameters'.
#' @param monitoredParametersShown, Determines which parameters are shown in tables and plots.
#' @param resultsFor, By default, 'all monitored parameters' is selected which implies that JASP stores the MCMC samples for all parameters in the model. However, for large JAGS models storing all MCMC samples may take too much memory. By selecting 'selected parameters', one can first decide for which parameters the MCMC samples should be stored, and in a next box, decide which of these parameters should be shown in the results.
#' \itemize{
#'   \item \code{"allParameters"}
#'   \item \code{"selectedParameters"}
#' }
#' @param samples, The number of samples to draw from the posterior distribution that are used for results (tables, plots).
#' @param thinning, Every nth value of 'No. samples' is kept for the results, where n is given by 'Thinning'.
#' @param tracePlot, Show a trace plot of the posterior samples for each parameter selected under 'Show results for these parameters'.
#'    Defaults to \code{FALSE}.
#' @param userData, Each row has a 'Parameter', the name to be used in the JAGS model code, and an 'R Code', the value for the data. This value can also be R code.
JAGS <- function(
          data = NULL,
          version = "1",
          actualExporter = FALSE,
          aggregatedChains = TRUE,
          autoCorPlot = FALSE,
          autoCorPlotLags = 20,
          autoCorPlotType = "lines",
          bivariateScatterDiagonalType = "density",
          bivariateScatterOffDiagonalType = "hexagon",
          bivariateScatterPlot = FALSE,
          burnin = 500,
          chains = 3,
          colorScheme = "colorblind",
          customInference = list(list(ciLevel = 0.95, dataSplit = list(types = "unknown", value = ""), ess = TRUE, hdiLevel = 0.95, inferenceCi = FALSE, inferenceCiLevel = 0.95, inferenceCustomHigh = 1, inferenceCustomLow = 0, inferenceData = list(types = "unknown", value = ""), inferenceHdi = FALSE, inferenceHdiLevel = 0.95, inferenceManual = FALSE, mean = TRUE, median = TRUE, mode = TRUE, name = "Plot 1", overlayGeomType = "density", overlayHistogramBinWidthType = "sturges", overlayHistogramManualNumberOfBins = 30, parameter = NULL, parameterOrder = "orderMean", parameterSubset = "", plotCustomHigh = 1, plotCustomLow = 0, plotInterval = "ci", plotsType = "", rhat = TRUE, savageDickey = FALSE, savageDickeyPoint = 0, savageDickeyPosteriorMethod = "samplingPosteriorPoint", savageDickeyPosteriorSamplingType = "normalKernel", savageDickeyPriorHeight = 0, savageDickeyPriorMethod = "sampling", savageDickeySamplingType = "normalKernel", sd = TRUE, shadeIntervalInPlot = FALSE)),
          densityPlot = FALSE,
          deviance = FALSE,
          exportSamplesFile = "",
          histogramPlot = FALSE,
          initialValues = list(list(levels = list(), name = "Parameter", values = list()), list(levels = list(), name = "R Code", values = list())),
          legend = TRUE,
          model = list(columns = list(), model = "model{

}", modelOriginal = "model{

}", parameters = list()),
          monitoredParameters = list(types = list(), value = list()),
          monitoredParametersShown = list(types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          resultsFor = "allParameters",
          samples = 2000,
          seed = 1,
          setSeed = FALSE,
          thinning = 1,
          tracePlot = FALSE,
          userData = list(list(levels = list(), name = "Parameter", values = list()), list(levels = list(), name = "R Code", values = list()))) {

   defaultArgCalls <- formals(jaspJags::JAGS)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("colorScheme", "customInference", "initialValues", "model", "monitoredParameters", "monitoredParametersShown", "userData")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspJags", "JAGS", "JAGS.qml", options, version, FALSE))
}