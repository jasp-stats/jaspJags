#
# Copyright (C) 2013-2022 University of Amsterdam
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

# This is a generated file. Don't change it

JAGS <- function(
          data = NULL,
          version = "0.19",
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
          customInference = list(list(ciLevel = 0.95, dataSplit = "", ess = TRUE, hdiLevel = 0.95, inferenceCi = FALSE, inferenceCiLevel = 0.95, inferenceCustomHigh = "1", inferenceCustomLow = "0", inferenceData = "", inferenceHdi = FALSE, inferenceHdiLevel = 0.95, inferenceManual = FALSE, mean = TRUE, median = TRUE, name = "Plot 1", overlayGeomType = "density", overlayHistogramBinWidthType = "sturges", overlayHistogramManualNumberOfBins = 30, parameter = NULL, parameterOrder = "orderMean", parameterSubset = "", plotCustomHigh = "1", plotCustomLow = "0", plotInterval = "hdi", plotsType = "", rhat = TRUE, savageDickey = FALSE, savageDickeyPoint = "0", savageDickeyPosteriorMethod = "samplingPosteriorPoint", savageDickeyPosteriorSamplingType = "normalKernel", savageDickeyPriorHeight = "0", savageDickeyPriorMethod = "sampling", savageDickeySamplingType = "normalKernel", sd = TRUE, shadeIntervalInPlot = FALSE)),
          densityPlot = FALSE,
          deviance = FALSE,
          exportSamplesFile = "",
          histogramPlot = FALSE,
          initialValues = list(list(levels = list(), name = "Parameter", values = list()), list(levels = list(), name = "R Code", values = list())),
          legend = TRUE,
          model = list(columns = list(), model = "model{

}", modelOriginal = "model{

}", parameters = list()),
          monitoredParameters = list(),
          monitoredParametersShown = list(),
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

   optionsWithFormula <- c("colorScheme", "customInference", "initialValues", "model", "monitoredParameters", "monitoredParametersShown", "userData")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspJags::JAGS", data, options, version))
}