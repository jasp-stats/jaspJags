context("JAGS")

options <- jaspTools::analysisOptions("JAGS")
options$initialValues <- list(list(levels = "Row 1", name = "Parameter", values = "theta"),
                              list(levels = "Row 1", name = "R Code", values = "..."))
options$model <- list(columns = list(), model = "model{\n theta ~ dbeta(1, 1)\n k ~ dbinom(theta, n)\n mu ~ dnorm(0, 1)}",
                      parameters = c("theta", "k"))
options$burnin <- 1
options$chains <- 4
options$samples <- 50
options$monitoredParametersShown <- c("theta", "mu")
options$autoCorPlot <- TRUE
options$densityPlot <- TRUE
options$histogramPlot <- TRUE
options$tracePlot <- TRUE
options$bivariateScatterPlot <- TRUE
options$userData <- list(list(
  levels = c("Row 0", "Row 1"),
  name = "Parameter", values = c("k", "n")
), list(
  levels = c("Row 0", "Row 1"),
  name = "R Code",
  values = c("70", "100")
))
options$parameters <- c("\"theta\"", "\"mu\"")


options(JASP_JAGS_UNITTEST_DATAFILE = "jags-test-model.rds")

set.seed(1)
results <- jaspTools::runAnalysis("JAGS", "debug.csv", options)

test_that("MCMC summary table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(-2.13189859421549, 0.0400235500930204, 1.75316153519098, 0.0150312557449907,
                           1.04100463312435, 162, "mu", 1.28053408855522,
                           1.08774224099908, 0.614492436789889, 0.69902486500684, 0.777692716928423,
                           0.696777556636238, 0.0431041676553818, 232, "theta",
                           1.25276545060289, 1.07739568349341))
})

test_that("autoCorPlot mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_autoCorPlot"]][["collection"]][["mainContainer_plotContainer_autoCorPlot_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotAutoCor-mu")
})

test_that("autoCorPlot theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_autoCorPlot"]][["collection"]][["mainContainer_plotContainer_autoCorPlot_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotAutoCor-theta")
})

test_that("densityPlot mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_densityPlot"]][["collection"]][["mainContainer_plotContainer_densityPlot_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotDensity-mu")
})

test_that("densityPlot theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_densityPlot"]][["collection"]][["mainContainer_plotContainer_densityPlot_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotDensity-theta")
})

test_that("histogramPlot mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_histogramPlot"]][["collection"]][["mainContainer_plotContainer_histogramPlot_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotHistogram-mu")
})

test_that("histogramPlot theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_histogramPlot"]][["collection"]][["mainContainer_plotContainer_histogramPlot_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotHistogram-theta")
})

test_that("tracePlot mu plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_tracePlot"]][["collection"]][["mainContainer_plotContainer_tracePlot_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotTrace-mu")
})

test_that("tracePlot theta plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_tracePlot"]][["collection"]][["mainContainer_plotContainer_tracePlot_theta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "plotTrace-theta")
})

test_that("Bivariate Scatter Plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_bivariateScatterPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bivariate-scatter-plot")
})


test_that("Model errors are captured gracefully", {

  options <- structure(
    list(
      .meta = list(initialValues = list(values = list(isRCode = TRUE))),
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
      customInference = list(
        list(
          ciLevel = 0.95,
          dataSplit = list(types = "unknown", value = ""),
          ess = TRUE,
          hdiLevel = 0.95,
          inferenceCi = FALSE,
          inferenceCiLevel = 0.95,
          inferenceCustomHigh = 10,
          inferenceCustomLow = 1.33,
          inferenceData = list(types = "unknown", value = ""),
          inferenceHdi = FALSE,
          inferenceHdiLevel = 0.95,
          inferenceManual = TRUE,
          mean = TRUE,
          median = TRUE,
          mode = TRUE,
          name = "Plot 1",
          overlayGeomType = "density",
          overlayHistogramBinWidthType = "sturges",
          overlayHistogramManualNumberOfBins = 30,
          parameter = "Cpk",
          parameterOrder = "orderMean",
          parameterSubset = "",
          plotCustomHigh = 10,
          plotCustomLow = 1.33,
          plotInterval = "manual",
          plotsType = "stackedDensity",
          rhat = TRUE,
          savageDickey = FALSE,
          savageDickeyPoint = 0,
          savageDickeyPosteriorMethod = "samplingPosteriorPoint",
          savageDickeyPosteriorSamplingType = "normalKernel",
          savageDickeyPriorHeight = 0,
          savageDickeyPriorMethod = "sampling",
          savageDickeySamplingType = "normalKernel",
          sd = TRUE,
          shadeIntervalInPlot = TRUE
        )
      ),
      densityPlot = FALSE,
      deviance = FALSE,
      exportSamplesFile = "",
      histogramPlot = FALSE,
      initialValues = list(
        list(
          levels = c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
          name = "Parameter",
          values = c("Cpk", "z", "mu", "LSL", "tau", "sigma", "n", "USL")
        ),
        list(
          levels = c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5", "Row 6", "Row 7", "Row 8"),
          name = "R Code",
          values = c("...", "...", "...", "...", "...", "...", "...", "...")
        )
      ),
      legend = TRUE,
      model = list(
        columns = "Measurement",
        model = "# The data block can contain fixed values\n# The variable \"Measurement\" has the observations,\n# and these could be added to the data block.\ndata{\n# USL <- 2.4\n#  LSL <- -2.4 \n#  n   <- length(Measurement)\n  z <- 1\n}\nmodel{\n  USL <- 2.4\n  LSL <- -2.4 \n  n   <- length(Measurement)\n\n  # nonconjugate prior distributions informed by the problem at hand:\n  mu    ~ dunif(LSL,USL) \n  sigma ~ dunif(0,USL-LSL)\n  tau <- pow(sigma,-2) # tau is the precision, i.e., 1/variance\n  # Likelihood:\n  for (i in 1:n){\n  Measurement[i] ~ dnorm(mu,tau)\n  } \n  # Propagate to a measure of capability:\n  Cpk <- min((USL-mu)/(3*sigma),(mu-LSL)/(3*sigma))\n}",
        modelOriginal = "# The data block can contain fixed values\n# The variable \"Measurement\" has the observations,\n# and these could be added to the data block.\ndata{\n# USL <- 2.4\n#  LSL <- -2.4 \n#  n   <- length(Measurement)\n  z <- 1\n}\nmodel{\n  USL <- 2.4\n  LSL <- -2.4 \n  n   <- length(Measurement)\n\n  # nonconjugate prior distributions informed by the problem at hand:\n  mu    ~ dunif(LSL,USL) \n  sigma ~ dunif(0,USL-LSL)\n  tau <- pow(sigma,-2) # tau is the precision, i.e., 1/variance\n  # Likelihood:\n  for (i in 1:n){\n  Measurement[i] ~ dnorm(mu,tau)\n  } \n  # Propagate to a measure of capability:\n  Cpk <- min((USL-mu)/(3*sigma),(mu-LSL)/(3*sigma))\n}",
        parameters = c("Cpk", "z", "mu", "LSL", "tau", "sigma", "n", "USL")
      ),
      monitoredParameters = list(types = list(), value = list()),
      monitoredParametersShown = list(
        types = c("unknown", "unknown", "unknown"),
        value = c("mu", "sigma", "Cpk")
      ),
      plotHeight = 320,
      plotWidth = 480,
      resultsFor = "allParameters",
      samples = 2000,
      seed = 1,
      setSeed = FALSE,
      thinning = 1,
      tracePlot = FALSE,
      userData = list(
        list(
          levels = list(),
          name = "Parameter",
          values = list()
        ),
        list(
          levels = list(),
          name = "R Code",
          values = list()
        )
      )
    ),
    analysisName = "JAGS"
  )

  result <- runAnalysis(options = options, dataset = data.frame(Measurement = rnorm(10)))

  errorMessage <- result$results$mainContainer$collection$mainContainer_mainTable$error$errorMessage

  expect_true(grepl("RUNTIME ERROR:",     errorMessage, fixed = TRUE))
  expect_true(grepl("Unknown variable n", errorMessage, fixed = TRUE))

})

