context("JAGS")

options <- jaspTools::analysisOptions("JAGS")
options$initialValues <- list(list(levels = "Row 1", name = "Parameter", values = "theta"),
                              list(levels = "Row 1", name = "R Code", values = "..."))
options$model <- list(columns = list(), model = "model{\n theta ~ dbeta(1, 1)\n k ~ dbinom(theta, n)\n mu ~ dnorm(0, 1)}",
                      parameters = c("theta", "k"))
options$burnin <- 1
options$chains <- 4
options$samples <- 50
options$parametersShown <- c("theta", "mu")
options$autoCorPlot <- TRUE
options$densityPlot <- TRUE
options$histogramPlot <- TRUE
options$tracePlot <- TRUE
options$plotBivarHex <- TRUE
options$userData <- list(list(
  levels = c("Row 0", "Row 1"),
  name = "Parameter", values = c("k", "n")
), list(
  levels = c("Row 0", "Row 1"),
  name = "R Code",
  values = c("70", "100")
))
options$parameters <- c("\"theta\"", "\"mu\"")


old.coda.samples <- rjags::coda.samples
new.coda.samples <- function(...) {
  candidates <- c("jags-test-model.rds", file.path("tests", "testthat", "jags-test-model.rds"))
  testFile <- Filter(file.exists, candidates)
  return(readRDS(testFile))
}

jaspTools:::replaceFn("coda.samples", new.coda.samples, "rjags")
on.exit({jaspTools:::replaceFn("coda.samples", old.coda.samples, "rjags")})

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
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_plotContainer"]][["collection"]][["mainContainer_plotContainer_plotBivarHex"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bivariate-scatter-plot")
})
