options <- analysisOptions("JAGS")
options$customInference <- list(list(ciLevel = 0.95, data = "contNormal",
                                     dataSplit = "facFive", name = "Plot 1",
                                     overlayGeomType = "histogram", overlayHistogramBinWidthType = "manual",
                                     overlayHistogramManualNumberOfBins = 30, parameter = "mu",
                                     parameterOrder = "orderMean", parameterSubset = "",
                                     plotCustomHigh = "1", plotCustomLow = "0",
                                     plotInterval = "hdi", plotsType = "stackedDensity",
                                     savageDickeyavageDickey = FALSE, savageDickeyavageDickeyPoint = "0",
                                     savageDickeyavageDickeyPosteriorMethod = "samplingPosteriorPoint",
                                     savageDickeyavageDickeyPosteriorSamplingType = "normalKernel",
                                     savageDickeyavageDickeyPriorHeight = "0", savageDickeyavageDickeyPriorMethod = "sampling",
                                     savageDickeyavageDickeySamplingType = "normalKernel",
                                     ess = TRUE, hdiLevel = 0.95, inferenceCi = TRUE,
                                     inferenceCiLevel = 0.95, inferenceCustomHigh = "1",
                                     inferenceCustomLow = "0", inferenceManual = TRUE,
                                     inferenceHdi = TRUE, inferenceHdiLevel = 0.95, mean = TRUE,
                                     median = TRUE, rhat = TRUE, sd = TRUE, shadeIntervalInPlot = TRUE))
options$overlayGeomType <- "density"
options$plotInterval <- "ci"
options$deviance <- FALSE
options$exportSamplesFile <- ""
options$initialValues <- list(list(levels = c("Row 1", "Row 2"), name = "Parameter", values = c("tau",
                                                                                                "mu")), list(levels = c("Row 1", "Row 2"), name = "R Code", values = c("...",
                                                                                                                                                                       "...")))
options$model <- list(columns = c("contNormal", "facFive"), model = "model{\n\n for (i in 1:5) {\n   mu[i]  ~ dnorm(0, .1)\n   tau[i] ~ dgamma(.1, .1)\n }\n\n  for (i in 1:100) {\n    contNormal[i] ~ dnorm(mu[facFive[i]], tau[facFive[i]])\n  }\n\n}",
                      modelOriginal = "model{\n\n for (i in 1:5) {\n   mu[i]  ~ dnorm(0, .1)\n   tau[i] ~ dgamma(.1, .1)\n }\n\n  for (i in 1:100) {\n    contNormal[i] ~ dnorm(mu[facFive[i]], tau[facFive[i]])\n  }\n\n}",
                      parameters = c("tau", "mu"))
options$monitoredParametersShown <- c("mu", "tau")
options$samples <- 2000
options$userData <- list(list(levels = list(), name = "Parameter", values = list()),
                         list(levels = list(), name = "R Code", values = list()))

options(JASP_JAGS_UNITTEST_DATAFILE = "jags-test-model-customizableInference-1.rds")

set.seed(1)

results <- runAnalysis("JAGS", "debug.csv", options)

test_that("Model 1: Inference for mu table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencemu"]][["collection"]][["mainContainer_customInferencemu_customTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.133602335511261, 0.138283372282693, 0.251645058422332, 0.0203433458631419,
                                      -0.812247968896915, 0.032, 0.00837185400553975, -0.818707889809534,
                                      5999, "mu<unicode>", 1.00201745378838, 1.00030064493222, -0.0109335880780561,
                                      -0.0142506827661663, 0.319350253685573, 0.128698529532083, -0.828230398273665,
                                      0.073, 0.149835793144606, -0.806788543361869, 5845, "mu<unicode>",
                                      1.00065933729562, 0.999901204764137, -0.337076587253446, -0.335829822221644,
                                      0.198588475451556, 0.0528028133540293, -0.728093408292255, 0.046,
                                      0.0674402294496762, -0.701517192431768, 6000, "mu<unicode>",
                                      1.00020507425196, 1.00003111989494, -0.351258227334154, -0.353550929288139,
                                      0.245358918840455, 0.60256007776191, -0.612576413436111, 0.481,
                                      0.55989626472293, -0.652843739585329, 5999, "mu<unicode>", 0.999658998258922,
                                      0.999617119294742, -0.384982284191809, -0.38515229155482, 0.216228053656646,
                                      0.620953611239903, -0.338825051377995, 0.71, 0.595971218263128,
                                      -0.35344356985566, 6143, "mu<unicode>", 1.0112935232358, 1.0032265318921
                                 ))
})

test_that("Model 1: Stacked density plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencemu"]][["collection"]][["mainContainer_customInferencemu_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-1-stacked-density")
})

test_that("Model 1: MCMC Summary table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_mainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.735115370473834, -0.337076587253446, 0.0525307652862037, -0.335829822221644,
                                      0.198588475451556, 6000, "mu[1]", 1.00020507425196, 1.00003111989494,
                                      -0.812154141624535, -0.384982284191809, 0.0342138255162074,
                                      -0.38515229155482, 0.216228053656646, 6143, "mu[2]", 1.0112935232358,
                                      1.0032265318921, -0.351300850007532, 0.133602335511261, 0.640660111794534,
                                      0.138283372282693, 0.251645058422332, 5999, "mu[3]", 1.00201745378838,
                                      1.00030064493222, -0.84397386826968, -0.351258227334154, 0.127933783390494,
                                      -0.353550929288139, 0.245358918840455, 5999, "mu[4]", 0.999658998258922,
                                      0.999617119294742, -0.648908825457368, -0.0109335880780561,
                                      0.612496638532025, -0.0142506827661663, 0.319350253685573, 5845,
                                      "mu[5]", 1.00065933729562, 0.999901204764137, 0.643816827999882,
                                      1.32905481130009, 2.36962352012354, 1.37823059658096, 0.444282587047275,
                                      5722, "tau[1]", 1.00747959457349, 1.00286375149186, 0.590901296743124,
                                      1.20077256943454, 2.14972998970762, 1.24399847626223, 0.402191240470682,
                                      5489, "tau[2]", 1.00581220709952, 1.00165847654655, 0.405672694151748,
                                      0.8313619922012, 1.49063928451176, 0.865317529459832, 0.279531316529832,
                                      5482, "tau[3]", 1.00610743374827, 1.0018776668539, 0.432487507473548,
                                      0.884659078264677, 1.59614060822089, 0.921619100806194, 0.300896032600605,
                                      5758, "tau[4]", 1.00617479051822, 1.00261878558365, 0.254513803148647,
                                      0.528682080860744, 0.944728722126935, 0.547341780478094, 0.175095540540636,
                                      5638, "tau[5]", 1.00368268931584, 1.00079106174679))
})

# reuses previous options
options[["customInference"]][[1L]]$dataSplit <- "contBinom"

options(JASP_JAGS_UNITTEST_DATAFILE = "jags-test-model-customizableInference-1.rds")

set.seed(1)
results <- runAnalysis("JAGS", "debug.csv", options)

test_that("Stacked density plot with incorrect split by matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencemu"]][["collection"]][["mainContainer_customInferencemu_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-1-stacked-density-bad-split")
})


options <- analysisOptions("JAGS")
options$customInference <- list(list(ciLevel = 0.95, data = "",
                                     dataSplit = "", name = "Plot 1",
                                     overlayGeomType = "histogram", overlayHistogramBinWidthType = "manual",
                                     overlayHistogramManualNumberOfBins = 30, parameter = "mu",
                                     parameterOrder = "orderMean", parameterSubset = "1:10",
                                     plotCustomHigh = "1", plotCustomLow = "0",
                                     plotInterval = "hdi", plotsType = "stackedDensity",
                                     savageDickeyavageDickey = FALSE, savageDickeyavageDickeyPoint = "0",
                                     savageDickeyavageDickeyPosteriorMethod = "samplingPosteriorPoint",
                                     savageDickeyavageDickeyPosteriorSamplingType = "normalKernel",
                                     savageDickeyavageDickeyPriorHeight = "0", savageDickeyavageDickeyPriorMethod = "sampling",
                                     savageDickeyavageDickeySamplingType = "normalKernel",
                                     ess = TRUE, hdiLevel = 0.95, inferenceCi = TRUE,
                                     inferenceCiLevel = 0.95, inferenceCustomHigh = "1",
                                     inferenceCustomLow = "0", inferenceManual = TRUE,
                                     inferenceHdi = TRUE, inferenceHdiLevel = 0.95, mean = TRUE,
                                     median = TRUE, rhat = TRUE, sd = TRUE, shadeIntervalInPlot = TRUE),
                                list(ciLevel = 0.95, data = "",
                                     dataSplit = "", name = "Plot 2",
                                     overlayGeomType = "density", overlayHistogramBinWidthType = "sturges",
                                     overlayHistogramManualNumberOfBins = 30,
                                     parameter = "tau", parameterOrder = "orderMean",
                                     parameterSubset = "1:9", plotCustomHigh = "1",
                                     plotCustomLow = "0", plotInterval = "manual",
                                     plotsType = "stackedDensity", savageDickeyavageDickey = FALSE,
                                     savageDickeyavageDickeyPoint = "0", savageDickeyavageDickeyPosteriorMethod = "samplingPosteriorPoint",
                                     savageDickeyavageDickeyPosteriorSamplingType = "normalKernel",
                                     savageDickeyavageDickeyPriorHeight = "0", savageDickeyavageDickeyPriorMethod = "sampling",
                                     savageDickeyavageDickeySamplingType = "normalKernel",
                                     ess = TRUE, hdiLevel = 0.95, inferenceCi = FALSE,
                                     inferenceCiLevel = 0.95, inferenceCustomHigh = "1",
                                     inferenceCustomLow = "0", inferenceManual = FALSE,
                                     inferenceHdi = FALSE, inferenceHdiLevel = 0.95,
                                     mean = TRUE, median = TRUE, rhat = TRUE, sd = TRUE, shadeIntervalInPlot = TRUE))
options$overlayGeomType <- "density"
options$plotInterval <- "ci"
options$deviance <- FALSE
options$exportSamplesFile <- ""
options$initialValues <- list(list(levels = c("Row 1", "Row 2", "Row 3", "Row 4", "Row 5",
                                              "Row 6"), name = "Parameter", values = c("tau", "grandTauMu",
                                                                                       "grandMuTau", "mu", "grandTauTau", "grandMuMu")), list(levels = c("Row 1",
                                                                                                                                                         "Row 2", "Row 3", "Row 4", "Row 5", "Row 6"), name = "R Code",
                                                                                                                                              values = c("...", "...", "...", "...", "...", "...")))

options$model <- list()
options$model$model         <- "model{\n\n  grandMuMu   ~ dnorm(0, .1)\n  grandMuTau  ~ dgamma(.1, .1)\n  grandTauMu  ~ dnorm(0, .1) T(0, )\n  grandTauTau ~ dgamma(.1, .1)\n\n  for (i in 1:10) {\n    mu[i]  ~ dnorm(grandMuMu, grandMuTau)\n    tau[i] ~ dgamma(\n      # transform mean and precision to shape and rate\n      grandTauMu * grandTauMu * grandTauTau, \n                   grandTauMu * grandTauTau\n    )\n  }\n\n  for (i in 1:10) {\n    contNormal[i] ~ dnorm(mu[i], tau[i])\n  }\n\n}"
options$model$modelOriginal <- "model{\n\n  grandMuMu   ~ dnorm(0, .1)\n  grandMuTau  ~ dgamma(.1, .1)\n  grandTauMu  ~ dnorm(0, .1) T(0, )\n  grandTauTau ~ dgamma(.1, .1)\n\n  for (i in 1:10) {\n    mu[i]  ~ dnorm(grandMuMu, grandMuTau)\n    tau[i] ~ dgamma(\n      # transform mean and precision to shape and rate\n      grandTauMu * grandTauMu * grandTauTau, \n                   grandTauMu * grandTauTau\n    )\n  }\n\n  for (i in 1:10) {\n    contNormal[i] ~ dnorm(mu[i], tau[i])\n  }\n\n}"
options$model$columns <- "contNormal"
options$model$parameters <- c("tau", "grandTauMu", "grandMuTau", "mu", "grandTauTau", "grandMuMu")

options$monitoredParametersShown <- c("tau", "mu")
options$samples <- 2000
options$userData <- list(list(levels = list(), name = "Parameter", values = list()),
                         list(levels = list(), name = "R Code", values = list()))

options(JASP_JAGS_UNITTEST_DATAFILE = "jags-test-model-customizableInference-2.rds")

set.seed(1)
results <- runAnalysis("JAGS", "debug.csv", options)

test_that("Model 2: Inference for mu table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencemu"]][["collection"]][["mainContainer_customInferencemu_customTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.942833386190196, 0.865251214181487, 0.743695274837535, 0.708089375780404,
                                      -2.42673314132405, 0.067, 0.433749791111257, -2.56867052033816,
                                      529, "mu<unicode>", 1.00501895164085, 1.00144702120523, 0.574473783596011,
                                      0.535509608712149, 0.615641804720132, 0.640502130348028, -2.0040889476173,
                                      0.076, 0.355472191099449, -2.16239846024205, 1072, "mu<unicode>",
                                      1.02338906515785, 1.00698499344117, 0.549862906744529, 0.507107315306628,
                                      0.61600231855459, 0.820731540873883, -1.59277099730187, 0.1525,
                                      0.715417831479595, -1.65334321724923, 734, "mu<unicode>", 1.00277730689935,
                                      1.00108879353086, 0.128319738227163, 0.11959962612576, 0.536025790278678,
                                      0.807923095310907, -1.4776010939342, 0.1605, 0.775848599748181,
                                      -1.4868246960011, 2477, "mu<unicode>", 1.00019595040173, 0.999814530625488,
                                      -0.426675435212148, -0.412306478224461, 0.531659702136803, 0.93195256453489,
                                      -1.50979455384213, 0.1715, 0.748303843853299, -1.57864257274597,
                                      2414, "mu<unicode>", 1.01297676811573, 1.00431258109361, -0.48722247815009,
                                      -0.473767975859069, 0.550812789765542, 0.862869178941269, -1.38475029078627,
                                      0.189, 0.866833652656244, -1.38234622786315, 1696, "mu<unicode>",
                                      1.01578478760002, 1.00582913369332, -0.525050218692258, -0.504340179329926,
                                      0.539163034110664, 1.15355936783457, -0.938994472134647, 0.558,
                                      1.1795464605845, -0.903764916673012, 1982, "mu<unicode><unicode>",
                                      1.01207040314615, 1.00419028993462, -0.562193885899904, -0.548664118026812,
                                      0.557911167937291, 1.60462201046021, -0.785536084738769, 0.5655,
                                      1.60747790547589, -0.78365869371817, 1475, "mu<unicode>", 1.0279772808537,
                                      1.00878651736965, -0.971771879257951, -0.920336956274568, 0.61566136598295,
                                      1.69053089244269, -0.801272187242055, 0.595, 1.69519467771385,
                                      -0.793143708903688, 1016, "mu<unicode>", 1.0291040678345, 1.00923889301844,
                                      -1.33887953546541, -1.24259036188083, 0.748439065253367, 2.19753230372668,
                                      -0.662276092922952, 0.403, 2.12896803690771, -0.7080541622189,
                                      348, "mu<unicode>", 1.05180585003254, 1.01976289318015))
})

test_that("Model 2: Stacked density plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencemu"]][["collection"]][["mainContainer_customInferencemu_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-2-stacked-density-1")
})

test_that("Inference for tau table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencetau"]][["collection"]][["mainContainer_customInferencetau_customTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.02132289834403, 3.6142176624171, 3.16755720372842, 274, "tau<unicode>",
                                      1.04191602519513, 1.03579220238584, 3.09902335033747, 3.59604828191439,
                                      2.94477490194612, 253, "tau<unicode>", 1.02751213237993, 1.02262859585023,
                                      3.02761306370809, 3.53210155989372, 2.88998140822678, 253, "tau<unicode>",
                                      1.01820738152086, 1.01605403255562, 3.04379251960017, 3.52944209663925,
                                      2.8989289792235, 281, "tau<unicode>", 1.04193069873328, 1.03004993557672,
                                      2.89254367971261, 3.4220146980564, 3.02912166676206, 295, "tau<unicode>",
                                      1.08197832513541, 1.06939286902598, 2.93496650089275, 3.36557139395542,
                                      2.75982675421442, 219, "tau<unicode>", 1.02613321394588, 1.02246688135332,
                                      2.84382231729769, 3.30697913362394, 2.77471483069821, 398, "tau<unicode>",
                                      1.03353801910008, 1.01616194368896, 2.70459043861919, 3.16209202043671,
                                      2.61345847929313, 189, "tau<unicode>", 1.04951413099708, 1.02971699625217,
                                      2.69768615520369, 3.15401991305167, 2.59204227683159, 228, "tau<unicode>",
                                      1.02862791065889, 1.01938435235686))
})

test_that("Stacked density plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencetau"]][["collection"]][["mainContainer_customInferencetau_tau"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-2-stacked-density-2")
})
