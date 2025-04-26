options <- analysisOptions("JAGS")
options$customInference <- list(list(ciLevel = 0.95, inferenceData = "contNormal",
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
                                     inferenceHdi = TRUE, inferenceHdiLevel = 0.95, mean = TRUE, mode = TRUE,
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
                                 list(0.133602335511261, 0.138283372282693, 0.102259127567002, 0.251645058422332, 0.640660111794534,
                                      -0.351300850007532, 0.0348333333333333, 0.632776781447128, -0.354233117239452,
                                      5999, "mu<unicode>", 1.00201745378838, 1.00030064493222, -0.0109335880780561,
                                      -0.0142506827661663, 0.0505791336309944, 0.319350253685573, 0.612496638532025, -0.648908825457367,
                                      0.0691666666666667, 0.611192968149425, -0.650154830739537, 5845,
                                      "mu<unicode>", 1.00065933729562, 0.999901204764137, -0.337076587253446,
                                      -0.335829822221644, -0.353460851478873, 0.198588475451556, 0.0525307652862037, -0.735115370473834,
                                      0.0473333333333333, 0.0700901312781582, -0.711785617708476,
                                      6000, "mu<unicode>", 1.00020507425196, 1.00003111989494, -0.351258227334154,
                                      -0.353550929288139, -0.323459720784797, 0.245358918840455, 0.127933783390494, -0.843973868269679,
                                      0.484833333333333, 0.139969156673534, -0.829091036990606, 5999,
                                      "mu<unicode>", 0.999658998258922, 0.999617119294742, -0.384982284191809,
                                      -0.38515229155482, -0.395018451353002, 0.216228053656646, 0.0342138255162074, -0.812154141624535,
                                      0.714, 0.0122545012905073, -0.82415397357156, 6143, "mu<unicode>",
                                      1.0112935232358, 1.0032265318921
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
options$customInference <- list(list(ciLevel = 0.95, inferenceData = "",
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
                                     inferenceHdi = TRUE, inferenceHdiLevel = 0.95, mean = TRUE, mode = TRUE,
                                     median = TRUE, rhat = TRUE, sd = TRUE, shadeIntervalInPlot = TRUE),
                                list(ciLevel = 0.95, inferenceData = "",
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
                                     mean = TRUE, mode = TRUE, median = TRUE, rhat = TRUE, sd = TRUE, shadeIntervalInPlot = TRUE))
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
                                 list(0.942833386190196, 0.865251214181487, 1.20567718845995, 0.743695274837535, 2.1133072882071,
                                      -0.702048475788177, 0.0541666666666667, 2.11348872475636, -0.701947448102273,
                                      529, "mu<unicode>", 1.00501895164085, 1.00144702120523, 0.574473783596011,
                                      0.535509608712149, 0.685060750386006, 0.615641804720132, 1.67571493491245, -0.74996082669382,
                                      0.0651666666666667, 1.65342248303529, -0.758239007248091, 1072,
                                      "mu<unicode>", 1.02338906515785, 1.00698499344117, 0.549862906744529,
                                      0.507107315306628, 0.667378060951214, 0.61600231855459, 1.60762086309086, -0.832548460566659,
                                      0.131166666666667, 1.66277485304878, -0.765725266431562, 734,
                                      "mu<unicode>", 1.00277730689935, 1.00108879353086, 0.128319738227163,
                                      0.11959962612576, 0.129562091770787, 0.536025790278678, 1.15103489646269, -0.955094570448939,
                                      0.148, 1.15724199517942, -0.93882737095928, 2477, "mu<unicode>",
                                      1.00019595040173, 0.999814530625488, -0.426675435212148, -0.412306478224461, -0.529591649505546,
                                      0.531659702136803, 0.711497645540398, -1.42252144402752, 0.156833333333333,
                                      0.685336968392912, -1.42996394887076, 2414, "mu<unicode>", 1.01297676811573,
                                      1.00431258109361, -0.48722247815009, -0.473767975859069, -0.458772723950717, 0.550812789765542,
                                      0.718723336549167, -1.50000808609402, 0.1855, 0.637148067833127,
                                      -1.53717438674336, 1696, "mu<unicode>", 1.01578478760002, 1.00582913369332,
                                      -0.525050218692258, -0.504340179329926, -0.521739910257032, 0.539163034110664, 0.655779275858269,
                                      -1.48990257581031, 0.559666666666667, 0.572785917556837, -1.56736090068435,
                                      1982, "mu<unicode><unicode>", 1.01207040314615, 1.00419028993462,
                                      -0.562193885899904, -0.548664118026812, -0.566513881947277, 0.557911167937291, 0.613701803787932,
                                      -1.60728020276412, 0.593833333333333, 0.499193215158133, -1.6942187092246,
                                      1475, "mu<unicode>", 1.0279772808537, 1.00878651736965, -0.971771879257951,
                                      -0.920336956274568, -1.08658831692418, 0.61566136598295, 0.419995715668148, -2.01879244077542,
                                      0.597, 0.331689388811898, -2.05265118826821, 1016, "mu<unicode>",
                                      1.0291040678345, 1.00923889301844, -1.33887953546541, -1.24259036188083, -1.6355903490587,
                                      0.748439065253367, 0.433759278010013, -2.46798977211001, 0.395833333333333,
                                      0.277910098399801, -2.56446289628071, 348, "mu<unicode>", 1.05180585003254,
                                      1.01976289318015))
})

test_that("Model 2: Stacked density plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencemu"]][["collection"]][["mainContainer_customInferencemu_mu"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-2-stacked-density-1")
})

test_that("Inference for tau table results match", {
  table <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencetau"]][["collection"]][["mainContainer_customInferencetau_customTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.02132289834403, 3.6142176624171, 2.76295009453304, 3.16755720372842, 274, "tau<unicode>",
                                      1.04191602519513, 1.03579220238584, 3.09902335033747, 3.59604828191439, 2.90202726162325,
                                      2.94477490194612, 253, "tau<unicode>", 1.02751213237993, 1.02262859585023,
                                      3.02761306370809, 3.53210155989372, 2.78712678142832, 2.88998140822678, 253, "tau<unicode>",
                                      1.01820738152086, 1.01605403255562, 3.04379251960017, 3.52944209663925, 1.78468644618715,
                                      2.8989289792235, 281, "tau<unicode>", 1.04193069873328, 1.03004993557672,
                                      2.89254367971261, 3.4220146980564, 1.36608079303323, 3.02912166676206, 295, "tau<unicode>",
                                      1.08197832513541, 1.06939286902598, 2.93496650089275, 3.36557139395542, 1.19881816235202,
                                      2.75982675421442, 219, "tau<unicode>", 1.02613321394588, 1.02246688135332,
                                      2.84382231729769, 3.30697913362394, 1.39782027391206, 2.77471483069821, 398, "tau<unicode>",
                                      1.03353801910008, 1.01616194368896, 2.70459043861919, 3.16209202043671, 0.930778924941472,
                                      2.61345847929313, 189, "tau<unicode>", 1.04951413099708, 1.02971699625217,
                                      2.69768615520369, 3.15401991305167, 1.11816231207987, 2.59204227683159, 228, "tau<unicode>",
                                      1.02862791065889, 1.01938435235686))
})

test_that("Stacked density plot matches", {
  plotName <- results[["results"]][["mainContainer"]][["collection"]][["mainContainer_customInferencetau"]][["collection"]][["mainContainer_customInferencetau_tau"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "model-2-stacked-density-2")
})
