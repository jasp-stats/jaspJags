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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

#' @importFrom jaspBase createJaspContainer createJaspPlot createJaspState createJaspTable jaspDeps
#' %setOrRetrieve% encodeColNames .extractErrorMessage .hasErrors isTryError .readDataSetToEnd

#' @export
JAGSInternal <- function(jaspResults, dataset, options, state = NULL) {

  # check model
  .JAGSInit(jaspResults, options)
  dataset <- .JAGSReadData(jaspResults, options)

  # run model or update model
  mcmcResult  <- .JAGSrunMCMC(jaspResults, dataset, options)

  # create output
  .JAGSoutputTable  (jaspResults, options, mcmcResult)
  .JAGSmcmcPlots    (jaspResults, options, mcmcResult)

  .JAGScustomInference  (jaspResults, options, mcmcResult)

  .JAGSexportSamples(jaspResults, options, mcmcResult)

  return()

}

.JAGSrunMCMC <- function(jaspResults, dataset, options, priorOnly = FALSE) {

  # TODO for inference:
  #
  # the option `priorOnly` does not work properly when userData
  # specifies something that cannot be sampled from the prior.
  # in order to do that properly, we need to parse the model and determine
  # for each node whether it is one of
  # - stochastic              (so sampled)
  # - stochasticObserved      (so sampled when priorOnly = TRUE)
  # - constant                (cannot be sampled, so data is used even when priorOnly = TRUE)
  # - stochasticDeterministic (optional, a deterministic function of the previous three)
  #
  # to distinghuish between stochasticObserved and constant we probably need to
  # figure out the DAG though...
  # coincidentally, with the DAG we can also reconstruct a function that computes the log posterior
  # and then we can do bridge sampling

  # no need to sample the prior
  if (priorOnly && !.JAGScustomPlotUserWantsInference(options))
    return()

  stateKey <- if (priorOnly) "stateMCMCprior" else "stateMCMC"
  if (!is.null(jaspResults[[stateKey]])) {
    obj <- jaspResults[[stateKey]]$object

    # if monitoredParametersShown changed, update objects.
    # else if parametersMonitored changed, check if we need to sample again.
    return(obj)

  }

  if (!.JAGSgetGoodModel(jaspResults) || jaspResults[["mainContainer"]]$getError())
    return(NULL)

  model <- options[["model"]][["model"]]

  location  <- jaspBase::.fromRCPP(".requestTempFileNameNative", ".txt")
  modelFile <- file.path(location$root, location$relativePath)
  fileConn  <- file(modelFile)
  writeLines(model, fileConn)
  close(fileConn)

  samples        <- options[["samples"]]
  burnin         <- options[["burnin"]]
  thinning       <- options[["thinning"]]
  chains         <- options[["chains"]]
  deviance         <- options[["monitorDIC"]]
  parametersToSave <- if (options[["resultsFor"]] == "allParameters")
    options[["monitoredParametersShown"]]
  else
    options[["monitoredParameters"]]

  datList <- as.list(dataset)

  maybeErrorMessage <- .JAGSloadModules(jaspResults)
  if (!is.null(maybeErrorMessage) && !priorOnly) {
    jaspResults[["mainContainer"]]$setError(maybeErrorMessage)
    return(NULL)
  }

  if (.JAGShasData(options) && !priorOnly) {

    deviance <- TRUE
    # convention: deviance is first parameter!
    parametersToSave <- c("deviance", parametersToSave)

    #     if ("dic" %in% rjags::list.modules())
    # 		  rjags::load.module("dic", quiet = TRUE)
  } else {
    deviance <- FALSE
  }

  # Evaluate user R code, terminate early if the code doesn't work
  inits    <- .JAGSreadRcode(jaspResults, options[["initialValues"]], type = "initial values", chains = options[["chains"]], envir = datList)
  if (jaspResults[["mainContainer"]]$getError()) return(NULL)
  if (all(lengths(inits) == 0L)) inits <- NULL
  userData <- .JAGSreadRcode(jaspResults, options[["userData"]], type = "data", envir = datList)
  if (jaspResults[["mainContainer"]]$getError()) return(NULL)
  if (all(lengths(userData) == 0L)) userData <- NULL

  if (any(names(userData) %in% names(datList))) {
    commonNames <- intersect(names(userData), names(datList))
    jaspResults[["mainContainer"]]$setError(gettextf(
      "The following names appeared both in the data set and in the user specified data:\n%s",
      commonNames
    ))
    return(NULL)
  } else {
    datList <- c(datList, userData)
  }

  # set a seed (same procedure as R2jags)
  jaspBase::.setSeedJASP(options)
  RNGname <- "base::Wichmann-Hill"
  if (is.null(inits)) {
    inits <- vector("list", chains)
    for (i in seq_len(chains)) {
      inits[[i]]$.RNG.name <- RNGname
      inits[[i]]$.RNG.seed <- stats::runif(1, 0, 2^31)
    }
  }

  # this code is similar to how R2jags does it, but with
  # a try around it.
  e <- try({

    # compile model
    model <- rjags::jags.model(
      file     = modelFile,
      n.chains = chains,
      n.adapt  = 0L,
      data     = datList,
      inits    = inits#unname(lapply(inits, list))
    )

    # sample burnin
    rjags::adapt(
      object         = model,
      n.iter         = burnin,
      by             = 0L,
      progress.bar   = "none",
      end.adaptation = TRUE
    )

    # sample remainder
    samples <- rjags::coda.samples(
      model          = model,
      variable.names = parametersToSave,
      n.iter         = samples,
      thin           = thinning,
      by             = 0L,
      progress.bar   = "none"
    )

    fit <- coda:::summary.mcmc.list(samples, quantiles = c(0.025, 0.5, 0.975))
    neff <- coda::effectiveSize(samples)

    # if we only one have one parameters, ensure objects are still matrices with rownames, etc.
    if (length(parametersToSave) == 1L && !is.matrix(fit$statistics)) {
      fit$statistics <- matrix(fit$statistics, 1L, dimnames = list(parametersToSave, names(fit$statistics)))
      fit$quantiles  <- matrix(fit$quantiles,  1L, dimnames = list(parametersToSave, names(fit$quantiles)))
    }
    fit$summary <- cbind(fit$statistics, fit$quantiles, neff)

    # construct parameter list
    allParams <- rownames(fit[["statistics"]])
    params <- vector("list", length = length(parametersToSave))
    names(params) <- parametersToSave
    for (p in parametersToSave) {
      idx <- allParams == p
      if (!any(idx))
        idx <- startsWith(allParams, paste0(p, "["))
      if (any(idx))
        params[[p]] <- allParams[idx]
    }
  })

  # if something went wrong, present useful error message
  if (isTryError(e) && !jaspResults[["mainContainer"]]$getError()) {
    jaspResults[["mainContainer"]]$setError(.JAGSmodelError(e, options))
    return(NULL)
  }

  out <- list(
    model              = model,
    BUGSoutput         = fit,
    parameters.to.save = parametersToSave,
    model.file         = modelFile,
    n.iter             = samples,
    DIC                = deviance,
    samples            = samples,
    hasUserData        = !is.null(userData),
    params             = params,
    rhat               = try(coda::gelman.diag(samples))
  )

  tmp <- createJaspState(object = out)
  tmp$dependOn(c("model", "samples", "burnin", "thinning", "chains", "initialValues", "userData", "resultsFor",
                 "setSeed", "seed"))
  if (options[["resultsFor"]] == "allParameters")
    tmp$dependOn("monitoredParametersShown")
  else
    tmp$dependOn("monitoredParameters")

  if (priorOnly) {
    # TODO for inference: how to do this?
    # options$customInference[[1]]$
    # tmp$dependOn(c(options))
  }

  jaspResults[[stateKey]] <- tmp

  return(out)

}

.JAGSisEmptyModel <- function(model) {
  model <- gsub("\\s", "", model)
  return(model == "model{}" || model == "")
}

.JAGSInit <- function(jaspResults, options) {

  jaspResults$addCitation(.JAGSCitations)
  if (is.null(jaspResults[["mainContainer"]])) {
    # setup outer container with all common dependencies
    mainContainer <- createJaspContainer(dependencies = c("model", "samples", "burnin", "thinning", "chains",
                                                          "parametersMonitored", "monitoredParametersShown", "initialValues", "userData",
                                                          "setSeed", "seed", "deviance"))
    jaspResults[["mainContainer"]] <- mainContainer
  }

  # checks and sets errors
  .JAGSCheckJAGSInstallation(jaspResults[["mainContainer"]])
  if (jaspResults[["mainContainer"]]$getError()) {
    .JAGSsetGoodModel(jaspResults, FALSE)
    return()
  }

  # user specified monitoring?
  manualMonitor   <- options[["resultsFor"]] == "selectedParameters"
  nParamAvailable <- length(options[["model"]][["parameters"]])

  # sum because only parameters can be assigned only once
  if (manualMonitor) {
    nParamMonitored <- length(options[["monitoredParameters"]])
  } else {
    nParamMonitored <- length(options[["monitoredParametersShown"]])
  }
  nParamShown <- length(options[["monitoredParametersShown"]])

  monitorWarning <- NULL
  goodModel <- TRUE
  if (.JAGSisEmptyModel(trimws(options[["model"]][["model"]]))) {
    goodModel <- FALSE
  } else if (nParamAvailable > 0L) {
    if (manualMonitor && nParamMonitored == 0L) {
      goodModel <- FALSE
      monitorWarning <- gettext("Please specify which parameters to monitor!")
    } else if (( manualMonitor && nParamMonitored > 0L && nParamShown == 0L) ||
               (!manualMonitor && nParamShown == 0L)) {
      goodModel <- FALSE
      monitorWarning <- gettext("Please specify which parameters to show output for!")
    }
  }
  .JAGSsetGoodModel     (jaspResults, goodModel)
  .JAGSsetMonitorWarning(jaspResults, monitorWarning)

  return()
}

.JAGSReadData <- function(jaspResults, options) {

  columnsFoundInUserCode <- .JAGSgetColumnNamesInUserCode(options)
  if (jaspResults[["mainContainer"]]$getError() ||
      !.JAGSgetGoodModel(jaspResults) ||
      (!.JAGShasData(options) && length(columnsFoundInUserCode) == 0L))
    return(NULL)

  varsToRead <- union(options[["model"]][["columns"]], columnsFoundInUserCode)
  dataset <- .readDataSetToEnd(columns.as.numeric = varsToRead)
  return(dataset)
}

# Tables ----
.JAGSoutputTable <- function(jaspResults, options, mcmcResult) {

  tb <- createJaspTable(title = gettext("MCMC Summary"))
  tb$position <- 1L
  ovt0 <- gettext("Posterior")
  ovt1 <- gettextf("95%% Credible Interval")
  ovt2 <- gettext("Rhat")
  tb$addColumnInfo(name = "parameter", title = gettext("Parameter"),             type = "string")
  tb$addColumnInfo(name = "Mean",      title = gettext("Mean"),                  type = "number", overtitle = ovt0)
  tb$addColumnInfo(name = "50%",       title = gettext("Median"),                type = "number", overtitle = ovt0)
  tb$addColumnInfo(name = "SD",        title = gettext("SD"),                    type = "number", overtitle = ovt0)
  tb$addColumnInfo(name = "2.5%",      title = gettext("Lower"),                 type = "number", overtitle = ovt1)
  tb$addColumnInfo(name = "97.5%",     title = gettext("Upper"),                 type = "number", overtitle = ovt1)
  tb$addColumnInfo(name = "rhatPoint", title = gettext("Point est."),            type = "number", overtitle = ovt2)
  tb$addColumnInfo(name = "rhatCI",    title = gettext("Upper CI"),              type = "number", overtitle = ovt2)
  tb$addColumnInfo(name = "neff",      title = gettext("Effective Sample Size"), type = "integer")

  if (!is.null(mcmcResult) && !jaspResults[["mainContainer"]]$getError()) {

    tb$addFootnote(gettextf("Output based on %s MCMC draws.",
                            floor(options[["samples"]] / options[["thinning"]]) * options[["chains"]]))

    if (!.JAGShasData(options) && !mcmcResult[["hasUserData"]]) {
      tb$addFootnote(message = gettext("No data was supplied, everything was sampled from the priors!"), symbol = .JAGSWarningSymbol)
      if (options[["deviance"]])
        tb$addFootnote(message = gettext("Deviance cannot be computed without data."), symbol = .JAGSWarningSymbol)
    }

    parametersToShow <- options[["monitoredParametersShown"]]
    if (mcmcResult[["DIC"]] && options[["deviance"]])
      parametersToShow <- c("deviance", parametersToShow)
    sum <- mcmcResult[["BUGSoutput"]][["summary"]]
    nms <- rownames(sum)
    nms2 <- sapply(stringr::str_extract_all(nms, "\\w+"), `[[`, 1L)
    idx <- nms2 %in% parametersToShow

    tbR <- as.data.frame(sum[idx, c("Mean", "SD", "50%", "2.5%", "97.5%", "neff"), drop = FALSE])
    tbR[["neff"]] <- as.integer(tbR[["neff"]])
    tbR$parameter <- nms[idx]

    if (options[["chains"]] > 1L) {

      rhat <- mcmcResult[["rhat"]]
      if (isTryError(rhat)) {
        tb$addFootnote(message = gettext("Failed to compute the Rhat statistic. This is expected if the model contains discrete parameters."),
                       colNames = c("rhatPoint", "rhatCI"))
      } else {

        tbR[["rhatPoint"]] <- rhat[["psrf"]][idx, 1L]
        tbR[["rhatCI"]]    <- rhat[["psrf"]][idx, 2L]
        if (!is.null(rhat[["mpsrf"]])) {
          tb$addFootnote(message = gettextf(
            "The multivariate potential scale reduction factor is estimated at %.3f.",
            rhat[["mpsrf"]]
          ))
        }
      }
    } else {
      tb$addFootnote(message = gettext("Rhat statistic cannot be computed for only one chain. It is strongly recommoned to run more than one chain to assess MCMC convergence!"))
    }
    tb$setData(tbR)
  }

  if (!is.null(message <- .JAGSgetMonitorWarning(jaspResults)))
    tb$addFootnote(message = message, symbol = .JAGSWarningSymbol)

  jaspResults[["mainContainer"]][["mainTable"]] <- tb

  return()
}

# Plots ----
.JAGSmcmcPlots <- function(jaspResults, options, mcmcResult) {

  if (is.null(jaspResults[["mainContainer"]][["plotContainer"]])) {
    plotContainer <- createJaspContainer(dependencies = c("monitoredParametersShown", "colorScheme"))
  } else {
    plotContainer <- jaspResults[["mainContainer"]][["plotContainer"]]
  }

  params <- .JAGSGetParams(options, mcmcResult)
  containerObj <- .JAGSInitPlotsContainers(plotContainer, options, params)
  if (is.null(containerObj) && is.null(plotContainer[["bivariateScatterPlot"]]))
    return()

  # put the container in jaspResults only now so that all empty plots appear at once
  if (is.null(jaspResults[["mainContainer"]][["plotContainer"]]))
    jaspResults[["mainContainer"]][["plotContainer"]] <- plotContainer

  colorpalette <- options[["colorScheme"]]
  oldColorpalette <- jaspGraphs::getGraphOption("palette")
  on.exit(jaspGraphs::setGraphOption("palette", oldColorpalette))
  jaspGraphs::setGraphOption("palette", colorpalette)

  if (!(is.null(mcmcResult) || jaspResults[["mainContainer"]][["plotContainer"]]$getError()))
    .JAGSFillPlotContainers(containerObj, options, mcmcResult, params)

  .JAGSPlotBivariateScatter(plotContainer, options, mcmcResult, params)

}

.JAGSInitPlotsContainers <- function(plotContainer, options, params) {

  # for each plot function, create an empty plot container (or the empty plot object)
  output <- NULL
  if (options[["densityPlot"]]) {

    add <- list("function" = ".JAGSPlotDensity")
    if (is.null(plotContainer[["densityPlot"]])) {
      add[["container"]] <- createJaspContainer(title = gettext("Marginal Density"),  position = 1,
                                                dependencies = c("densityPlot", "aggregatedChains", "legend"))
      plotContainer[["densityPlot"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["densityPlot"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["histogramPlot"]]) {

    add <- list("function" = ".JAGSPlotHistogram")
    if (is.null(plotContainer[["histogramPlot"]])) {
      add[["container"]] <- createJaspContainer(title = gettext("Marginal Histogram"),  position = 2,
                                                dependencies = c("histogramPlot", "aggregatedChains", "legend"))
      plotContainer[["histogramPlot"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["histogramPlot"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["tracePlot"]]) {

    add <- list("function" = ".JAGSPlotTrace")
    if (is.null(plotContainer[["tracePlot"]])) {
      add[["container"]] <- createJaspContainer(title = gettext("Trace Plots"),  position = 3,
                                                dependencies = c("tracePlot", "legend"))
      plotContainer[["tracePlot"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["tracePlot"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["autoCorPlot"]]) {

    add <- list("function" = ".JAGSPlotAutoCor")
    if (is.null(plotContainer[["autoCorPlot"]])) {
      add[["container"]] <- createJaspContainer(title = gettext("Autocorrelation Plots"),  position = 4,
                                                dependencies = c("autoCorPlot", "autoCorPlotLags", "autoCorPlotType", "legend"))
      plotContainer[["autoCorPlot"]] <- add[["container"]]
    } else {
      add[["container"]] <- plotContainer[["autoCorPlot"]]
    }
    output[[length(output) + 1L]] <- add
  }

  if (options[["bivariateScatterPlot"]] && is.null(plotContainer[["bivariateScatterPlot"]])) {

    jaspPlot <- createJaspPlot(title  = gettext("Bivariate Scatter Plot"),  position = 5,
                               dependencies = c("bivariateScatterPlot", "monitoredParametersShown", "bivariateScatterDiagonalType",
                                                "bivariateScatterOffDiagonalType"))
    plotContainer[["bivariateScatterPlot"]] <- jaspPlot

  }

  for (i in seq_along(output))
    .JAGSInitContainerWithPlots(output[[i]][["container"]], params)

  return(output)
}

.JAGSInitContainerWithPlots <- function(jaspContainer, params) {

  if (is.null(params) || length(params) == 0L) {
    # if there are no parameters, we just show a single empty plot
    jaspContainer[["temp"]] <- createJaspPlot()

  } else {

    baseParams <- names(params)
    for (j in seq_along(params))
      for (param in params[[j]])
        if (is.null(jaspContainer[[param]])) {
          jaspPlot <- createJaspPlot(title = param)
          jaspPlot$dependOn(optionContainsValue = list(monitoredParametersShown = param))
          jaspContainer[[param]] <- jaspPlot
        }
  }
}

.JAGSFillPlotContainers <- function(containerObj, options, mcmcResult, params) {

  samples <- mcmcResult[["samples"]]

  baseParams <- names(params)
  for (i in seq_along(containerObj)) {
    jaspContainer <- containerObj[[i]][["container"]]
    plotFun       <- get(containerObj[[i]][["function"]], mode = "function")
    for (j in seq_along(params))
      for (param in params[[j]])
        if (!is.null(jaspContainer[[param]]) && is.null(jaspContainer[[param]]$plotObject)) {
          jaspContainer[[param]]$status     <- "running"
          jaspContainer[[param]]$plotObject <- plotFun(samples, param, options)
        }
  }
}

.JAGSPlotDensity <- function(samples, param, options, removeAxisLabels = FALSE) {

  npoints <- 2^10 # precision for density estimation
  if (!options[["aggregatedChains"]]) {
    df <- do.call(rbind.data.frame, lapply(seq_along(samples), function(i) {
      d <- stats::density(samples[[i]][, param], n = npoints)[c("x", "y")]
      return(data.frame(x = d[["x"]], y = d[["y"]], g = factor(i)))
    }))
    mapping <- ggplot2::aes(x = x, y = y, color = g)
    colorScale <- jaspGraphs::scale_JASPcolor_discrete(name = "Chain")
  } else {
    n <- nrow(samples[[1L]])
    d <- stats::density(unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE))
    df <- data.frame(x = d[["x"]], y = d[["y"]])
    mapping <- ggplot2::aes(x = x, y = y)
    colorScale <- NULL
  }
  if (removeAxisLabels) {
    xName <- yName <- NULL
  } else {
    xName <- param
    yName <- gettext("Density")
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(df[["x"]])
  xLimits <- range(xBreaks)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df[["y"]])
  yLimits <- range(yBreaks)

  g <- ggplot2::ggplot(df, mapping) +
    ggplot2::geom_line(show.legend = !options[["aggregatedChains"]]) +
    colorScale +
    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = yLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["legend"]]) "right" else "none")
  return(g)
}

.JAGSGetHistogramBreaks <- function(samples, param) {

  n <- nrow(samples[[1L]])
  # all unique values for this paramer
  u <- sort(unique(unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE)))

  # if a parameter has 25 unique values or less, we assume the parameter is discrete
  isDiscrete <- length(u) <= 25L

  if (isDiscrete) {
    # This works because by default, graphics::hist makes the first category using e.g., [0, 1], but the next
    # using (1, 2]. The resulting histogram may be heavily misleading, as the first bar can lump two categories
    # together. This only for particular frequencies, but unfortunately, the default number of samples can lead
    # to this when sampling from the prior predictive of binomial(theta) with theta ~ dbeta(1, 1).
    return(list(breaks = c(u[1L], 0.999 + u), unique = u))
  } else {
    return(list(breaks = "Sturges")) # the default of hist
  }
}

.JAGSPlotHistogram <- function(samples, param, options, removeAxisLabels = FALSE) {

  # TODO: get parameter bounds and respect these, e.g., truncate [0, 1] (probably pretty hard though)
  npoints <- 2^10 # precision for density estimation

  tmpBreaks  <- .JAGSGetHistogramBreaks(samples, param)
  breaksType <- tmpBreaks$breaks
  isDiscrete <- !is.character(breaksType)

  if (!options[["aggregatedChains"]]) {
    df <- do.call(rbind.data.frame, lapply(seq_along(samples), function(i) {
      d <- graphics::hist(samples[[i]][, param], breaks = breaksType, plot = FALSE)
      return(data.frame(x = if (isDiscrete) tmpBreaks$unique else d[["mids"]], y = d[["counts"]], g = factor(i)))
    }))
    mapping <- ggplot2::aes(x = x, y = y, color = g, fill = g)
    colorScale <- jaspGraphs::scale_JASPcolor_discrete(name = gettext("Chain"))
    fillScale  <- jaspGraphs::scale_JASPfill_discrete(name = gettext("Chain"))
  } else {
    n <- nrow(samples[[1L]])
    d <- graphics::hist(unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE), breaks = breaksType, plot = FALSE)
    df <- data.frame(x = if (isDiscrete) tmpBreaks$unique else d[["mids"]], y = d[["counts"]])
    mapping <- ggplot2::aes(x = x, y = y)
    fillScale <- colorScale <- NULL
  }

  if (removeAxisLabels) {
    xName <- yName <- NULL
  } else {
    xName <- param
    yName <- gettext("Counts")
  }

  # prevent non-discrete axis labels
  if (isDiscrete) {
    xAxis <- ggplot2::scale_x_continuous(name = xName, breaks = round(jaspGraphs::getPrettyAxisBreaks(tmpBreaks$unique)))
  } else {
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(df[["x"]])
    xLimits <- range(xBreaks)
    xAxis <- ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = xLimits)
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(df[["y"]])
  yLimits <- range(yBreaks)
  yAxis <- ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = yLimits)

  # TODO: (vandenman - 29/03) from ggplot2 version 3.3.0 onwards we need to uncomment the 'orientation = "x"'
  g <- ggplot2::ggplot(df, mapping) +
    ggplot2::geom_bar(show.legend = !options[["aggregatedChains"]], position = ggplot2::position_dodge(), stat = "identity") + #, orientation = "x") +
    xAxis +
    yAxis +
    colorScale +
    fillScale +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["legend"]]) "right" else "none")
  return(g)
}

.JAGSPlotTrace <- function(samples, param, options) {

  n <- nrow(samples[[1L]])
  df <- data.frame(
    x = rep(seq_len(n), length(samples)),
    y = unlist(lapply(samples, `[`, i = 1:n, j = param), use.names = FALSE),
    g = factor(rep(seq_along(samples), each = n))
  )

  g <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = g)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = gettext("Iteration"), y = param) +
    jaspGraphs::scale_JASPcolor_discrete(name = gettext("Chain")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["legend"]]) "right" else "none") +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 10, 0, 0))
  return(g)
}

.JAGSPlotAutoCor <- function(samples, param, options) {

  # TODO: x coordinates should be based on acf()#n.used!
  nchains  <- length(samples)
  nlags    <- options[["autoCorPlotLags"]]
  nvals    <- nlags + 1L # we're getting one more value than nlags since the 0th lag counts.
  acfs <- numeric(nchains * nvals)
  for (i in seq_len(nchains))
    acfs[(1 + (i-1) * nvals):(i * nvals)] <- c(stats::acf(x = samples[[i]][, param], type = "correlation",
                                                          lag.max = nlags, plot = FALSE)$acf)

  df <- data.frame(
    x = 0:nlags,
    y = acfs,
    g = factor(rep(seq_len(nchains), each = nvals))
  )

  if (options[["autoCorPlotType"]] == "lines") {
    geom <- ggplot2::geom_line()
  } else {
    geom <- ggplot2::geom_col(position = ggplot2::position_dodge())
  }
  colorScale <- jaspGraphs::scale_JASPcolor_discrete(name = gettext("Chain"))
  fillScale  <- jaspGraphs::scale_JASPfill_discrete(name = gettext("Chain"))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, nlags))
  xBreaks <- xBreaks[xBreaks <= nlags]
  xLimits <- c(0L, nlags)

  g <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y, color = g, group = g, fill = g)) +
    geom + colorScale + fillScale +
    ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 1.05) +
    ggplot2::ylab(gettext("Autocorrelation")) +
    ggplot2::scale_x_continuous(name = gettext("Lag"), breaks = xBreaks, limits = xLimits) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (options[["legend"]]) "right" else "none")

  return(g)
}

.JAGSPlotBivariateScatter <- function(plotContainer, options, mcmcResult, params) {

  if (is.null(plotContainer[["bivariateScatterPlot"]]) || !is.null(plotContainer[["bivariateScatterPlot"]]$plotObject))
    return()

  jaspPlot <- plotContainer[["bivariateScatterPlot"]]
  nParams <- sum(lengths(params))
  if (nParams >= 2L) {

    jaspPlot$width  <- nParams * 320L
    jaspPlot$height <- nParams * 320L

    if (!plotContainer$getError())
      jaspPlot$plotObject <- .JAGSPlotBivariateMatrix(options, mcmcResult, unlist(params))

  } else if (nParams == 1L) {
    # only show an error when some variables are selected to avoid error messages when users set the options
    jaspPlot$setError(gettext("At least two parameters need to be monitored and shown to make a bivariate scatter plot!"))
  }
}

.JAGSPlotBivariateMatrix <- function(options, mcmcResult, allParams) {

  samples <- mcmcResult[["samples"]]
  nParams <- length(allParams)
  plotMatrix <- matrix(list(), nParams, nParams, dimnames = list(allParams, allParams))

  # these should always be fixed for the matrix plot
  options[["aggregatedChains"]] <- TRUE
  options[["legend"]]      <- FALSE

  # https://github.com/jasp-stats/jasp-test-release/issues/1956
  marginAdjust <- ggplot2::theme(plot.margin = ggplot2::margin(r = 5, l = 5))
  for (j in seq_len(nParams)) {
    for (i in j:nParams) {

      if (i == j) {
        if (options[["bivariateScatterDiagonalType"]] == "density") {
          plotMatrix[[i, j]] <- .JAGSPlotDensity(samples, allParams[j], options, removeAxisLabels = TRUE) + marginAdjust
        } else {
          plotMatrix[[i, j]] <- .JAGSPlotHistogram(samples, allParams[j], options, removeAxisLabels = TRUE) + marginAdjust
        }
      } else {#if (i > j) {
        plotMatrix[[i, j]] <- .JAGSPlotHexOrScatter(samples, allParams[j], allParams[i],
                                                    type = options[["bivariateScatterOffDiagonalType"]])
        plotMatrix[[j, i]] <- plotMatrix[[i, j]] + ggplot2::coord_flip() + marginAdjust
        # } else {
        # TODO: do we want to show anything else for i > j?
      }
    }
  }
  return(jaspGraphs::ggMatrixPlot(plotMatrix))
}

.JAGSPlotHexOrScatter <- function(samples, paramX, paramY, type, removeAxisLabels = TRUE) {

  n <- nrow(samples[[1L]])
  df <- data.frame(
    x = unlist(lapply(samples, `[`, i = 1:n, j = paramX), use.names = FALSE),
    y = unlist(lapply(samples, `[`, i = 1:n, j = paramY), use.names = FALSE)
  )

  if (type == "hexagon") {
    geom <- ggplot2::stat_bin_hex()
    mapping = ggplot2::aes(x = x, y = y, fill = ..density..)
    scaleFill <- jaspGraphs::scale_JASPfill_continuous()
    scaleCol  <- NULL
  } else {
    geom <- ggplot2::stat_density_2d(mapping = ggplot2::aes(fill = stat(level)), geom = "polygon")
    mapping <- ggplot2::aes(x = x, y = y)
    scaleFill <- jaspGraphs::scale_JASPfill_continuous()
    scaleCol  <- jaspGraphs::scale_JASPcolor_continuous()
  }
  labs <- NULL
  if (removeAxisLabels)
    labs <- ggplot2::labs(x = NULL, y = NULL)

  g <- ggplot2::ggplot(data = df, mapping = mapping) +
    geom +
    labs +
    scaleFill +
    scaleCol +
    jaspGraphs::scale_x_continuous() +
    jaspGraphs::scale_y_continuous() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(g)

}

# Custom inference ----
.JAGScustomInference <- function(jaspResults, options, mcmcResult) {

  parameters <- vapplyChr(options[["customInference"]], `[[`, "parameter")
  parameters <- parameters[parameters != ""]

  for (i in seq_along(parameters)) {

    parameter <- parameters[i]
    containerKey <- paste0("customInference", parameter)

    # TODO: sometimes a tab contains invalid options... in that case we skip it and show an error
    if (.JAGSinvalidCustomOptions(options[["customInference"]][[i]])) {
      jaspResults[["mainContainer"]][[containerKey]] <-
        createJaspHtml(text = sprintf("options[[\"customInference\"]][[%d]] is invalid!", i), position = i + 1L)
      next
    }
    container <- jaspResults[["mainContainer"]][[containerKey]]
    if (is.null(container)) {
      container <- createJaspContainer(
        title = gettextf("Custom output for %s", parameter),
        initCollapsed = length(parameters) > 1L,
        position = i + 1L
      )
      container$dependOn(nestedOptions = .JAGSnestedDepsWithBase(
        base = c("customInference", i),
        deps = c("parameter", "parameterSubset", "parameterOrder")
      ))
      jaspResults[["mainContainer"]][[containerKey]] <- container
    }

    customPlotOpts <- options[["customInference"]][[i]]
    customPlotOpts <- jaspBase::.parseAndStoreFormulaOptions(
      container, customPlotOpts,
      c("plotCustomLow", "plotCustomHigh", "inferenceCustomLow", "inferenceCustomHigh")
    )

    params <- .JAGSgetCustomPlotParameters(mcmcResult, customPlotOpts)

    # computes all relevant information
    object <- .JAGScomputeCustomInference(container, mcmcResult, customPlotOpts, params, i)

    if (customPlotOpts[["plotsType"]] == "stackedDensity") {
      .JAGSstackedDensityPlot(container, mcmcResult, customPlotOpts, object, params, i)
    } else {
      # TODO more plots in the future
    }

    .JAGScustomTable(container, mcmcResult, customPlotOpts, object, params, i)

  }
}

.JAGSinvalidCustomOptions <- function(opts) {
  !is.list(opts) || !all(
    c("parameter", "parameterSubset", "parameterOrder")
    %in% names(opts)
  )
}

.JAGScomputeStackedDensityPlotData <- function(params, mcmcResult) {

  npoints <- 512L
  nparams <- length(params)

  plotData <- data.frame(
    x = numeric(nparams * npoints),
    y = numeric(nparams * npoints),
    g = rep(params, each = npoints)
  )

  for (i in seq_len(nparams)) {
    iStart <- 1 + (i - 1) * npoints
    iEnd   <- i * npoints
    r <- iStart:iEnd

    d <- density(unlist(mcmcResult[["samples"]][, params[i], ]))

    plotData[["x"]][r] <- d[["x"]]
    plotData[["y"]][r] <- d[["y"]]

  }

  # as a heuristic for the y-axis height allocated for each density we use the 90% quantile
  # of the maximum densities. Using the maximum might be prone to outliers
  maxDensities <- tapply(plotData[["y"]], plotData[["g"]], max)
  yHeightPerDensity <- stats::quantile(maxDensities, probs = .85)

  # normalize wrt to yHeightPerDensity and add an offset for each parameter
  plotData[["y"]] <- plotData[["y"]] / yHeightPerDensity + yHeightPerDensity * rep(seq_len(nparams) - 1, each = npoints)

  return(list(plotData = plotData, yHeightPerDensity = yHeightPerDensity))
}

.JAGScomputeOverlayData <- function(customPlotOpts, params, plotData, yHeightPerDensity, npoints) {

  dataVar <- customPlotOpts[["inferenceData"]]
  splitVar <- customPlotOpts[["dataSplit"]]
  vars2read <- c(dataVar, splitVar)
  vars2read <- vars2read[vars2read != ""]

  # TODO: JASP should encode this by default, but it does not...
  vars2read2 <- jaspBase::encodeColNames(vars2read)
  print(sprintf("vars2read = %s\n",  toString(vars2read)))
  print(sprintf("vars2read2 = %s\n", toString(vars2read2)))
  overlayRawData <- .readDataSetToEnd(columns = vars2read2)

  if (!identical(vars2read, vars2read2)) {
    dataVar <- vars2read2[[1L]]
    splitVar <- if (splitVar != "") vars2read2[2L]
  }
  parameterOrder <- attr(params, "order")
  ngroups <- length(parameterOrder)

  if (customPlotOpts[["dataSplit"]] == "") {

    if (customPlotOpts[["overlayGeomType"]] == "histogram") {

      breaks <- if (customPlotOpts[["overlayHistogramBinWidthType"]] == "manual") {
        customPlotOpts[["overlayHistogramManualNumberOfBins"]]
      } else {
        customPlotOpts[["overlayHistogramBinWidthType"]]
      }
      h <- graphics::hist(overlayRawData[[dataVar]], plot = FALSE, breaks = breaks)
      breaks <- h$breaks
      nbars <- length(breaks) - 1L

      overlayData <- data.frame(
        x    = rep(h[["mids"]], ngroups),
        ymax = rep(h[["density"]], ngroups),
        ymin = rep(0:(ngroups - 1L) * yHeightPerDensity, each = nbars),
        g    = plotData[["g"]]
      )

      overlayData[["ymax"]] <- overlayData[["ymax"]] + overlayData[["ymin"]]
      attr(overlayData, "width") <- (breaks[2L] - breaks[1L]) / 2.005

    } else {
      d <- stats::density(x = overlayRawData[[dataVar]])
      overlayData <- data.frame(
        x = rep(d[["x"]], ngroups),
        y = rep(d[["y"]], ngroups) + rep(0:(ngroups - 1L) * yHeightPerDensity, each = npoints),
        g = plotData[["g"]]
      )
    }

  } else {

    levelsInData <- if (is.factor(overlayRawData[[splitVar]])) levels(droplevels(overlayRawData[[splitVar]])) else levels(factor(overlayRawData[[splitVar]]))
    if (length(levelsInData) < length(parameterOrder))
      return(gettextf("Warning. There are fewer levels in \"split by\" (%1$s has %2$d levels) than in the parameter or parameter subset (%3$d levels)",
                      customPlotOpts[["dataSplit"]], length(levelsInData), length(parameterOrder)))

    levelsToKeep <- levelsInData[parameterOrder]

    rowIdx <- which(overlayRawData[[splitVar]] %in% levelsToKeep)

    if (customPlotOpts[["overlayGeomType"]] == "histogram") {

      # first compute the histogram on all the data to obtain the breaks
      breaks0 <- if (customPlotOpts[["overlayHistogramBinWidthType"]] == "manual") {
        customPlotOpts[["overlayHistogramManualNumberOfBins"]]
      } else {
        customPlotOpts[["overlayHistogramBinWidthType"]]
      }

      h0 <- graphics::hist(overlayRawData[rowIdx, dataVar], plot = FALSE, breaks = breaks0)
      breaks <- h0$breaks
      nbars <- length(breaks) - 1L

      groupData <- if (is.factor(overlayRawData[[splitVar]])) droplevels(overlayRawData[rowIdx, splitVar]) else overlayRawData[rowIdx, splitVar]
      hs <- tapply(overlayRawData[rowIdx, dataVar], groupData, hist, breaks = breaks, plot = FALSE)

      overlayData <- data.frame(
        x    = c(vapply(hs, `[[`, "mids",    FUN.VALUE = numeric(nbars))),
        ymax = c(vapply(hs, `[[`, "density", FUN.VALUE = numeric(nbars))),
        ymin = rep(0:(ngroups - 1L) * yHeightPerDensity, each = nbars),
        g    = rep(names(hs), each = nbars)
      )

      overlayData[["ymax"]] <- overlayData[["ymax"]] + overlayData[["ymin"]]
      attr(overlayData, "width") <- (breaks[2L] - breaks[1L]) / 2.005

    } else {

      overlayRawDataSplit <- split(overlayRawData[rowIdx, dataVar], overlayRawData[rowIdx, splitVar])
      overlayData <- data.frame(x = numeric(ngroups*npoints), y = numeric(ngroups*npoints), g = plotData[["g"]])
      for (i in seq_len(ngroups)) {

        iStart <- 1 + (i - 1) * npoints
        iEnd <- i * npoints
        r <- iStart:iEnd

        d <- stats::density(overlayRawDataSplit[[i]])

        overlayData[["x"]][r]    <- d[["x"]]
        overlayData[["y"]][r] <- d[["y"]] + (i - 1) * yHeightPerDensity

      }
    }
  }
  return(overlayData)
}

.JAGScomputeCustomInference <- function(container, mcmcResult, customPlotOpts, params, i) {

  npoints <- 512L

  plotData <- container[["statePlotData"]] %setOrRetrieve% (
    .JAGScomputeStackedDensityPlotData(params, mcmcResult) |>
      createJaspState(jaspDeps(nestedOptions = list(c("customInference", i, "parameter"))))
  )
  yHeightPerDensity <- plotData[["yHeightPerDensity"]]
  plotData          <- plotData[["plotData"]]

  overlayPlotData <- if (customPlotOpts[["inferenceData"]] != "") {
    container[["stateOverlayPlotData"]] %setOrRetrieve% (
      .JAGScomputeOverlayData(customPlotOpts, params, plotData, yHeightPerDensity, npoints) |>
        createJaspState(jaspDeps(
          nestedOptions = .JAGSnestedDepsWithBase(
            base = c("customInference", i),
            deps = c("inferenceData", "dataSplit",
                     "overlayGeomType",
                     "overlayHistogramBinWidthType",
                     "overlayHistogramManualNumberOfBins")
          ),
          optionsFromObject = container[["statePlotData"]]
        ))
    )
  }

  tmp <- if (customPlotOpts[["shadeIntervalInPlot"]]) {
    switch(
      customPlotOpts[["plotInterval"]],
      "ci" = .JAGScomputCustomCri(
        customPlotOpts, params, mcmcResult, plotData, yHeightPerDensity, npoints, i, container,
        valueKey = "ciLevel",
        stateKey = "statePlotRibbonData"
      ),
      "hdi" = .JAGScomputCustomHdi(
        customPlotOpts, params, mcmcResult, plotData, yHeightPerDensity, npoints, i, container,
        valueKey = "hdiLevel",
        stateKey = "statePlotRibbonData"
      ),
      "manual" = .JAGScomputeCustomArea(
        customPlotOpts, params, plotData, yHeightPerDensity, npoints, mcmcResult, i, container,
        valueKeyLow  = "plotCustomLow",
        valueKeyHigh = "plotCustomHigh",
        stateKey     = "statePlotRibbonData"
      )
    )
  }
  plotRibbonBounds <- tmp[[1L]]
  plotRibbonData   <- tmp[[2L]]

  # ensure plotRibbonData also depends on the selected radiobutton value
  if (!is.null(container[["statePlotRibbonData"]]))
    container[["statePlotRibbonData"]]$dependOn(nestedOptions = c("customInference", i, "plotInterval"))

  tmp <- if (customPlotOpts[["inferenceCi"]]) {
    .JAGScomputCustomCri(
      customPlotOpts, params, mcmcResult, plotData, yHeightPerDensity, npoints, i, container,
      valueKey = "inferenceCiLevel",
      stateKey = "stateInfCriPlotData"
    )
  }
  criBounds   <- tmp[["criBounds"]]
  criPlotData <- tmp[["criPlotData"]]

  tmp <- if (customPlotOpts[["inferenceHdi"]]) {
    .JAGScomputCustomHdi(
      customPlotOpts, params, mcmcResult, plotData, yHeightPerDensity, npoints, i, container,
      valueKey = "inferenceHdiLevel",
      stateKey = "stateInfHdiPlotData"
    )
  }
  hdiPlotData <- tmp[["hdiPlotData"]]
  hdiBounds   <- tmp[["hdiBounds"]]

  tmp <- if (customPlotOpts[["inferenceManual"]]) {
    .JAGScomputeCustomArea(
      customPlotOpts, params, plotData, yHeightPerDensity, npoints, mcmcResult, i, container,
      valueKeyLow  = "inferenceCustomLow",
      valueKeyHigh = "inferenceCustomHigh",
      stateKey     = "stateInfCustomAreaPlotData"
    )
  }
  customPlotData <- tmp[["customPlotData"]]
  customBounds   <- tmp[["customBounds"]]
  customArea     <- tmp[["customArea"]]

  # NOTE: the xxxBounds objects below are all a matrix with column(.) == params and are 2 x length(params)
  # this is required for .JAGSboundsToRibbon.
  return(list(
    plotData          = plotData,
    overlayPlotData   = overlayPlotData,
    plotRibbonData    = plotRibbonData,
    plotRibbonBounds  = plotRibbonBounds,
    criPlotData       = criPlotData,
    hdiPlotData       = hdiPlotData,
    customPlotData    = customPlotData,
    criBounds         = criBounds,
    hdiBounds         = hdiBounds,
    customBounds      = customBounds,
    customArea        = customArea,
    yHeightPerDensity = yHeightPerDensity
  ))
}

.JAGScomputCustomCri <- function(customPlotOpts, params, mcmcResult, plotData, yHeightPerDensity, npoints, i, container,
                                 valueKey, stateKey) {

  if (!is.null(container[[stateKey]]))
    return(container[[stateKey]]$object)

  criValue <- customPlotOpts[[valueKey]]
  criDelta <- (1 - criValue) / 2
  probs <- c(criDelta, 1 - criDelta)
  criBounds <- vapply(params, \(p) stats::quantile(unlist(mcmcResult[["samples"]][, p, ][[1L]]), probs = probs), FUN.VALUE = numeric(2L))
  colnames(criBounds) <- params
  criPlotData <- .JAGSboundsToRibbonData(criBounds, plotData, yHeightPerDensity, npoints)

  result <- list(criBounds = criBounds, criPlotData = criPlotData)

  stateCriPlotData <- createJaspState(result)
  stateCriPlotData$dependOn(nestedOptions = list(c("customInference", i, valueKey)))
  container[[stateKey]] <- stateCriPlotData
  return(result)
}

.JAGScomputCustomHdi <- function(customPlotOpts, params, mcmcResult, plotData, yHeightPerDensity, npoints, i, container,
                                 valueKey, stateKey) {

  if (!is.null(container[[stateKey]]))
    return(container[[stateKey]]$object)

  hdiLevel <- customPlotOpts[[valueKey]]
  hdiBounds <- vapply(params, \(p) coda::HPDinterval(unlist(mcmcResult[["samples"]][, p, ][[1L]]), prob = hdiLevel), FUN.VALUE = numeric(2L))
  colnames(hdiBounds) <- params
  hdiPlotData <- .JAGSboundsToRibbonData(hdiBounds, plotData, yHeightPerDensity, npoints)

  result <- list(hdiBounds = hdiBounds, hdiPlotData = hdiPlotData)
  stateHdiPlotData <- createJaspState(result)
  stateHdiPlotData$dependOn(nestedOptions = list(c("customInference", i, valueKey)))
  container[[stateKey]] <- stateHdiPlotData
  result

}

.JAGScomputeCustomArea <- function(customPlotOpts, params, plotData, yHeightPerDensity, npoints, mcmcResult, i, container,
                                   valueKeyLow, valueKeyHigh, stateKey) {
  if (!is.null(container[[stateKey]]))
    return(container[[stateKey]]$object)
  customBounds <- matrix(
    sort(c(customPlotOpts[[valueKeyLow]], customPlotOpts[[valueKeyHigh]])),
    nrow = 2L, ncol = length(params), dimnames = list(NULL, params)
  )

  customPlotData <- .JAGSboundsToRibbonData(customBounds, plotData, yHeightPerDensity, npoints)
  customArea     <- vapply(params, \(p) .JAGScomputeArea(unlist(mcmcResult[["samples"]][, p, ][[1L]]), customBounds[, 1L]), FUN.VALUE = numeric(1L))

  result <- list(customBounds = customBounds, customPlotData = customPlotData, customArea = customArea)
  stateCustomPlotData <- createJaspState(result)
  stateCustomPlotData$dependOn(nestedOptions = list(
    c("customInference", i, valueKeyLow),
    c("customInference", i, valueKeyHigh)
  ))
  container[[stateKey]] <- stateCustomPlotData
  return(result)
}

.JAGScomputeArea <- function(x, bounds) {
  # mean(findInterval(sort(x), bounds) == 1L) # is not better nor faster
  mean(x > bounds[1L] & x < bounds[2L])
}

.JAGSstackedDensityPlot <- function(container, mcmcResult, customPlotOpts, object, params, i) {

  parameterName <- customPlotOpts[["parameter"]]
  if (!is.null(container[[parameterName]]))
    return()

  npoints           <- 512L
  nparams           <- length(params)
  plotData          <- object[["plotData"]]
  yHeightPerDensity <- object[["yHeightPerDensity"]]

  ribbon <- .JAGSRibbonDataToRibbon(object[["plotRibbonData"]])
  overlayGeomOrLabs <- .JAGSoverlayDataToGeom(object[["overlayPlotData"]])

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(plotData[["x"]])

  if (is.data.frame(object[["overlayPlotData"]])) {
    width <- attr(object[["overlayPlotData"]], "width")
    if (is.null(width)) { # density
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(xBreaks, object[["overlayPlotData"]][["x"]]))
    } else { # histogram
      extrema <- range(object[["overlayPlotData"]][["x"]])
      extrema <- extrema + c(-width, width)
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(xBreaks, extrema))
    }
  }

  xLimits <- range(xBreaks)


  # slightly unusual heuristic but since we do not show the y-axis we want to be closer to the maximum density
  yLimits <- c(0, 0.1 * yHeightPerDensity + max(plotData[["y"]]))
  yBreaks <- c(seq(0, length = nparams, by = yHeightPerDensity), yLimits[2])
  if (any(grepl("[", params, fixed = TRUE))) {
    # for a plot with 100 parameters/ subscripts, using ggtext is about 1.5 times slower (.467 to .666 seconds for total plotting time, measured with ggplot2::benchplot)
    # but it is much more pretty
    yLabels      <- c(.JAGSParameterToUnicode(params), "")
    yTextElement <- ggplot2::element_text(margin = ggplot2::margin(r = -15))
    #ggtext::element_markdown(halign = 1, align_widths = TRUE, margin = ggplot2::margin(r = -15))
  } else {
    yLabels      <- c(params, "")
    yTextElement <- ggplot2::element_text(margin = ggplot2::margin(r = -15))
  }

  dfAbline <- data.frame(
    slope     = rep(0, nparams),
    intercept = yHeightPerDensity * rep(seq_len(nparams) - 1, each = npoints)
  )

  plt <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y, group = g)) +
    jaspGraphs::geom_abline2(data = dfAbline, mapping = ggplot2::aes(slope = slope, intercept = intercept),
                             color = "grey70", alpha = .70) +
    ribbon +
    overlayGeomOrLabs +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(name = parameterName, breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(name = "Density",     breaks = yBreaks, limits = yLimits, labels = yLabels) +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(
      axis.ticks.length.y = ggplot2::unit(0, "cm"),
      axis.line.y         = ggplot2::element_blank(),
      axis.text.y         = yTextElement,
      plot.caption        = ggplot2::element_text(hjust = 0)
    )

  jaspPlot <- createJaspPlot(plot = plt, title = gettext("Stacked density"), width = 400, height = 400 + 25 * nparams, position = 1L)
  jaspPlot$dependOn(optionsFromObject = container[["statePlotRibbonData"]], nestedOptions = .JAGSnestedDepsWithBase(
    base = c("customInference", i),
    deps = c("plotsType",
             "shadeIntervalInPlot", "plotInterval",
             "inferenceData", "dataSplit",
             "overlayGeomType",
             "overlayHistogramBinWidthType",
             "overlayHistogramManualNumberOfBins")
  ))
  container[[parameterName]] <- jaspPlot

}

.JAGSnestedDepsWithBase <- function(base, deps) {
  lapply(deps, \(dep) c(base, dep))
}

.JAGSboundsToRibbonData <- function(bounds, plotData, yHeightPerDensity, npoints = 512L) {

  params <- colnames(bounds)

  ribbonData <- data.frame(x = numeric(), ymin = numeric(), ymax = numeric(), g = character())
  for (i in seq_along(params)) {
    iStart <- 1 + (i - 1) * npoints
    iEnd   <- i * npoints
    r <- iStart:iEnd

    inside <- findInterval(plotData[r, "x"], vec = bounds[, i])
    insideIdx <- inside == 1L
    nInside <- sum(insideIdx)

    xValues <- plotData[r[insideIdx], "x"]
    yMin    <- yHeightPerDensity * (i - 1)

    # TODO: this might not be 100% compatible with plot editing or custom x-axis bounds!
    ribbonDataSubset <- data.frame(
      x    = plotData[r[insideIdx], "x"],
      ymax = plotData[r[insideIdx], "y"],
      ymin = rep(yHeightPerDensity * (i - 1), nInside),
      g    = rep(params[i], nInside)
    )
    ribbonData <- rbind(ribbonData, ribbonDataSubset)
  }
  return(ribbonData)
}

.JAGSRibbonDataToRibbon <- function(ribbonData) {
  if (is.null(ribbonData))
    return(NULL)

  return(ggplot2::geom_ribbon(
    data        = ribbonData,
    mapping     = ggplot2::aes(x = x, ymin = ymin, ymax = ymax, group = g),
    inherit.aes = FALSE,
    fill        = "grey20",
    alpha       = .5
  ))
}

.JAGSoverlayDataToGeom <- function(overlayData) {
  if (is.null(overlayData))
    return(NULL)
  if (is.character(overlayData)) # shown as a warning
    return(ggplot2::labs(caption = paste(strwrap(overlayData, width = 55), collapse = "\n")))
  width <- attr(overlayData, "width")
  if (!is.null(width)) { # histogram
    return(ggplot2::geom_rect(
      mapping     = ggplot2::aes(xmin = x - width, xmax = x + width, ymin = ymin, ymax = ymax, group = g),
      data        = overlayData,
      inherit.aes = FALSE,
      fill        = scales::alpha("gray", .55),
      color       = scales::alpha("black", .75)
    ))
  } else { # density line (no shaded region under the curve since that may clash with the interval shown)
    return(ggplot2::geom_line(mapping = ggplot2::aes(x = x, y = y, group = g), data = overlayData, inherit.aes = FALSE,
                              color = "black", linetype = "dashed"))
  }
}

.JAGSgetCustomPlotParameters <- function(mcmcResult, customPlotOpts) {

  parameterName <- customPlotOpts[["parameter"]]
  paramsTotal <- mcmcResult$params[[parameterName]]
  nparamsTotal <- length(paramsTotal)

  subset <- .JAGSparameterSubset(customPlotOpts[["parameterSubset"]], nparamsTotal)
  params <- paramsTotal[subset]
  nparams <- length(subset)

  parameterOrder <- switch(customPlotOpts[["parameterOrder"]],
                           "orderMean"   = order(vapplyNum(params, \(p)   mean(unlist(mcmcResult[["samples"]][, p, ][[1L]]))), decreasing = FALSE),
                           "orderMedian" = order(vapplyNum(params, \(p) median(unlist(mcmcResult[["samples"]][, p, ][[1L]]))), decreasing = FALSE),
                           "orderSubset" = rev(seq_along(params)) # so we show 3, 2, 1 when a user puts in 1:3
  )

  params <- params[parameterOrder]
  attr(params, "order") <- parameterOrder
  return(params)
}

.JAGSparameterSubset <- function(userSubset, nparams) {

  if (identical(userSubset, ""))
    return(seq_len(nparams))

  str <- trimws(strsplit(userSubset, ",", fixed = TRUE)[[1L]])
  return(unlist(lapply(str, \(x) eval(parse(text = x)))))

}

.JAGScustomTable <- function(container, mcmcResult, customPlotOpts, object, params, i) {

  if (!is.null(container[["customTable"]]))
    return()

  enablingOptions <- c(
    "mean", "median", "sd", "rhat",
    "inferenceCi", "inferenceHdi", "inferenceManual"
  )

  # hide table if no option is checked
  if (!any(unlist(customPlotOpts[enablingOptions], use.names = FALSE)))
    return()

  parameterName <- customPlotOpts[["parameter"]]
  tb <- createJaspTable(title = gettextf("Inference for %s", parameterName), position = 2L)

  tb$dependOn(nestedOptions = .JAGSnestedDepsWithBase(
    base = c("customInference", i),
    deps = c(enablingOptions, "inferenceCiLevel", "inferenceHdiLevel",
             "inferenceCustomLow", "inferenceCustomHigh")
  ))

  overTitle <- gettext("Statistic")
  tb$addColumnInfo(name = "parameter", title = gettext("Parameter"),             type = "string")

  if (customPlotOpts[["mean"]])   tb$addColumnInfo(name = "Mean",      title = gettext("Mean"),                  type = "number", overtitle = overTitle)
  if (customPlotOpts[["median"]]) tb$addColumnInfo(name = "50%",       title = gettext("Median"),                type = "number", overtitle = overTitle)
  if (customPlotOpts[["sd"]])     tb$addColumnInfo(name = "SD",        title = gettext("SD"),                    type = "number", overtitle = overTitle)

  if (customPlotOpts[["inferenceCi"]]) {
    overTitle <- gettextf("%s%% Credible Interval", 100 * customPlotOpts[["ciLevel"]])
    tb$addColumnInfo(name = "criLower",     title = gettext("Lower"),                 type = "number", overtitle = overTitle)
    tb$addColumnInfo(name = "criHigher",    title = gettext("Upper"),                 type = "number", overtitle = overTitle)
  }

  if (customPlotOpts[["inferenceHdi"]]) {
    overTitle <- gettextf("%s%% HDI", 100 * customPlotOpts[["hdiLevel"]])
    tb$addColumnInfo(name = "hdiLower",     title = gettext("Lower"),                 type = "number", overtitle = overTitle)
    tb$addColumnInfo(name = "hdiHigher",    title = gettext("Upper"),                 type = "number", overtitle = overTitle)
  }

  if (customPlotOpts[["inferenceManual"]]) {
    title <- gettextf("%1$s(%2$s < %3$s < %4$s)",
                      "\u2119", # fancy P
                      customPlotOpts[["inferenceCustomLow"]],
                      "\u03B8", # theta
                      customPlotOpts[["inferenceCustomHigh"]])
    tb$addColumnInfo(name = "customArea", title = title, type = "number")
  }

  # Not sure if it is meaningful to compute these on a subset of the parameters... but we could show it computed on the total!
  if (customPlotOpts[["rhat"]]) {
    overTitle <- gettext("Rhat")
    tb$addColumnInfo(name = "rhatPoint", title = gettext("Point est."),            type = "number", overtitle = overTitle)
    tb$addColumnInfo(name = "rhatCI",    title = gettext("Upper CI"),              type = "number", overtitle = overTitle)
  }

  if (customPlotOpts[["ess"]])
    tb$addColumnInfo(name = "neff",      title = gettext("Effective Sample Size"), type = "integer")

  container[["customTable"]] <- tb
  if (is.null(mcmcResult) || container$getError())
    return()

  # flip the params so that the order in the table matches that in the plot
  # the last row shows smallest/ most negative value and the first row shows the largest/ most positive value
  params <- rev(params)

  df <- data.frame(parameter = .JAGSParameterToUnicode(params))

  summarySubset <- c("Mean", "50%", "SD")[unlist(customPlotOpts[c("mean", "median", "sd")], use.names = FALSE)]
  if (length(summarySubset) > 0L)
    df <- cbind(df,
                as.data.frame(mcmcResult[["BUGSoutput"]][["summary"]][params, summarySubset, drop = FALSE]))
  if (customPlotOpts[["inferenceCi"]])
    df <- cbind(df,
                criLower  = object[["criBounds"]][1L, ],
                criHigher = object[["criBounds"]][2L, ])

  if (customPlotOpts[["inferenceHdi"]])
    df <- cbind(df,
                hdiLower  = object[["hdiBounds"]][1L, ],
                hdiHigher = object[["hdiBounds"]][2L, ])

  if (customPlotOpts[["inferenceManual"]])
    df <- cbind(df, customArea  = object[["customArea"]])

  if (customPlotOpts[["rhat"]])
    df <- cbind(df,
                rhatPoint = mcmcResult[["rhat"]][["psrf"]][params, 1L],
                rhatCI    = mcmcResult[["rhat"]][["psrf"]][params, 2L])

  if (customPlotOpts[["ess"]])
    df[["neff"]] <- as.integer(mcmcResult[["BUGSoutput"]][["summary"]][params, "neff"])

  tb$setData(df)

}

.JAGSParameterToHTML <- function(x) {
  x <- gsub("[", "<sub>", x, fixed = TRUE)
  x <- gsub("]", "</sub>", x, fixed = TRUE)
  x
}

.JAGSParameterToUnicode <- function(x) {

  needsReplacement <- grepl("[", x, fixed = TRUE)
  y <- x[needsReplacement]
  y <- gsub("[", "",  y, fixed = TRUE)
  y <- gsub("]", "",  y, fixed = TRUE)
  # y <- gsub(",", "\u2e12", y, fixed = TRUE)
  y <- gsub(",", "\u201a", y, fixed = TRUE) # supported by more fonts per https://github.com/JuliaLang/julia/issues/34835#issuecomment-589832898
  y <- gsub("(\\d)", "\\\\u208\\1", y)
  x[needsReplacement] <- stringi::stri_unescape_unicode(y)
  x

  # to test
  # x <- c("mu", "mu[1]", "mu[2]", "jaja[1,2]","hahah[1,2,3,4,5]")
  # .JAGSParameterToUnicode(x)
}


# TODO: perhaps just use purrr::map_chr & purrr::map_dbl, etc.?
vapplyChr <- function(x, f, ...) { vapply(x, f, FUN.VALUE = character(1L), ...) }
vapplyNum <- function(x, f, ...) { vapply(x, f, FUN.VALUE = numeric(1L), ...)   }

.JAGScustomPlotUserWantsInference <- function(options) {
  for (opt in options[["customInference"]])
    if (isTRUE(opt[["savageDickeyavageDickey"]]))
      return(TRUE)
  return(FALSE)
}

# Errors ----
.extractJAGSErrorMessage <- function(error) {
  split <- base::strsplit(as.character(error), "\n")[[1]]
  return(trimws(paste(split[-1L], collapse = "\n")))
}

.JAGSmodelError <- function(error, options) {

  errorMessage <- .extractJAGSErrorMessage(error)

  # perhaps some helpful checks...
  modelString <- options[["model"]][["model"]]
  chars <- stringr::fixed(c("[", "]", "{", "}", "(", ")"))
  counts <- stringr::str_count(modelString, chars)
  toAdd <- paste(
    .JAGSmodelErrorString(counts[1:2], chars[1:2]),
    .JAGSmodelErrorString(counts[3:4], chars[3:4]),
    .JAGSmodelErrorString(counts[5:6], chars[5:6])
  )

  if (length(toAdd) > 0L)
    errorMessage <- gettextf("%1$s%2$sIn addition:%3$s%4$s", errorMessage, "\n\n", "\n", toAdd)

  # return error message
  return(errorMessage)
}

.JAGSmodelErrorString <- function(counts, chars) {
  if (counts[1L] == counts[2L]) return(NULL)

  if (counts[1L] < counts[2L]) {
    counts <- counts[2:1]
    chars <- chars[2:1]
  }
  return(gettextf(
    "The model contains more '%1$s' than '%2$s' (%3$d vs %4$d)",
    chars[1L], chars[2L], counts[1L], counts[2L]
  ))
}

.JAGSCheckJAGSInstallation <- function(jaspContainer) {
  # NOTE: this function does setError if loadNamespace("rjags") fails.
  # Thus, all other calls to rjags:: should be preceded by $getError().
  e <- try(loadNamespace("rjags"), silent = TRUE)
  if (isTryError(e)) {
    # Sys.getenv() returns "" if nothing was found
    jaspContainer$setError(gettextf("There was a problem loading JAGS, JAGS_HOME is: '%1$s'.\nPlease contact the JASP team for support.\nError was: %2$s.",
                                    Sys.getenv("JAGS_HOME"), e))
  } else if (isTRUE(rjags::jags.version() < "4.3.0")) {
    jaspContainer$setError(gettextf("Expected JAGS version 4.3.0 but found %s", as.character(rjags::jags.version())))
  }
  return(NULL)
}


# Export to CSV ----
.JAGSexportSamples <- function(jaspResults, options, mcmcResult) {

  if (!options[["actualExporter"]] || is.null(mcmcResult) || jaspResults[["mainContainer"]]$getError())
    return()

  path <- options[["exportSamplesFile"]]
  # some sanity checks on the path
  if (identical(path, ""))
    return()

  # this will probably be common when opening a jasp file from someone else
  if (!dir.exists(dirname(path))) {
    jaspResults[["mainContainer"]][["mainTable"]]$addFootnote(
      message = gettextf("Failed to export the samples because the directory to save them in does not exist."),
      symbol = .JAGSWarningSymbol
    )
    return()
  }

  result <- try({

    noIter    <- nrow(mcmcResult[["samples"]][[1L]])
    noChains  <- length(mcmcResult[["samples"]])

    samplesMatrix <- cbind(
      chain = rep(seq_len(noChains), each = noIter),
      do.call(rbind, mcmcResult[["samples"]])
    )

    write.csv(samplesMatrix, row.names = FALSE, file = path)

  })

  if (jaspBase::isTryError(result))
    jaspResults[["mainContainer"]][["mainTable"]]$addFootnote(
      message = gettextf("Failed to export the samples because the following error occured: %s", .extractErrorMessage(result)),
      symbol = .JAGSWarningSymbol
    )

}

# Helper functions ----
.JAGSGetParams <- function(options, mcmcResult) {

  if (!is.null(mcmcResult)) {
    params <- mcmcResult[["params"]]
    if (!options[["deviance"]]) {
      params <- params[names(params) != "deviance"]
    }

    if (options[["resultsFor"]] == "selectedParameters") {
      return(params[intersect(names(params), options[["monitoredParametersShown"]])])
    } else {
      return(params)
    }
  }

  params <- unlist(options[["monitoredParametersShown"]])
  if (is.null(params))
    return(NULL)
  obj <- as.list(params)
  names(obj) <- params
  return(obj)

}

.JAGSreadRcode <- function(jaspResults, input, type = c("initial values", "data"), chains = 1L, envir = list()) {

  type <- match.arg(type)
  paramNms <- unlist(input[[1L]][["values"]])
  rcodes   <- encodeColNames(unlist(input[[2L]][["values"]]))

  output <- vector("list", length = chains)
  for (j in seq_len(chains)) {
    oneOutput <- vector("list", length = length(paramNms))
    names(oneOutput) <- paramNms
    for (i in seq_along(oneOutput)) {
      string <- rcodes[i]
      if (is.null(string) || string == "" || string == "...") { # this shouldn't be possible, but if string = NULL, parse prompts for user input.
        next
      }
      obj <- try(eval(parse(text = string), envir = envir, enclos = globalenv()))
      if (isTryError(obj)) {
        jaspResults[["mainContainer"]]$setError(gettextf("The R code for %1$s crashed with error:\n%2$s",
                                                         type, .extractErrorMessage(obj)))
        return()
      } else if (!is.numeric(obj)) {
        jaspResults[["mainContainer"]]$setError(gettextf("The result of %1$s R code should be numeric but it was of mode %2$s and class %3$s",
                                                         type, mode(obj), paste(class(obj), collapse = ",")))
        return()
      } else {
        oneOutput[[i]] <- obj
      }
    }
    if (any(lengths(oneOutput) > 0L))
      output[[j]] <- oneOutput
  }
  if (type == "data")
    output <- output[[1L]]
  return(output)
}

.JAGSloadModules <- function(jaspResults) {

  # just like R2jags, we just always load the modules
  e <- try({
    for (m in c("glm", "dic")) # allow users to add custom modules?
      rjags::load.module(m)
  })

  if (isTryError(e))
    return(gettextf("The following error occured while loading the jags modules \"glm\" and \"dic\": %s.", .extractErrorMessage(e)))

  return(NULL)
}

.JAGSgetColumnNamesInUserCode <- function(options) {

  targets <- Filter(
    function(x) x != "...",
    c(
      options$userData[[2L]]$values,
      options$initialValues[[2L]]$values
    )
  )
  # TODO: this is only defined in jasp & jaspTools, but not jaspBase!
  # it should be added to jaspBase, because now jaspBase:::.allColumnNamesDataset fails
  allColumnNames <- .allColumnNamesDataset()
  columnsFound <- c()
  if (length(allColumnNames) > 0L)
    for (target in targets) {
      expr <- parse(text = target)
      newColumnsFound <- .JAGSlocateColumnNamesInExpression(expr, allColumnNames)
      columnsFound <- union(columnsFound, newColumnsFound)
    }
  return(columnsFound)
}

.JAGSlocateColumnNamesInExpression <- function(expr, keyvals, res = c()) {

  if (length(expr) == 0L)
    return()

  for (i in seq_along(expr)) {

    if (is.call(expr[[i]]))
      res <- Recall(expr[[i]][-1L], keyvals, res)

    nm <- deparse(expr[[i]])
    if (is.name(expr[[i]]) && nm %in% keyvals && !(nm %in% res))
      res <- c(res, nm)

  }
  return(res)
}

# one line helpers ----
.JAGSWarningSymbol <- "&#9888;"

.JAGShasData <- function(options) length(options[["model"]][["columns"]]) > 0L

.JAGSsetState <- function(jaspResults, key, value, dependencies = NULL) jaspResults[[key]] <- createJaspState(value, dependencies = dependencies)
.JAGSgetState <- function(jaspResults, key) jaspResults[[key]]$object

.JAGSsetGoodModel <- function(jaspResults, value) .JAGSsetState(jaspResults, "__goodModel", value)
.JAGSgetGoodModel <- function(jaspResults) isTRUE(.JAGSgetState(jaspResults, "__goodModel"))

.JAGSsetMonitorWarning <- function(jaspResults, value) .JAGSsetState(jaspResults, "__monitorWarning", value)
.JAGSgetMonitorWarning <- function(jaspResults) .JAGSgetState(jaspResults, "__monitorWarning")

# References ----
# citation("rjags")
# citation("coda")
# citation("stringr")
.JAGSCitations <- c(
  "rjags"   = "Martyn Plummer (2018). rjags: Bayesian Graphical Models using MCMC. R package version 4-8. https://CRAN.R-project.org/package=rjags",
  "coda"    = "Martyn Plummer, Nicky Best, Kate Cowles and Karen Vines (2006). CODA: Convergence Diagnosis and Output Analysis for MCMC, R News, vol 6, 7-11",
  "stringr" = "Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0. https://CRAN.R-project.org/package=stringr",
  "JAGS"    = "Plummer, Martyn. (2003). JAGS: A Program for Analysis of Bayesian Graphical Models using Gibbs Sampling. 3rd International Workshop on Distributed Statistical Computing (DSC 2003); Vienna, Austria. 124."
)


# TODO long term: ----
# - get parameter bounds and respect these in the plots and density estimation, e.g., truncate [0, 1] (probably pretty hard though).
# - allow custom jags modules?
