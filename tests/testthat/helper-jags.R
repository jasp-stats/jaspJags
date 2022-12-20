old.coda.samples <- rjags::coda.samples
new.coda.samples <- function(...) {
  targetFile <- getOption("JASP_JAGS_UNITTEST_DATAFILE")
  if (is.null(targetFile))
    stop("JASP_JAGS_UNITTEST_DATAFILE is not set!", domain = NA)

  return(readRDS(testthat::test_path(targetFile)))
}

jaspTools:::replaceFn("coda.samples", new.coda.samples, "rjags")
