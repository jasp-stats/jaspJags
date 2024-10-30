# Adapted from R package rjags to display a progress bar in jasp
#
# sources:
#   - https://github.com/cran/rjags/blob/b97b9785fbfcf0e0219f9f0fc2d7bfdad51cb7a5/R/jags.object.R
#   - https://github.com/cran/rjags/blob/b97b9785fbfcf0e0219f9f0fc2d7bfdad51cb7a5/R/jags.R
#
# modified functions:
#   - update.jags
#
# copied functions without any further changes except for now calling the modified function(s)
#   - adapt
#   - jags.model
#   - coda.samples
#
# all modified call sites are postfixed with # CALLS MODIFIED FUNCTION
# some calls are prefixed with rjags:::, these are not postfixed with a comment
#
# new values for progress.bar:
#   - "jasp"       - automatic progress bar, start with jaspBase::startProgressbar and update with jaspBase::progressbarTick. Customize the label via "jaspLabel".
#   - "jaspManual" - manual progress bar, only calls to jaspBase::progressbarTick are made.
#
update.jags <- function(object, n.iter = 1, by, progress.bar, jaspLabel, ...) {
  if (!is.numeric(n.iter) || n.iter < 1) {
    stop("Invalid n.iter")
  }

  adapting <- .Call("is_adapting", object$ptr(), PACKAGE="rjags")
  on.exit(object$sync())

  if (missing(progress.bar)) {
    progress.bar <- getOption("jags.pb")
  }
  if (!is.null(progress.bar)) {
    match.arg(progress.bar, c("text","gui","none", "jasp", "jaspManual"))
    if (progress.bar=="none")
      progress.bar <- NULL
  }

  ## Set refresh frequency for progress bar
  if (missing(by) || by <= 0) {
    ##In JAGS 3.x.y there is a memory reallocation bug when
    ##monitoring that slows down updates. Drop refresh
    ##frequency to avoid triggering memory reallocations.
    ##by <- min(ceiling(n.iter/50), 100)
    by <- ceiling(n.iter/50)
  }
  else {
    by <- ceiling(by)
  }

  jaspPbAutomatic <- identical(progress.bar, "jasp")
  jaspPbManual    <- identical(progress.bar, "jaspManual")
  jaspPb <- jaspPbManual || jaspPbAutomatic

  if (jaspPbAutomatic) {
    if (missing(jaspLabel))
      jaspLabel <- if (adapting) "Adapting" else "Sampling"

    expectedNoTicks <- ceiling((n.iter + 1) / by)
    jaspBase::startProgressbar(expectedNoTicks, jaspLabel)
  }

  do.pb <- interactive() && !is.null(progress.bar) && n.iter >= 100
  if (do.pb) {
    start.iter <- object$iter()
    end.iter <- start.iter + n.iter
    pb <- switch(progress.bar,
                 "text" = txtProgressBar(start.iter, end.iter,
                                         initial = start.iter, style=3, width=50,
                                         char=ifelse(adapting,"+","*")),
                 "gui" = rjags:::updatePB(start.iter, end.iter, adapting))
  }

  ## Do updates
  n <- n.iter
  tick <- 1
  while (n > 0) {
    .Call("update", object$ptr(), min(n,by), PACKAGE="rjags")
    if (jaspPb) {
      jaspBase::progressbarTick()
    } else if (do.pb) {
      cat(sprintf("progress.bar update n: %d, tick %d\n", n, tick))
      tick <- tick + 1
      switch(progress.bar,
             "text" = setTxtProgressBar(pb, object$iter()),
             "gui" =  rjags:::setPB(pb, object$iter()))
    }
    n <- n - by
  }
  if (do.pb) {
    close(pb)
  }

  invisible(NULL)
}

adapt <- function(object, n.iter, end.adaptation = FALSE, ...) {
  if(.Call("is_adapting", object$ptr(), PACKAGE="rjags")) {
    if(n.iter > 0)
      update.jags(object, n.iter, ...) # CALLS MODIFIED FUNCTION
    ok <- .Call("check_adaptation", object$ptr(), PACKAGE="rjags")
    if (end.adaptation) {
      .Call("adapt_off", object$ptr(), PACKAGE="rjags")
    }
    return(ok)

  }
  else {
    return(TRUE)
  }
}

coda.samples <- function(model, variable.names=NULL, n.iter, thin=1,
                         na.rm = TRUE, ...)
{
  start <- model$iter() + thin
  out <- jags.samples(model, variable.names, n.iter, thin, type="trace", ...)

  ans <- vector("list", rjags:::nchain(model))
  for (ch in 1:rjags:::nchain(model)) {
    ans.ch <- vector("list", length(out))

    vnames.ch <- NULL
    for (i in seq(along=out)) {

      varname <- names(out)[[i]]
      d <- dim(out[[i]])
      if (length(d) < 3) {
        stop("Invalid dimensions for sampled output")
      }
      vardim <- d[1:(length(d)-2)]
      nvar <- prod(vardim)
      niter <- d[length(d) - 1]
      nchain <- d[length(d)]

      values <- as.vector(out[[i]])
      var.i <- matrix(NA, nrow=niter, ncol=nvar)
      for (j in 1:nvar) {
        var.i[,j] <- values[j + (0:(niter-1))*nvar + (ch-1)*niter*nvar]
      }
      vnames.ch <- c(vnames.ch, rjags:::coda.names(varname, vardim))
      ans.ch[[i]] <- var.i
    }

    ans.ch <- do.call("cbind", ans.ch)
    colnames(ans.ch) <- vnames.ch
    ans[[ch]] <- coda::mcmc(ans.ch, start=start, thin=thin)
  }

  if (isTRUE(na.rm)) {
    ## Drop variables that are missing for all iterations in at least
    ## one chain
    all.missing <- sapply(ans, function(x) {apply(is.na(x), 2, any)})
    drop.vars <- if (is.matrix(all.missing)) {
      apply(all.missing, 1, any)
    }
    else {
      any(all.missing)
    }
    ans <- lapply(ans, function(x) return(x[, !drop.vars, drop=FALSE]))
  }

  coda::mcmc.list(ans) # MODIFIED: prefixed `coda::`
}

jags.samples <- function(model, variable.names, n.iter, thin=1, type="trace", force.list=FALSE, ...) {
    if (!inherits(model, "jags"))
      stop("Invalid JAGS model")

    if (!is.character(variable.names) || length(variable.names) == 0)
      stop("variable.names must be a character vector")

    if (!is.numeric(n.iter) || length(n.iter) != 1 || n.iter <= 0)
      stop("n.iter must be a positive integer")
    if (!is.numeric(thin) || length(thin) != 1 || thin <= 0)
      stop("thin must be a positive integer")
    if (!is.character(type) || length(type) == 0)
      stop("type must be a character vector")

    ####  Allow vectorisation of type argument and variable.names argument
    if(length(type)==1){
      type <- rep(type, length(variable.names))
    }else if(length(variable.names)==1){
      variable.names <- rep(variable.names, length(type))
    }
    if(length(type)!=length(variable.names))
      stop("non matching lengths of monitor type and variable.names")

    ####  Set monitors must be called for each relevant monitor type
    status <- lapply(unique(type), function(t){
      pn <- rjags:::parse.varnames(variable.names[type==t])
      status <- .Call("set_monitors", model$ptr(), pn$names, pn$lower, pn$upper,
                      as.integer(thin), t, PACKAGE="rjags")
    })
    names(status) <- unique(type)
    if (!any(unlist(status))) stop("No valid monitors set")

    startiter <- model$iter()
    n.iter <- n.iter - n.iter%%thin

    update.jags(model, n.iter, ...) # CALLS MODIFIED FUNCTION

    ####  Retrieve values for each monitor type being used
    usingtypes <- unique(type)[sapply(unique(type), function(t) return(any(status[[t]])))]
    allans <- lapply(usingtypes, function(t){
      ans <- .Call("get_monitored_values", model$ptr(), t, PACKAGE="rjags")
      for (i in seq(along=ans)) {
        class(ans[[i]]) <- "mcarray"
        attr(ans[[i]], "varname") <- names(ans)[i]

        # Assure there is a valid dim attribute for pooled scalar nodes:
        if(is.null(dim(ans[[i]]))){
          dim(ans[[i]]) <- length(ans[[i]])
        }

        # New attributes for rjags_4-7:
        attr(ans[[i]], "type") <- t
        attr(ans[[i]], "iterations") <- c(start=startiter+thin, end=startiter+n.iter, thin=thin)
      }
      pn <- rjags:::parse.varnames(variable.names[type==t])
      for (i in seq(along=variable.names[type==t])) {
        if (status[[t]][i]) {
          .Call("clear_monitor", model$ptr(), pn$names[i], pn$lower[[i]],
                pn$upper[[i]], t, PACKAGE="rjags")
        }
      }
      return(ans)
    })

    ####  The return value is a named list of monitor types
    names(allans) <- usingtypes

    ####  Remove any that are empty:
    allans <- allans[sapply(allans, length) > 0]

    ####  And if all monitors are of the same type and !force.list then return
    ####  just a single element for back compatibility with rjags < 4-7
    if(!force.list && length(usingtypes)==1)
      allans <- allans[[1]]

    return(allans)
  }
