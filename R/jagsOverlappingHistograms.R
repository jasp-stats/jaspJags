# TODO: move to jaspGraphs?
.JAGSoverlappingHistogramDensity <- function(
    x, g,
    type  = c("histogram", "density"),
    xName = NULL,
    yName = if (type == "histogram") gettext("Counts") else gettext("Density"),
    groupName = NULL,
    position = ggplot2::position_identity()
) {

  type <- match.arg(type)

  if (type == "histogram") {

    h0 <- hist(x, plot = FALSE)
    breaks <- h0$breaks
    nbars <- length(breaks) - 1L

    hs <- tapply(x, g, hist, breaks = breaks, plot = FALSE)

    df <- data.frame(
      x = c(vapply(hs, `[[`, "mids",   FUN.VALUE = numeric(nbars))),
      y = c(vapply(hs, `[[`, "counts", FUN.VALUE = numeric(nbars))),
      g = rep(names(hs), each = nbars)
    )
    geom <- ggplot2::geom_col(alpha = .6, position = position, show.legend = TRUE)
    mapping <- ggplot2::aes(x = x, y = y, group = g, color = g, fill = g)

    thm <- NULL

  } else {

    npoints <- 512L
    ds <- tapply(x, g, density)

    df <- data.frame(
      x = c(vapply(ds, `[[`, "x", FUN.VALUE = numeric(npoints))),
      y = c(vapply(ds, `[[`, "y", FUN.VALUE = numeric(npoints))),
      g = rep(names(hs), each = npoints)
    )
    geom <- list(
      ggplot2::geom_line(alpha = .6, show.legend = FALSE),
      ggplot2::geom_ribbon(alpha = .6, show.legend = TRUE)
    )
    mapping <- ggplot2::aes(x = x, y = y, group = g, color = g, fill = g, ymin = 0, ymax = y)

    thm <- ggplot2::theme(axis.ticks.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())

  }

  ggplot2::ggplot(data = df, mapping) +
    geom +
    jaspGraphs::scale_y_continuous(name = yName) +
    jaspGraphs::scale_x_continuous(name = xName) +
    jaspGraphs::scale_JASPfill_discrete(name = groupName) +
    jaspGraphs::scale_JASPcolor_discrete(name = groupName) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = c(.85, .95)) +
    thm
}
