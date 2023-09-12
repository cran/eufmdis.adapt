#' Plot graph of a time series with daily error margin
#'
#' Creates a line plot with a shaded polygon showing daily error margins
#' (uncertainty ranges)
#'
#' @param x Data frame with columns \code{day} containing the counter for the
#' time steps (=days), \code{q025} for the 2.5 percentiles
#' (i.e. the lower values for the error margin), \code{median} for the median
#' values (i.e. the values for the line plot) and \code{q975} for the 97.5
#' percentiles (i.e. the upper values for the error margin).
#' @param parameter Character to use as label of the y-axis
#' @param main (optional) character to use as plot title
#'
#' @return No return value. Creates a plot.
#' @author Ian Kopacka


# Example:
# x <- data.frame(
#   day = 1:20,
#   q025 = 1:20,
#   median = (1:20)*1.5,
#   q975 = (1:20)*1.8
# )
# eufmdis.adapt:::plot_time_series(x, "Costs", "This is a nice plot")
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-16
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
plot_time_series <- function(x, parameter, main = ""){

  # Prepare canvas:
  graphics::plot.new()
  graphics::plot.window(
    xlim = range(x$day),
    ylim = range(c(x$q025, x$q975))
  )
  graphics::grid(nx = NA, ny = NULL)
  graphics::axis(2, las = 1, col = "white", tcl = 0)
  graphics::axis(1)

  # Plot values:
  graphics::polygon(
    x = c(x$day, rev(x$day)),
    y = c(x$q025, rev(x$q975)),
    col = "#93DBDE",
    border = NA
  )
  graphics::lines(x$day, x$median, type = "l", lwd = 3, col = "#3AA1A6")

  # Annotations:
  graphics::title(
    main = NULL,
    sub = NULL,
    xlab = "Day of control phase",
    ylab = parameter
  )
  graphics::mtext(text = main, side = 3,line = 1, adj = 0)

  return(TRUE)
}
