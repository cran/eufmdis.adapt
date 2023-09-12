#' Plot the distribution of a variable
#'
#' Creates a histogram of the value along with a horizontal boxplot above it
#' to show the distribution of a variable.
#'
#' @param x A numeric vector
#' @param parameter Character to use as label of the x-axis
#' @param main (optional) character to use as plot title
#'
#' @return No return value. Creates a plot.
#' @author Ian Kopacka


# Plot the distribution of a variable as a histogram + boxplot
# Example:
# eufmdis.adapt:::plot_distribution(rnorm(100), "height", "This is a nice plot")
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-02-24
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_distribution <- function(x, parameter, main = ""){

  # Reset users' par-settings on exit:
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  if (length(x) > 0){

    # Scale Values:
    if (stats::median(x) > 1e7){
      x <- x/1e6
      parameter <- paste(parameter, "(in millions)")
    } else if (stats::median(x) > 1e4){
      x <- x/1e3
      parameter <- paste(parameter, "(in thousands)")
    }

    # Layout to split the screen
    graphics::layout(
      mat = matrix(c(1,2), 2, 1, byrow = TRUE),
      height = c(2,8)
    )

    # Draw the boxplot and the histogram
    graphics::par(mar = c(0, 4.1, 2, 2.1))
    graphics::boxplot(
      x,
      horizontal = TRUE,
      xaxt = "n",
      col = "#3AA1A6",
      frame = F,
      ylim = range(x)
    )
    graphics::mtext(text = main, adj = 0)
    graphics::par(mar = c(5, 4.1, 1.1, 2.1))
    graphics::hist(
      x,
      breaks = 31,
      col = "#93DBDE",
      border = "#3AA1A6",
      main = "",
      xlab = parameter,
      ylab = "Number of simulation runs",
      xlim = range(x),
      las = 1
    )
  } else {
    graphics::par(mar = c(5, 3.1, 1, 2.1))
    graphics::plot.new()
    graphics::plot.window(xlim = c(-1,1), ylim = c(0,1), asp = 1)
    graphics::axis(1)
    graphics::axis(2, at = c(0,1), las = 1)
    graphics::mtext(text = main, adj = 0)
    graphics::title(xlab = parameter)
  }
  return(TRUE)
}

