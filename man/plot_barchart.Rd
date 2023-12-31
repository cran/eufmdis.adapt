% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_barchart.R
\name{plot_barchart}
\alias{plot_barchart}
\title{Plot bar chart with error bars}
\usage{
plot_barchart(x)
}
\arguments{
\item{x}{Data frame with columns \code{par} or \code{parameter} containing
the name of the parameter, \code{q2.5} or \code{q2_5} for the 2.5 percentiles
(i.e. the lower values for the error bars), \code{median} for the median
values (i.e. the height of the bars) and \code{q97.5} or \code{q97_5} for
the 97.5 percentiles (i.e. the upper values for the error bars).}
}
\value{
Returns an object of class \code{ggplot} and prints it to the
graphics device.
}
\description{
Creates a bar chart with error bars using \code{ggplot2::geom_col}.
}
