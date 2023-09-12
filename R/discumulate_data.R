#' Inverse of Cumulative Sum
#'
#' Computes the inverse of the \code{cumsum} function
#'
#' @param value_cum numeric vector; usually the result of cumulating values.
#'
#' @return A vector of the same length as \code{value_cum}
#' @author Ian Kopacka


# Example:
# x <- rpois(10,100)
# cumulated <- cumsum(x)
# discumulated <- eufmdis.adapt:::discumulate_data(cumulated) # Should equal x
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-02-20
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

discumulate_data <- function(value_cum){
  value <- value_cum - c(0, value_cum[-length(value_cum)])
}

