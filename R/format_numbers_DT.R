#' Safe wrapper for DT::formatCurrency
#
#' Wrapper for \code{DT::formatCurrency} that returns NULL when the input table
#' is \code{NULL} (instead of throwing an error).
#'
#' @param x A table object created from \code{DT::datatable()}
#' @param ... other arguments passed to \code{DT::formatCurrency}
#'
#' @return Behaves the same output as \code{DT::formatCurrency} except when
#' \code{x} is \code{NULL}. Then \code{NULL} is returned and no error is thrown.
#' @author Ian Kopacka


# Example:
# eufmdis.adapt:::format_numbers_DT(
#   DT::datatable(data.frame(a = 1:10)),
#   "a",
#   currency = '',
#   digits = 2
# )
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-20
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
format_numbers_DT <- function(x, ...){

  if (is.null(x$x)){
    return(NULL)
  } else {
    return(DT::formatCurrency(x, ...))
  }

}
