#' Clean up a vector of column names
#'
#' The function takes a vector of column names as an argument and returns a
#' cleaned up version of it.
#'
#' The following changes are made:
#' - names are converted to lower case
#' - dots are replaced by underscores
#' - underscores in the beginning and end of a string are removed
#' - multiple underscores are replaced by a single one
#'
#' @param x A character vector
#'
#' @return A character vector
#' @author Ian Kopacka

# Example:
# eufmdis.adapt:::cleanup_names(c("Number..of.farms", "Duration__2"))
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-02-20
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cleanup_names <- function(x){
  x <- tolower(x)
  x <- gsub(pattern = "\\.+", replacement = "_", x = x)
  x <- gsub(pattern = "^_", replacement = "", x = x)
  x <- gsub(pattern = "_$", replacement = "", x = x)
  x <- gsub(pattern = "_+", replacement = "_", x = x)
  x
}

