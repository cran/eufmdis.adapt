#' Compute sample size for freedom from disease (vectorised)
#'
#' Compute sample size for a one stage freedom from disease survey for given
#' Population size, design prevalence and accuracy, assuming a perfect
#' diagnostic test. Vectorised version of \code{\link{compute_sample_size}}.
#'
#' Uses vapply to vectorise \code{\link{compute_sample_size}} over the
#' population size \code{N}. \code{prev} and \code{accuracy} must be scalars.
#' For the sake of efficiency, the sample size is only computed once for every
#' different value of \code{N}, even if they apper multiple times in the vector.
#'
#' @param N Integer vector containing the population sizes
#' @param prev Numeric between 0 and 1; design prevalence
#' @param accuracy Numeric between 0 and 1; accuracy of the survey (i.e.
#' detection probability)
#'
#' @return Sample size (integer vector).
#' @author Ian Kopacka
#' @seealso \code{\link{compute_sample_size}}

# Example:
# eufmdis.adapt:::compute_sample_size_vectorised(N = 1000:1010, prev = 0.01, accuracy = 0.95)
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-16
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Vektorisierte Funktion:
compute_sample_size_vectorised <- function(N, prev, accuracy){
  N_vec <- unique(N)
  n_sample_vec <- vapply(
    N_vec,
    compute_sample_size,
    numeric(1),
    prev,
    accuracy
  )
  # Return value:
  return(n_sample_vec[match(N, N_vec)])
}
