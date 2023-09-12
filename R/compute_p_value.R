#' Compute p value for freedom from disease sample
#'
#' Compute the probability of drawing no positives in a sample of n items
#' from a Population of N containing n_dis positives.
#'
#' The probability is computed using the hypergeometric distribution. This
#' function is used in \code{\link{compute_sample_size}}.
#'
#' @param N Integer; size of the population
#' @param n Integer; size of the sample
#' @param n_dis Integer; number of positives in the population
#'
#' @return Returns the probability of not finding any positives in the sample
#' as a numeric between 0 and 1
#' @author Ian Kopacka
#' @seealso \code{\link{compute_sample_size}}

# Example:
# eufmdis.adapt:::compute_p_value(N = 1000, n = 50, n_dis = 10)
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-16
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
compute_p_value <- function (N, n, n_dis){
  if (n <= (N - n_dis)){
    stats::dhyper(
        x = 0,
        m = n_dis,
        n = N - n_dis,
        k = n
    )
  } else {
    0
  }
}
