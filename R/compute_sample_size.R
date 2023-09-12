#' Compute sample size for freedom from disease
#'
#' Compute sample size for a one stage freedom from disease survey for given
#' Population size, design prevalence and accuracy, assuming a perfect
#' diagnostic test.
#'
#' The function finds the optimal sample size using a bisection method.
#'
#' @param N Integer containing the Size of the population
#' @param prev Numeric between 0 and 1; design prevalence
#' @param accuracy Numeric between 0 and 1; accuracy of the survey (i.e.
#' detection probability)
#'
#' @return Sample size (integer).
#' @author Ian Kopacka

# Example:
# eufmdis.adapt:::compute_sample_size(N = 1000, prev = 0.01, accuracy = 0.95)
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-16
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
compute_sample_size <- function(N, prev, accuracy){

  if (accuracy <= 0) return(0)

  n_dis <- max(round(N * prev), 1)
  n_lower <- 1
  n_upper <- N
  prob_lower_upper <- sapply(
      c(n_lower, n_upper),
      function(sample_size){
        compute_p_value(
            N = N,
            n = sample_size,
            n_dis = n_dis
        )
      }
  )
  if (prob_lower_upper[2] > (1 - accuracy)) return(Inf)
  if (prob_lower_upper[1] <= (1 - accuracy)) return(n_lower)
  interval_width_min <- 4
  while (n_upper - n_lower > interval_width_min) {
    n_mid <- round((n_lower + n_upper)/2)
    prob_mid <- compute_p_value(
        N = N,
        n = n_mid,
        n_dis = n_dis
    )
    if (prob_mid > (1 - accuracy)) {
      n_lower <- n_mid
    } else {
      n_upper <- n_mid
    }
  }
  n_vector <- n_lower:n_upper
  prob_vector <- sapply(
      n_vector,
      function(n_sample){
        compute_p_value(
            N = N,
            n = n_sample,
            n_dis = n_dis
        )
      }
  )
  index_optimal <- which(prob_vector <= (1 - accuracy))
  if (length(index_optimal) > 0) {
    out <- n_vector[min(index_optimal)]
  } else {
    out <- n_upper
  }
  return(out)
}

