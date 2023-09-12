#' Plot bar chart with error bars
#'
#' Creates a bar chart with error bars using \code{ggplot2::geom_col}.
#'
#' @param x Data frame with columns \code{par} or \code{parameter} containing
#' the name of the parameter, \code{q2.5} or \code{q2_5} for the 2.5 percentiles
#' (i.e. the lower values for the error bars), \code{median} for the median
#' values (i.e. the height of the bars) and \code{q97.5} or \code{q97_5} for
#' the 97.5 percentiles (i.e. the upper values for the error bars).
#'
#' @return Returns an object of class \code{ggplot} and prints it to the
#' graphics device.
#' @importFrom magrittr %>%
#' @importFrom rlang .data

# Example:
# x <- data.frame(
#   parameter = c("A", "B", "C"),
#   q2.5 = c(1,2,3),
#   median = c(2,4,5),
#   q97.5 = c(3.5, 4.5, 7)
# )
# eufmdis.adapt:::plot_barchart(x)
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-16
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_barchart <- function(x){

  # Normalise column names:
  names(x) <- tolower(names(x))
  names(x) <- gsub("[._]", "_", names(x))
  names(x)[names(x) == "par"] <- "parameter"
  ylab <- "Samples"

  # Find right scale:
  if (max(x$median > 1e7)){
    x <-
        x %>%
        dplyr::mutate(
            median = .data$median/1e6,
            q2_5 = .data$q2_5/1e6,
            q97_5 = .data$q97_5/1e6
        )
    ylab <- "Samples (in millions)"
  } else if (max(x$median > 1e4)){
    x <-
        x %>%
        dplyr::mutate(
            median = .data$median/1e3,
            q2_5 = .data$q2_5/1e3,
            q97_5 = .data$q97_5/1e3
        )
    ylab <- "Samples (in thousands)"
  }

  # Create plot:
  p <- ggplot2::ggplot(x) +
      ggplot2::geom_col(
          ggplot2::aes(x = .data$parameter, y = .data$median),
          fill = "#3AA1A6",
          alpha = 0.5
      ) +
      ggplot2::geom_errorbar(
          ggplot2::aes(
              x = .data$parameter,
              y = .data$median,
              ymin = .data$q2_5,
              ymax = .data$q97_5
          ),
          width = 0.4,
          colour = "orange",
          alpha = 0.9,
          linewidth = 1.3
      ) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::ylab(ylab) +
      ggplot2::ggtitle(
          paste0(
              "The bars represent the median values, the error bars ",
              "represent the 95% range of variation."
          )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
          text = ggplot2::element_text(size = 16),
          plot.title = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 12)
      )
  print(p)
}


