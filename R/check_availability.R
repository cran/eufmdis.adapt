#' Check if list items are empty
#'
#' The function argument is a list of data frames that are required for some
#' subsequent analysis. If any of the data frames are empty (i.e. the data
#' have not been uploaded to the app), a message is returned as HTML code
#' listing the names of the required data frames (= names of list items).
#'
#' In the ADAPT app, individual analyses can only be performed if the
#' necessary output files are uploaded. If certain files are not uploaded,
#' the app produces empty data frames. In the app, the function
#' \code{check_availability()} is used with the necessary data frames to check if they
#' have been uploaded and displays a message of the form "To generate this
#' analysis, please upload the following reports: x, y" otherwise.
#'
#' @param list_data Named list of data frames
#'
#' @return Possibly empty HTML text, listing names of required data frames.
#' @author Ian Kopacka


# Example:
# eufmdis.adapt:::check_availability(
#   list(farm_summary = NULL, herd_summary = data.frame(a = 1, b = 1))
# )
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-03-16
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
check_availability <- function(list_data){

  names_data <- names(list_data)
  flag_is_empty <- unlist(lapply(list_data, is.null))

  if (any(flag_is_empty)){
    text_out <- paste0(
      '<p class="infotext">',
      'To generate this analysis, please upload the following reports: ',
      paste(names_data, collapse = ', '),
      '.</p>'
    )
    shiny::HTML(text_out)
  } else {
    shiny::HTML('')
  }
}
