#' Run ADAPT Shiny App
#'
#' This function runs the Shiny App "ADAPT" to analyse 'EuFMDiS' output files.
#'
#' Upload the relevant csv output files via the "Upload files" dialog to
#' trigger the analysis.
#'
#' @returns no return value; starts a Shiny app
#' @author Ian Kopacka
#' @examples
#' if (interactive()) {
#'     run_adapt()
#' }
#' @export
#' @import shinydashboard
#' @import grDevices
#' @import htmltools
#' @import shinyWidgets
#' @import tibble
run_adapt <- function(){

  # Load app:
  dir_app <- system.file("shiny_apps", "adapt", package = "eufmdis.adapt")
  if (dir_app == "") {
    stop(
        "Could not find app directory. Try re-installing package 'eufmdis.adapt'.",
        call. = FALSE
    )
  }
  shiny::runApp(dir_app, display.mode = "normal")
}
