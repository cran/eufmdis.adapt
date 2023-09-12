# Version 05
# 2023-03-20

# To run the app locally and not using the package
# e.g. when running on a server where you do not want to install
# the package, do the following:
# - copy the folder "R" into the local app folder
# - copy the folder "ext" (in inst) into the local app folder
# - copy everything from the folder inst/shiny_apps_adapt into the local app folder
# - set the parametetr "location" in globals.R to "server" 
#
# The folder should then look like this:
# +---[ext]
# +---[R]
# +---[www]
# +---global.R
# +---server.R
# \---ui.R
location <- c("package", "server")[1]

# FOR DEBUGGING ================================================================

if (location == "server"){
  # Log errors and warnings to a text file:
  if (Sys.info()["sysname"] == "Linux") {
    zz <- file("messages.txt", open="wt")
    sink(zz, type = "message")
  }
}


# LOAD PACKAGES ================================================================

library(grDevices)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(tidyselect)
library(htmltools)
library(tibble)
library(shinyWidgets)
library(ggplot2)
library(DT)
options(shiny.maxRequestSize = 120*1024^2)


# LADE FUNCTIONS ==============================================================

if (location == "package"){
  # Import external functions:
  check_availability <- eufmdis.adapt:::check_availability
  cleanup_names <- eufmdis.adapt:::cleanup_names
  compute_p_value <- eufmdis.adapt:::compute_p_value
  compute_sample_size <- eufmdis.adapt:::compute_sample_size
  compute_sample_size_vectorised <- eufmdis.adapt:::compute_sample_size_vectorised
  create_diag_control <- eufmdis.adapt:::create_diag_control
  create_long_data_frame <- eufmdis.adapt:::create_long_data_frame
  discumulate_data <- eufmdis.adapt:::discumulate_data
  format_numbers_DT <- eufmdis.adapt:::format_numbers_DT
  import_data_file <- eufmdis.adapt:::import_data_file
  plot_barchart <- eufmdis.adapt:::plot_barchart
  plot_barchart_euros <- eufmdis.adapt:::plot_barchart_euros
  plot_distribution <- eufmdis.adapt:::plot_distribution
  plot_time_series <- eufmdis.adapt:::plot_time_series
} else {
  folder_functions_global <- file.path("R")
  list.files(
    folder_functions_global,
    full.names = TRUE,
    pattern = "\\.[Rr]$"
    ) %>%
    purrr::walk(source)
}


# LOAD METADATA ================================================================

if (location == "package"){
  folder_meta <- system.file("ext", package = "eufmdis.adapt")
} else {
  folder_meta <- "ext"
}

def_columns <- read.csv2(
  file.path(
    folder_meta,
    "names_columns_for_import.csv"
  )
)

names_farm_types <- read.csv2(
  file.path(
    folder_meta,
    "names_farm_types.csv"
  )
)

parameters_epi_general <- read.csv2(
  file.path(
    folder_meta,
    "parameters_epi_general.csv"
  )
)

parameters_epi_types <- read.csv2(
  file.path(
    folder_meta,
    "parameters_epi_types.csv"
  )
)

parameters_epi_line_plots <- read.csv2(
  file.path(
    folder_meta,
    "parameters_epi_line_plots.csv"
  )
)

parameters_economy <- read.csv2(
  file.path(
    folder_meta,
    "parameters_economy.csv"
  )
)

parameters_postoutbr <- read.csv2(
  file.path(
    folder_meta,
    "parameters_postoutbr.csv"
  )
)

parameters_diag_control <- read.csv2(
  file.path(
    folder_meta,
    "parameters_diag_control.csv"
  )
)

parameters_diag_pom_surv <- read.csv2(
  file.path(
    folder_meta,
    "parameters_diag_pom_surv.csv"
  )
)

parameters_config <- read.csv2(
  file.path(
    folder_meta,
    "parameters_config.csv"
  )
)


# LADE DEFAULTWERTE ============================================================

defaults_diag_control <- read.csv2(
  file.path(
    folder_meta,
    "defaults_diag_control.csv"
  )
)

defaults_diag_pom_surv <- read.csv2(
  file.path(
    folder_meta,
    "defaults_diag_pom_surv.csv"
  )
)


# LADE HTML-INHALTE ============================================================

text_diag_control <- readLines(
  file.path(
    folder_meta,
    "text_diag_control.txt"
  )
) %>%
  paste(collapse = " ")

text_diag_pom_surv_general <- readLines(
  file.path(
    folder_meta,
    "text_diag_pom_surv_general.txt"
  )
) %>%
  paste(collapse = " ")

text_diag_pom_surv_vacc_remove <- readLines(
  file.path(
    folder_meta,
    "text_diag_pom_surv_vacc_remove.txt"
  )
) %>%
  paste(collapse = " ")

text_diag_pom_surv_vacc_retain <- readLines(
  file.path(
    folder_meta,
    "text_diag_pom_surv_vacc_retain.txt"
  )
) %>%
  paste(collapse = " ")
