#' Import data from csv file in Shiny App
#'
#' The function is used in the ADAPT app to import data from uploaded csv files
#' into a data frame. Only relevant columns are returned, the column names are
#' matched and unified, so that data produced by different versions of the
#' EuFMDiS software can be used.
#'
#' @param pattern Character string containing a regular expression to identify
#' the correct file by its name.
#' @param names_files Character vector of file names as they were uploaded (=
#' file name on the original file system from which they were uploaded)
#' @param paths_files Character vector of file names + absolute paths of the
#' files in the local hard drive to which they were uploaded. Each entry of
#' \code{paths_files} corresponds to an entry of \code{names_files}. They must
#' have the same length.
#' @param def_columns Data frame of meta information containing the possible
#' column names in the different versions of the EuFMDiS output files. The data
#' frame must contain columns \code{Datensatz} (name of the relevant data frame;
#' this corresponds to values used in \code{pattern}), \code{Name_Parameter}
#' (the unified column name in the generated return value) and columns
#' containing possible variations of the name in the different versions of
#' EuFMDiS. Each column contains the notation in one version of EuFMDiS; the
#' column names must begin with the string \code{Spalte}
#' @param transpose Logical flag (default = FALSE). COntrols whether the data
#' frame should be transposed prior to any data manipulation/extraction.
#'
#' @return Data frame containing the columns defined in \code{def_columns} for
#' the relevant \code{Datensatz} according to the argument \code{pattern}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data

# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 14.03.2023
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

import_data_file <- function(
    pattern,
    names_files,
    paths_files,
    def_columns,
    transpose = FALSE
){

  # Identify correct file:
  rel_ind <- grep(
    pattern = pattern,
    x = names_files
  )

  # Only return a non-NULL value if exactly one file is identified:
  if (length(rel_ind) == 1){

    # Import data:
    x <- utils::read.csv(paths_files[rel_ind])

    # Transpose data if necessary:
    if (transpose == TRUE){
      x <- x[grepl("\\S", x[,1]), ]
      col_names <- gsub("\\s|-|:", "\\.", x[, 1])
      values <- t(x[, -1, drop = FALSE])
      x <- as.data.frame(values) 
      names(x) <- col_names
    }

    # Sanitize column names:
    names(x) <- cleanup_names(names(x))

    # Extract relevant entries in the metadata:
    cols_spalten <- grep(
      "^Spalte",
      names(def_columns),
      value = TRUE
    )
    rel_col_def <-
      def_columns %>%
      dplyr::filter(.data$Datensatz == pattern) %>%
      dplyr::select(
        dplyr::all_of(
          c("Datensatz", "Name_Parameter",  cols_spalten)
        )
      ) %>%
      unique()
    if (nrow(rel_col_def) > 0){
      for (i in 1:nrow(rel_col_def)){
        name_out <- rel_col_def$Name_Parameter[i]
        namen_in <-
          rel_col_def[i, cols_spalten] %>%
          unlist() %>%
          unique()
        names(x)[names(x) %in% namen_in] <- name_out
      }
    }
    x
  } else {
    NULL
  }
}



