#' Reshape wide data frame with combined column names
#'
#' The function identifies columns whose name contains a combination of two
#' categorical characteristics (e.g. farm type and output parameter), splits
#' them up and reshapes the data to a long format.
#'
#' The function looks for combined columns based on the category names provided
#' in the argument \code{categories}. Two modes of searching are possible:
#' \code{starts_with = FALSE} (=default) looks for all columns whose name
#' contains the strings in \code{categories}, whereas \code{starts_with = TRUE}
#' only includes columns whose name starts with the string. Relevant
#' combinations of #' values that are not found in the wide data frame are
#' filled with \code{NA} in the ling data frame.
#'
#' @param dat Data frame with combined column names (e.g. \code{type_A_farms},
#' \code{type_B_farms}, \code{type_A_animals}, \code{type_B_animals})
#' @param categories Character vector of possible values of categories in the
#' column names (e.g. \code{c("type_A", "type_B")})
#' @param name_categories Character; name of the newly created column that
#' contains the categories in the long data frame
#' @param starts_with Logical; Flag indicating how the combined columns should
#' be identified. \code{starts_with = TRUE} enforces a stricter search mode
#' where only columns are considered whose name starts with the given string.
#'
#' @return A long data frame where the combined columns have been split up
#' @author Ian Kopacka

# Example:
# wide_data <- data.frame(
#   run = 1:5,
#   type_A_farms = rpois(5, 10),
#   type_B_farms = rpois(5, 10),
#   type_A_animals = rpois(5, 100),
#   type_B_animals = rpois(5, 100),
#   type_C_animals = rpois(5, 100)
# )
# long_data <- eufmdis.adapt:::create_long_data_frame(
#   dat = wide_data,
#   categories = c("tape_A", "type_B", "type_C"),
#   name_categories = "type"
# )
#
# Author: Ian Kopacka
# Encoding: UTF-8
# Version: 2023-02-20
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

create_long_data_frame <- function(
    dat,
    categories,
    name_categories,
    starts_with = FALSE
){

  # Loose search mode: all columns whose name contains the strings in
  # categories are considered:
  if (!starts_with){
    # Identify which categories are contained in the data. Consider only these.
    is_cat_in_data <-
      vapply(
        paste0(categories, "_[[:alpha:]]*$"),
        function(y){
          any(grepl(pattern = y, x = names(dat)))
        },
        logical(1)
      )
    cat_in_data <- categories[is_cat_in_data]

    # Loop over categories, identify combined columns, rename columns,
    # create column with category information. In the end bind rows to create
    # long data frame.
    out_long <-
      lapply(
        cat_in_data,
        function(x){
          out <- dplyr::select(
            dat,
            tidyselect::all_of("run"),
            tidyselect::matches(x)
          )
          names(out) <- sub(
            pattern = x,
            replacement = "",
            x = names(out),
            fixed = TRUE
          )
          names(out) <- cleanup_names(names(out))
          out$temp <- x
          return(out)
        }
      )
    out_long <- do.call(dplyr::bind_rows, out_long)

    # Stricts search mode: all columns whose name starts withthe strings in
    # categories are considered:
  } else {
    # Identify which categories are contained in the data. Consider only these.
    is_cat_in_data <-
      vapply(
        paste0("^", categories),
        function(y){
          any(grepl(pattern = y, x = names(dat)))
        },
        logical(1)
      )
    cat_in_data <- categories[is_cat_in_data]

    # Loop over categories, identify combined columns, rename columns,
    # create column with category information. In the end bind rows to create
    # long data frame.
    out_long <-
      lapply(
        categories,
        function(x){
          out <- dplyr::select(
            dat,
            tidyselect::all_of("run"),
            tidyselect::starts_with(x)
          )
          names(out) <- sub(
            pattern = x,
            replacement = "",
            x = names(out),
            fixed = TRUE
          )
          names(out) <- cleanup_names(names(out))
          out$temp <- x
          return(out)
        }
      )
    out_long <- do.call(dplyr::bind_rows, out_long)

  }
  names(out_long)[names(out_long) == "temp"] <- name_categories
  return(out_long)
}
