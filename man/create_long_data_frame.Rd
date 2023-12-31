% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_long_data_frame.R
\name{create_long_data_frame}
\alias{create_long_data_frame}
\title{Reshape wide data frame with combined column names}
\usage{
create_long_data_frame(dat, categories, name_categories, starts_with = FALSE)
}
\arguments{
\item{dat}{Data frame with combined column names (e.g. \code{type_A_farms},
\code{type_B_farms}, \code{type_A_animals}, \code{type_B_animals})}

\item{categories}{Character vector of possible values of categories in the
column names (e.g. \code{c("type_A", "type_B")})}

\item{name_categories}{Character; name of the newly created column that
contains the categories in the long data frame}

\item{starts_with}{Logical; Flag indicating how the combined columns should
be identified. \code{starts_with = TRUE} enforces a stricter search mode
where only columns are considered whose name starts with the given string.}
}
\value{
A long data frame where the combined columns have been split up
}
\description{
The function identifies columns whose name contains a combination of two
categorical characteristics (e.g. farm type and output parameter), splits
them up and reshapes the data to a long format.
}
\details{
The function looks for combined columns based on the category names provided
in the argument \code{categories}. Two modes of searching are possible:
\code{starts_with = FALSE} (=default) looks for all columns whose name
contains the strings in \code{categories}, whereas \code{starts_with = TRUE}
only includes columns whose name starts with the string. Relevant
combinations of #' values that are not found in the wide data frame are
filled with \code{NA} in the ling data frame.
}
\author{
Ian Kopacka
}
