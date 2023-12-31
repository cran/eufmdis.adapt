% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanup_names.R
\name{cleanup_names}
\alias{cleanup_names}
\title{Clean up a vector of column names}
\usage{
cleanup_names(x)
}
\arguments{
\item{x}{A character vector}
}
\value{
A character vector
}
\description{
The function takes a vector of column names as an argument and returns a
cleaned up version of it.
}
\details{
The following changes are made:
- names are converted to lower case
- dots are replaced by underscores
- underscores in the beginning and end of a string are removed
- multiple underscores are replaced by a single one
}
\author{
Ian Kopacka
}
