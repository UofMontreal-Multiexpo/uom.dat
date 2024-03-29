% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{standardize_nchar}
\alias{standardize_nchar}
\title{Standardize the nuber of characters in strings}
\usage{
standardize_nchar(x, at_end = FALSE)
}
\arguments{
\item{x}{Character vector containing the strings to standardize.}

\item{at_end}{\code{TRUE} or \code{FALSE} depending on whether to add spaces at the end or at the beginning
of the strings.}
}
\value{
Vector of the same length as \code{x} in which all character values have the same number
of characters.
}
\description{
Add zero, one or multiple spaces at the beginning or the end of each string so that they all have the
same number of characters.
}
\note{
This function is not exported from the package.
}
\seealso{
\code{\link{cap}}, \code{\link{pluralize}}, \code{\link{first_characters}}, \code{\link{substr2}}.
}
\author{
Gauthier Magnin
}
\keyword{internal}
