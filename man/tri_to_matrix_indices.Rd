% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tri_to_matrix_indices}
\alias{tri_to_matrix_indices}
\title{Turn triangular matrix indices into row and column indices}
\usage{
tri_to_matrix_indices(indices)
}
\arguments{
\item{indices}{Indices in a row-by-row flattened lower triangle of a matrix,
including its diagonal.}
}
\value{
Two-column matrix containing the row and column indices corresponding
to \code{indices} in a matrix.
}
\description{
Turn indices of a vector into indices of a matrix.
Vector elements match those of the lower triangle of a matrix considered
row-by-row.
}
\details{
Given indices correspond to the following elements in a matrix:\cr
\code{1}\cr
\verb{2  3}\cr
\verb{4  5  6}\cr
\verb{7  8  9  10}\cr
\verb{11 12 13 14 15}\cr
etc.
}
\note{
This function is not exported from the package.
}
\author{
Gauthier Magnin
}
\keyword{internal}
