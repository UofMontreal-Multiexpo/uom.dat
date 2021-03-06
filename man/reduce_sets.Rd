% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixer.R
\name{reduce_sets}
\alias{reduce_sets}
\title{Reduce sets according to the names of their components}
\usage{
reduce_sets(values, references = NULL, FUN, ...)
}
\arguments{
\item{values}{Numeric named vector or matrix, or list of numeric named vectors.
Sets of values to reduce.}

\item{references}{Numeric vector or list of numeric vectors. Reference values associated with the
\code{values}.}

\item{FUN}{Function to apply on each subset of values corresponding to each different name to reduce
it to a single value.}

\item{...}{Further arguments to the function \code{FUN}.}
}
\value{
If \code{references} is \code{NULL}, sets of values resulting from the application of the function given as
argument on each subset corresponding to each different name associated with the values.\cr
If not, list containing:
\describe{
\item{\code{values}}{Sets of values resulting from the application of the function given as argument
on each subset corresponding to each different name associated with the values.}
\item{\code{references}}{Subset of references corresponding to the resulting values.}
}
}
\description{
For each set of values, apply a function to merge the ones having the same names or choose one
of them, for each different name found in the set of values. This results in sets of values having
only one value per different name found in the original sets, instead of several ones.
}
\details{
If \code{values} and \code{references} are two lists, \code{references} must be a list of vectors having the same
lengths as those present in \code{values} so that \code{values} and \code{references} can be matched.

If \code{references} is not a list, it is not processed and is returned as is. Otherwise, it is assumed
that the same reference values are given for same value names in one set of values of the argument
\code{values}.

The components of each resulting set of values are ordered in the order in which the names are found
in the original set.
}
\examples{
## Reduce a set of values given as a vector
reduce_sets(values = c(A = 1, A = 3, B = 2, C = 1, C = 5, C = 4),
            FUN = max)
reduce_sets(values = c(C = 1, C = 5, C = 4, B = 2, A = 1, A = 3),
            FUN = function(x) min(x) + 0.5)

## Reduce sets of values given as a matrix
v <- matrix(sample(seq(0.1, 1, by = 0.1), 50, replace = TRUE),
            ncol = 10, dimnames = list(c("A", "B", "A", "C", "C")))
reduce_sets(values = v, FUN = mean)

## Reduce sets of values and associated reference values given as lists
reduce_sets(values = list(c(A = 0.1, A = 0.3, B = 0.5),
                          c(A = 0.2),
                          c(B = 0.3, B = 0.4, B = 0.7, C = 0.4, C = 0.1)),
            references = list(c(1, 1, 2),
                              1,
                              c(2, 2, 2, 3, 3)),
            FUN = median)

}
\author{
Gauthier Magnin
}