% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixer.R
\name{hazard_index}
\alias{hazard_index}
\title{Hazard Index (HI)}
\usage{
hazard_index(values, references)
hazard_index(hq)
}
\arguments{
\item{values}{Numeric vector or matrix. Values for which the hazard index is to be computed.}

\item{references}{Numeric vector. Reference values associated with the \code{values}.}

\item{hq}{Numeric vector or matrix. \strong{H}azard \strong{q}uotients on which to compute the hazard index
or indices.}
}
\value{
Numeric value or vector (according to \code{values} or \code{hq}) of the computed hazard index or
indices.
}
\description{
Compute the hazard index as the sum of hazard quotients (i.e. the sum of ratios between values and
reference values).
}
\details{
Arguments \code{values} and \code{references} are used to compute the hazard quotients before computing the
hazard index. Thus, call the function with the argument \code{hq} is faster (if it is already computed).

If \code{values} is a matrix, the reference values are applied once on each set of values, i.e. on each row.
Therefore, there must be one reference value for each column of the matrix.

If \code{values} or \code{hq} is a matrix, one hazard index is computed for each row.

\loadmathjax

The hazard index of the vector \eqn{i} is given by:
\mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.

The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
\mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
where \eqn{V} denotes the \code{values} and \eqn{RV} denotes the \code{references}.
}
\note{
Due to the multiple possible usages, the argument \code{hq} must be explicitly named in the function call.
}
\examples{
hazard_index(c(1,2,3,4,5), c(1,2,3,4,5))
hazard_index(hq = hazard_quotient(c(1,2,3,4,5), c(1,2,3,4,5)))

hazard_index(
  values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                  nrow = 2, byrow = TRUE,
                  dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
  references = c(1,2,3,4,5)
)

}
\references{
Price PS, Han X (2011).
Maximum cumulative ratio (MCR) as a tool for assessing the value of performing a cumulative risk assessment.
\emph{International Journal of Environmental Research and Public Health}. 8(6): 2212-2225.
\url{https://doi.org/10.3390/ijerph8062212}.
}
\seealso{
\code{\link{hazard_quotient}}.
}
\author{
Gauthier Magnin
}
