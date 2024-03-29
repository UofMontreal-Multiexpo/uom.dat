% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixer.R
\name{classify_mixture}
\alias{classify_mixture}
\title{Classify mixture into the four MIAT groups}
\usage{
classify_mixture(values, references)
classify_mixture(hi, mhq)
classify_mixture(hi, mhq, mcr)
}
\arguments{
\item{values}{Numeric vector or matrix. Values of the mixture(s) to classify.}

\item{references}{Numeric vector. Reference values associated with the \code{values}.}

\item{hi}{Numeric value or vector. \strong{H}azard \strong{i}ndex or indices of the mixture(s) to classify.}

\item{mhq}{Numeric value or vector. \strong{M}aximum \strong{h}azard \strong{q}uotient(s) associated with the hazard
index or indices \code{hi}.}

\item{mcr}{Numeric value or vector. \strong{M}aximum \strong{c}umulative \strong{r}atio(s) associated with \code{hi} and
\code{mhq}.}
}
\value{
Character value or vector (according to \code{values} or \code{hi} and \code{mhq}) of the groups assigned to
the mixture(s).
}
\description{
Classify mixtures into four groups according to the CEFIC-MIAT (Mixtures Industry Ad-hoc Team)
decision tree, each one requiring a different risk management strategy. The groups describe the
following situations:
\itemize{
\item Group I: the mixture presents a potential risk already based on individual components.
\item Group II: the assessment does not identify a concern.
\item Group IIIA: the majority of the risk offered by the mixture is driven by one component.
\item Group IIIB: the potential risk is driven by multiple components.
}
}
\details{
Arguments \code{values} and \code{references} are used to compute the hazard quotients and the hazard indices
before searching for the maximum hazard quotients, computing the maximum cumulative ratios then
classifying the mixtures. Thus, call the function with the arguments \code{hi} and \code{mhq} is faster and
call it with the argument \code{mcr} is even faster (if they are already computed).

If \code{values} is a matrix, the reference values are applied once on each set of values, i.e. on each row.
Therefore, there must be one reference value for each column of the matrix.

If \code{values} is a matrix (or \code{hi}, \code{mhq} and \code{mcr} are vectors larger than 1), one class is assigned
for each row (or each value, respectively).

\loadmathjax

The mixtures are assigned to the groups according the following conditions:
\itemize{
\item Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
\item Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
\item Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
\item Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
}

The maximum cumulative ratio of the vector \eqn{i} is given by:
\mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
\cr
In the specific case where all values of the vector (or the corresponding \eqn{HI} and \eqn{MHQ})
are equal to \eqn{0}, the resulting \eqn{MCR} is \eqn{0}.

The hazard index of the vector \eqn{i} is given by:
\mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.

The maximum hazard quotient of the vector \eqn{i} is given by:
\mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.

The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
\mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
where \eqn{V} denotes the \code{values} and \eqn{RV} denotes the \code{references}.
}
\note{
Due to the multiple possible usages, the arguments \code{hi}, \code{mhq} and \code{mcr} must be explicitly named in
the function call.
}
\examples{
classify_mixture(c(1,2,3,4,5), c(1,2,3,4,5))
classify_mixture(hi = hazard_index(c(1,2,3,0.5), c(1,2,3,0.5)),
                 mhq = maximum_hazard_quotient(c(1,2,3,0.5), c(1,2,3,0.5)))

classify_mixture(
  values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
                  nrow = 3, byrow = TRUE,
                  dimnames = list(c("set.1", "set.2", "set.3"), LETTERS[1:4])),
  references = c(1,2,3,0.5)
)

}
\references{
Reyes JM, Price PS (2018).
An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
\emph{Environment International}, 112, 77-84.
\url{https://doi.org/10.1016/j.envint.2017.12.008}.

De Brouwere K, et al. (2014).
Application of the maximum cumulative ratio (MCR) as a screening tool for the evaluation of mixtures in residential indoor air.
\emph{The Science of the Total Environment}, 479-480, 267-276.
\url{https://doi.org/10.1016/j.scitotenv.2014.01.083}.
}
\seealso{
\code{\link{hazard_index}}, \code{\link{maximum_hazard_quotient}}, \code{\link{maximum_cumulative_ratio}}.
}
\author{
Gauthier Magnin
}
