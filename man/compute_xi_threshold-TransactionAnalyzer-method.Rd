% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_analyzer.R
\name{compute_xi_threshold,TransactionAnalyzer-method}
\alias{compute_xi_threshold,TransactionAnalyzer-method}
\alias{compute_xi_threshold}
\title{Computation of threshold xi}
\usage{
\S4method{compute_xi_threshold}{TransactionAnalyzer}(object, reporting_indexes)
}
\arguments{
\item{object}{S4 object of class \code{TransactionAnalyzer}.}

\item{reporting_indexes}{Reporting indexes associated with the patterns.}
}
\value{
Computed threshold.
}
\description{
Compute the number of patterns allowing to explain the main part of the reporting indexes.
}
\details{
\loadmathjax
The threshold \mjseqn{\xi} is given by:
 \mjdeqn{\xi = \left\lceil \frac{1}{\sum_{p \in P} RI_p(t_1,t_0)^2} \right\rceil}{xi = ceiling(1 / sum(RI_p(t_1,t_0)^2) for p in P)}
where \mjseqn{RI_p(t_1,t_0)} is the reporting index of the pattern \mjseqn{p} given by:
 \mjdeqn{RI_p(t_1,t_0) = \frac{\sum_{t = t_0}^{t_1} F_{p,t}}{\sum_{q \in P} \sum_{t = t_0}^{t_1} F_{q,t}}}{RI_p(t_1,t_0) = sum F_pt from t = t_0 to t_1 / sum F_qt for q in P and from t = t_0 to t_1}
where \mjseqn{P} is the set of patterns, \mjeqn{F_{p,t}}{F_pt} is the frequency of the pattern
 \mjseqn{p} in the transactions of the year \mjseqn{t}, \mjseqn{t_0} and \mjseqn{t_1} are the first and
 last years defining the period on which to compute the reporting index.
}
\note{
This method is not exported from the package.
}
\references{
Bosson-Rieutort D, de Gaudemaris R, Bicout DJ (2018).
            The spectrosome of occupational health problems. \emph{PLoS ONE} 13(1): e0190196.
            \url{https://doi.org/10.1371/journal.pone.0190196}.
}
\seealso{
\code{\link{compute_reporting_indexes}}, \code{\link{compute_reporting_indexes_limits}},
         \code{\link{compute_ri_threshold}}.
}
\author{
Gauthier Magnin
}
\keyword{internal}
