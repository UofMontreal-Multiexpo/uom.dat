% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_analyzer.R
\name{get_maximals,TransactionAnalyzer-method}
\alias{get_maximals,TransactionAnalyzer-method}
\alias{get_maximals}
\title{Search for patterns that are the maximals}
\usage{
\S4method{get_maximals}{TransactionAnalyzer}(object, pc)
}
\arguments{
\item{object}{S4 object of class \code{TransactionAnalyzer}.}

\item{pc}{Data frame of \strong{p}atterns and their \strong{c}haracteristics. Any
subset of \code{object["patterns"]}.

\code{"patterns"} and \code{"p"} are special values for \code{object["patterns"]}.}
}
\value{
Subset of the data frame of patterns that are the maximals.
}
\description{
Extract the patterns that have no superset among the other ones.
}
\note{
If the \code{target} parameter of a \code{TransactionAnalyzer} object is equal to
\code{"maximally frequent itemsets"}, all its patterns are already maximals.
}
\examples{
get_maximals(TA_instance, "patterns")
get_maximals(TA_instance, TA_instance["patterns"][1:10, ])

}
\seealso{
\code{\link{get_nodes}}, \code{\link{get_patterns}}, \code{\link{get_complexes}},
\code{\link{get_isolates}}, \code{\link{get_non_isolates}}.
}
\author{
Gauthier Magnin
}
