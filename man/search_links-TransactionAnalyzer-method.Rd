% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_analyzer.R
\name{search_links,TransactionAnalyzer-method}
\alias{search_links,TransactionAnalyzer-method}
\alias{search_links}
\title{Elaboration of links}
\usage{
\S4method{search_links}{TransactionAnalyzer}(object, entities)
}
\arguments{
\item{object}{S4 object of class \code{TransactionAnalyzer}.}

\item{entities}{Type of entities for which to elaborate links (nodes or patterns).
\code{NODES} or \code{PATTERNS}.}
}
\value{
Invisible. Data frame detailing the links between pairs of nodes or patterns.
 Isolated nodes or patterns (i.e. unrelated to any other entity) appear at the bottom of the data
 frame.
}
\description{
Identify the links according to items in common between nodes or patterns.
The resulting data frame is assigned respectively to the attribute \code{node_links} or
 \code{pattern_links} of \code{object}.
}
\note{
This method is not exported from the package.
}
\author{
Delphine Bosson-Rieutort, Gauthier Magnin
}
\keyword{internal}
