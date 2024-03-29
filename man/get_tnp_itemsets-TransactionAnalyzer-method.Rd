% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_analyzer.R
\name{get_tnp_itemsets,TransactionAnalyzer-method}
\alias{get_tnp_itemsets,TransactionAnalyzer-method}
\alias{get_tnp_itemsets}
\title{Get transaction, node or pattern itemsets}
\usage{
\S4method{get_tnp_itemsets}{TransactionAnalyzer}(object, tnp, entities = NODES_OR_PATTERNS)
}
\arguments{
\item{object}{S4 object of class \code{TransactionAnalyzer}.}

\item{tnp}{List of \strong{t}ransaction, \strong{n}ode or \strong{p}attern itemsets (or of any itemsets) or one of
the following character values: \code{"transactions"}, \code{"t"}, \code{"nodes"}, \code{"n"}, \code{"patterns"}, \code{"p"}.}

\item{entities}{Type of the entities that the list may refer to (\code{TRANSACTIONS}, \code{NODES}, \code{PATTERNS},
\code{NODES_OR_PATTERNS}, \code{NODES_PATTERNS_OR_TRANSACTIONS}, \code{ANY_ITEMSETS}).}
}
\value{
List of transaction, node or pattern itemsets corresponding to the arguments.
}
\description{
Find and return the list of itemsets corresponding to the transactions, the nodes or the patterns of
the object of class \code{TransactionAnalyzer}, or return the given list.
}
\details{
If \code{tnp} is a list, it is returned.

If \code{tnp} is a character value equal to:
\itemize{
\item \code{"transactions"} or \code{"t"}: \code{object["transactions"][object["transactions"]["item_key"]]} is returned.
\item \code{"nodes"} or \code{"n"}: \code{object["nodes"]$node} is returned.
\item \code{"patterns"} or \code{"p"}: \code{object["patterns"]$pattern} is returned.
}

The argument \code{entities} is only used to adapt a possible error message.
}
\note{
This method is not exported from the package.
}
\seealso{
\code{\link{get_tnp}}, \code{\link{which_entities}}, \code{\link[=get_items,TransactionAnalyzer-method]{get_items}}.
}
\author{
Gauthier Magnin
}
\keyword{internal}
