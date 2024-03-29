% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_set.R
\name{subset,TransactionSet-method}
\alias{subset,TransactionSet-method}
\alias{subset}
\title{Subsetting Transaction Set}
\usage{
\S4method{subset}{TransactionSet}(x, indices, keep_names = TRUE)
}
\arguments{
\item{x}{S4 object of class \code{TransactionSet} to be subsetted.}

\item{indices}{Numeric vector indicating which transactions from \code{x["data"]} to keep,
or logical vector indicating for each transaction whether to keep it.}

\item{keep_names}{If \code{TRUE}, transactions of the returned object keep the names from the initial
object \code{x}. If \code{FALSE}, they are not named.}
}
\value{
S4 object of class \code{TransactionSet} having a subset of transactions from \code{x} (i.e. having
a subset of the attribute \code{data} from \code{x}).
}
\description{
Return a \code{TransactionSet} in which transactions are a subset of those of another \code{TransactionSet}.
}
\details{
If the transactions from \code{x} are not named and \code{keep_names = TRUE}, the transactions of the resulting
object are named according to the indices of the initial transactions.
}
\examples{
subset(TS_instance, c(1, 7, 8, 9))
subset(TS_instance, c(TRUE, rep(FALSE, 12), TRUE))

}
\seealso{
\code{\link{reorder}}, \code{\link{remove_items}}, \code{\link{remove_empty_trx}}.
}
\author{
Gauthier Magnin
}
