% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_set.R
\name{get_info_from_items,TransactionSet-method}
\alias{get_info_from_items,TransactionSet-method}
\alias{get_info_from_items}
\title{Search for information by item}
\usage{
\S4method{get_info_from_items}{TransactionSet}(object, items, info_names, presence = "all")
}
\arguments{
\item{object}{S4 object of class \code{TransactionSet}.}

\item{items}{Sought items.}

\item{info_names}{Names of information to extract from transactions.}

\item{presence}{Item presence condition for information to be extracted from a transaction.
One of \code{"all"}, \code{"any"}, \code{"exactly"}, \code{"only"}.
\describe{
\item{\code{"all"}}{All the sought items must be part of a transaction for its information to be
extracted.}
\item{\code{"any"}}{At least one of the sought items must be part of a transaction for its information
to be extracted.}
\item{\code{"exactly"}}{The item set contained in a transaction must be exactly the same as the sought
item set for its information to be extracted.}
\item{\code{"only"}}{A transaction must contain only the sought items (any of them) for its
information to be extracted.}
}}
}
\value{
Vector or list of information corresponding to the search.
Vector if only one type of information is to be extracted. List otherwise.
}
\description{
Retrieve information associated with transactions that contain a set of sought items.
}
\examples{
get_info_from_items(TS_instance,
                    items = 3146,
                    info_names = "SAMPLE.ID")

get_info_from_items(TS_instance,
                    items = c(19, 25),
                    info_names = c("JOB.TITLE", "JOB.TASK"),
                    presence = "any")
get_info_from_items(TS_instance,
                    items = c(19, 25),
                    info_names = c("JOB.TITLE", "JOB.TASK"),
                    presence = "exactly")

get_info_from_items(TS_instance,
                    items = c(192, 3146),
                    info_names = c("JOB.TITLE", "JOB.TASK"),
                    presence = "any")
get_info_from_items(TS_instance,
                    items = c(192, 3146),
                    info_names = c("JOB.TITLE", "JOB.TASK"),
                    presence = "only")

}
\seealso{
\code{\link{get_items_from_info}}, \code{\link{get_all_items}}.
}
\author{
Gauthier Magnin
}
