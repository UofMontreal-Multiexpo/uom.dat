% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_set.R
\name{itemset_chart,TransactionSet-method}
\alias{itemset_chart,TransactionSet-method}
\alias{itemset_chart}
\alias{itemset_chart,TransactionSet}
\title{Itemset chart, for TransactionSet}
\usage{
\S4method{itemset_chart}{TransactionSet}(
  object,
  identifiers = "original",
  length_one = TRUE,
  jitter = TRUE,
  under = "ID",
  over = NULL,
  title = "Transaction itemsets",
  path = NULL,
  name = NULL
)
}
\arguments{
\item{object}{S4 object of class \code{TransactionSet}.}

\item{identifiers}{Which IDs to use to identify the transactions on the chart and in the
return object. One of \code{"original"}, \code{"new"}.
\describe{
\item{\code{"original"}}{Use of the original identifiers.}
\item{\code{"new"}}{Use of new identifiers based on sorting (see 'Details' section to learn more
about the sort that is performed).}
}}

\item{length_one}{If \code{FALSE}, itemsets of length \eqn{1} are not plotted. If \code{TRUE}, all itemsets
are plotted.}

\item{jitter}{If \code{FALSE}, non-equivalent itemsets of length \eqn{1} are aligned vertically.
If \code{TRUE}, they are spread over several vertical lines to avoid overplotting while taking as little
space as possible. If \code{NA}, they are plotted one after the other.
Ignored if \code{length_one} is \code{FALSE}.}

\item{under, over}{Data to display on the chart under and over the itemsets.
Identifiers (\code{"ID"}) or one of the elements of the transactions (i.e. one of the values of
\code{object["names"]}).}

\item{title}{Chart title.}

\item{path}{Path of the directory in which to save the chart as a PDF file. Default is the working
directory.}

\item{name}{Name of the PDF file in which to save the chart. To be ignored to plot the chart in the
active device.}
}
\value{
S4 object of class \code{TransactionSet} containing the transactions represented on the chart.
}
\description{
Plot a chart of the transaction itemsets. It can be automatically saved as a PDF file.
}
\details{
Itemsets are sorted according to their lengths then (for the same length) are taken according to
the initial order in \code{object}. Items are sorted alphanumerically.

If the argument \code{name} is not \code{NULL}, the chart is plotted in a PDF file of A4 landscape paper size.
If it is \code{NULL}, the chart is plotted in the active device.
}
\examples{
itemset_chart(TS_instance)
itemset_chart(TS_instance, identifiers = "new", under = "YEAR", over = "ID")

itemset_chart(TS_instance, path = getwd(), name = "trx_itemsets")

}
\references{
Bosson-Rieutort D, Sarazin P, Bicout DJ, Ho V, Lavoué J (2020).
Occupational Co-exposures to Multiple Chemical Agents from Workplace Measurements by the US Occupational Safety and Health Administration.
\emph{Annals of Work Exposures and Health}, Volume 64, Issue 4, May 2020, Pages 402–415.
\url{https://doi.org/10.1093/annweh/wxaa008}.
}
\seealso{
\code{\link{get_itemsets}}, \code{\link{get_all_items}}.

Method for signature \code{TransactionAnalyzer}:
\code{\link[=itemset_chart,TransactionAnalyzer-method]{itemset_chart,TransactionAnalyzer}}.
}
\author{
Delphine Bosson-Rieutort, Gauthier Magnin
}
