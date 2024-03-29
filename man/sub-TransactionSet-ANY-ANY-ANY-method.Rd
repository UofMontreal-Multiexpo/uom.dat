% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transaction_set.R
\name{sub-TransactionSet-ANY-ANY-ANY-method}
\alias{sub-TransactionSet-ANY-ANY-ANY-method}
\alias{[,TransactionSet,ANY,ANY,ANY-method}
\alias{[,TransactionSet-method}
\alias{[<-,TransactionSet,ANY,ANY,ANY-method}
\alias{[<-,TransactionSet-method}
\alias{[[,TransactionSet-method}
\alias{[[<-,TransactionSet-method}
\alias{$,TransactionSet-method}
\alias{$<-,TransactionSet-method}
\title{Extract or replace parts of an object of class TransactionSet}
\usage{
\S4method{[}{TransactionSet,ANY,ANY,ANY}(x, i, drop = TRUE)

\S4method{[}{TransactionSet,ANY,ANY,ANY}(x, i) <- value

\S4method{[[}{TransactionSet}(x, i)

\S4method{[[}{TransactionSet}(x, i) <- value

\S4method{$}{TransactionSet}(x, name)

\S4method{$}{TransactionSet}(x, name) <- value
}
\arguments{
\item{x}{Object from which to extract element(s) or in which to replace
element(s).}

\item{i}{Numeric or character value(s). Indice(s) specifying element(s) to
extract or replace. See 'Details' section.}

\item{drop}{Only if \code{i} refers to one or more transactions. \code{TRUE} or \code{FALSE}
whether to return a list or a TransactionSet object.}

\item{value}{Value of type similar to the element(s) to be replaced.}

\item{name}{A single character value corresponding to one of the names of the
attribute \code{data} (i.e., the name of one transaction).}
}
\description{
General selectors and mutators to access the attributes of an object of class
\code{TransactionSet}.
}
\details{
Character values can be used with \code{[} to access:
\enumerate{
\item attributes of \code{x}
\item elements of the attribute \code{data} (i.e., specific transactions), or
\item sub-elements of the attribute \code{data}.
}

Numeric values can be used with all operators to access elements of the
attribute \code{data}.

\code{[[} and \code{$} can only be used to access a single element of the attribute
\code{data}, using a numeric or a character value.

Replacing elements of \code{data} do not change its names.

Replacing the attribute \code{names}:
\itemize{
\item reorders the elements contained in each transaction if \code{value} is a
reordered equivalent of \code{names};
\item renames the elements contained in each transaction if \code{value} is the same
length as \code{names} but is not equivalent. In this case, the attributes
\code{item_key} and \code{year_key} are updated (unless \code{year_key} is \code{NA});
\item removes missing elements of \code{value} in each transaction if it is smaller
than \code{names}.
}
}
\examples{
## Extracting attributes
TS_instance["data"]
TS_instance["item_key"]

## Extracting a specific element of the transactions
TS_instance["CODE"]

## Extracting specific transactions
names(TS_instance["data"])
TS_instance[["2014-B-1"]]
TS_instance[[4]]
TS_instance$`2015-D-4`
TS_instance[3:5]

## Replacing attributes or transactions
TS_instance["year_key"] <- NA
TS_instance[[3]] <- list(CODE = 1,
                         YEAR = 2000,
                         JOB.TITLE = 45454545,
                         JOB.TASK = "A3000",
                         SAMPLE.ID = c(1, 3, 5, 8))
TS_instance$`2015-D-4` <- TS_instance[[3]]
TS_instance["names"] <- c("CODE", "YEAR")
## Remove the local copy to recover the original object
rm(TS_instance)

}
\author{
Gauthier Magnin
}
