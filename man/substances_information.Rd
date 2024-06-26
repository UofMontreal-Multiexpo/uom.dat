% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{substances_information}
\alias{substances_information}
\title{Information about substances}
\format{
A data frame with 1,151 rows and 6 columns:
\describe{
\item{\code{CODE}}{\emph{Character}. Unique identifier of a substance.}
\item{\code{NAME}}{\emph{Character}. Name of the substance corresponding to the
code.}
\item{\code{FAMILY}}{\emph{Character}. Family the substance belongs to.}
\item{\code{SUBFAMILY}}{\emph{Character}. Subfamily the substance belongs to.}
\item{\code{TOXICITY}}{\emph{List}. Toxicity classes associated with the substance.}
\item{\code{LIMIT}}{\emph{Numeric}. Exposure limit value.}
}
}
\source{
Data from the \href{http://en.inrs.fr/}{INRS}.
}
\usage{
substances_information
}
\description{
Information about substances such as family of substances, toxicity
classes...
}
\keyword{datasets}
