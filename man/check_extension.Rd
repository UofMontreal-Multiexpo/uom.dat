% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_extension}
\alias{check_extension}
\title{File extension}
\usage{
check_extension(filename, ext)
}
\arguments{
\item{filename}{Character string to be considered as a filename.}

\item{ext}{Desired extension.}
}
\value{
Character string \code{filename}, ending with the extension \code{ext}.
}
\description{
Check if a filename has the desired extension.
Add this extension if it has not.
}
\note{
This function is not exported from the package.
}
\author{
Gauthier Magnin
}
\keyword{internal}
