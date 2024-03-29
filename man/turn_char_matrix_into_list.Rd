% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_manager.R
\name{turn_char_matrix_into_list}
\alias{turn_char_matrix_into_list}
\title{Turn a char matrix into a list}
\usage{
turn_char_matrix_into_list(x, indices = c(1, 2), inline = TRUE)
}
\arguments{
\item{x}{Character matrix to turn into a list.}

\item{indices}{Two-element vector indicating the names of the associated rows or columns (according
to \code{inline}) to use to create the list}

\item{inline}{\code{TRUE} if associations in \code{x} are made in line (i.e. there is one association per line).
\code{FALSE} if associations are made in column (i.e. there is one association per column).}
}
\value{
List whose names correspond to the column (or row if \code{inline = FALSE}) of \code{x}
corresponding to the first element of \code{indices} and whose values correspond to the column (row if
\code{inline = FALSE}) of \code{x} corresponding to the second element of \code{indices}.
}
\description{
Turn two columns or two lines of a chararacter matrix into a named list.
}
\note{
This function is not exported from the package.
}
\examples{
m1 <- matrix(c("e1", "e1", "e1", "e2", "e3", "e3", "e4",
               "e4", "e4", "e4", "e5", "e5", "e5", "e5", "e5",
               "E", "F", "I", "I", "C", "I", "A",
               "C", "D", "F", "B", "D", "E", "G", "I"),
             ncol = 2)

turn_char_matrix_into_list(m1)

m2 <- matrix(c("e1", "e1", "e1", "e2", "e3", "e3", "e4",
               "e4", "e4", "e4", "e5", "e5", "e5", "e5", "e5",
               "E", "F", "I", "I", "C", "I", "A",
               "C", "D", "F", "B", "D", "E", "G", "I"),
             nrow = 2, byrow = TRUE)

turn_char_matrix_into_list(m2, inline = FALSE)

}
\seealso{
\code{\link{coerce_to_list}}, \code{\link{turn_logical_matrix_into_list}}, \code{\link{turn_data_frame_into_list}},
\code{\link{turn_list_into_char_matrix}}.
}
\author{
Gauthier Magnin
}
\keyword{internal}
