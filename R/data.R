
#' Example of occupational exposure data
#' 
#' An illustrative dataset containing identified substances in specific situations.
#' 
#' @format A data frame with 131 rows and 9 variables:
#'  \describe{
#'    \item{ID}{Identifier of an inspection in a specific business establishment.}
#'    \item{YEAR}{Year of the inspection.}
#'    \item{CODE}{Code identifying the substance sampled.}
#'    \item{NAME}{Name of the substance sampled.}
#'    \item{SAMPLE.ID}{Identifier of the sample. Several samples may refer to the same inspection.}
#'    \item{ACTIVITY}{Economic activity of the business establishment inspected.}
#'    \item{JOB.TITLE}{Specific profession inspected.}
#'    \item{JOB.TASK}{Specific job task inspected.}
#'    \item{CONCENTRATION}{Measured concentration value.}
#'  }
"oedb_sample"


#' Information about substances
#' 
#' Information about substances such as family of substances, toxicity classes...
#' 
#' @format A data frame with 1,151 rows and 6 variables:
#'  \describe{
#'    \item{CODE}{Unique identifier of a substance.}
#'    \item{NAME}{Name of the substance corresponding to the code.}
#'    \item{FAMILY}{Family the substance belongs to.}
#'    \item{SUBFAMILY}{Subfamily the substance belongs to.}
#'    \item{TOXICITY}{Toxicity classes associated with the substance.}
#'    \item{LIMIT}{Exposure limit value.}
#'  }
#' @source Data from the \href{http://en.inrs.fr/}{INRS}.
"substances_information"


#' Example of a SpectralAnalyzer object
#' 
#' An example of an object of class \code{SpectralAnalyzer}.
#' 
#' Here is the way it was created using the dataset \code{\link{oedb_sample}}:
#' \preformatted{
#' ## Making a list of observations
#' to_keep <- c("NAME", "ACTIVITY", "JOB.TITLE", "JOB.TASK", "SAMPLE.ID")
#' ws <- data.frame(WS_ID = c(1, 2, 2, 3, 3),
#'                  JOB.TITLE = c(44121004, 44142001, 44132032, 44132019, 44132030),
#'                  JOB.TASK = c("A5440", "A6410", "A5110", "A5260", "A5240"),
#'                  stringsAsFactors = FALSE)
#' ws_vars <- c("JOB.TITLE", "JOB.TASK")
#' 
#' obs <- make_INRS_observations(oedb_sample, mode = 1,
#'                               work_situations = ws,
#'                               variable_names = ws_vars,
#'                               additional = to_keep,
#'                               unique_values = TRUE)
#' 
#' ## Associating item identifiers with names and one category
#' substances <- get_all_items(obs)
#' families <- substances_information[match(substances,
#'                                          substances_information$CODE),
#'                                    "SUBFAMILY"]
#' families[is.na(families)] <- "Unknown"
#' names <- substances_information[match(substances,
#'                                       substances_information$CODE),
#'                                 "NAME"]
#' 
#' items <- data.frame(item = substances, name = names, family = families)
#' 
#' ## Creation of the SpectralAnalyzer
#' SA_instance <- spectral.analyzer(obs, items)
#' }
#' 
#' @format An object of class \code{SpectralAnalyzer} created from 14 observations, 25 items, 1 category
#'  associated to the items, and generating 12 nodes and 20 patterns by enumeration of the closed
#'  frequent itemsets.
#' 
#' For more about the attributes, see \code{\link{SpectralAnalyzer}}.
"SA_instance"


