#' Example of occupational exposure data
#' 
#' An illustrative dataset containing identified substances in specific situations.
#' 
#' @format A data frame with 131 rows and 8 variables:
#'  \describe{
#'    \item{ID}{Identifier of an inspection in a specific business establishment.}
#'    \item{YEAR}{Year of the inspection.}
#'    \item{CODE}{Code of the substance sampled.}
#'    \item{NAME}{Name of the substance sampled.}
#'    \item{SAMPLE.ID}{Identifier of the sample. Several samples may refer to the same inspection.}
#'    \item{ACTIVITY}{Economic activity of the business establishment inspected.}
#'    \item{JOB.TITLE}{Specific job inspected.}
#'    \item{JOB.TASK}{Specific job task inspected.}
#'  }
"oedb_sample"

#' Classification of substances by family
#' 
#' Classification associating each substance with a unique code, a family and a subfamily.
#' 
#' @format A data frame with 1,151 rows and 4 variables:
#'  \describe{
#'    \item{CODE}{Code of a substance.}
#'    \item{NAME}{Name of the substance corresponding to the code.}
#'    \item{FAMILY}{Family of the substance.}
#'    \item{SUBFAMILY}{Subfamily of the substance.}
#'  }
"substances_classification"
