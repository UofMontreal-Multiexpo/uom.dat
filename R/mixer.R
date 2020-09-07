
#### Maximum Cumulative Ratio approach ####

#' Hazard Quotient (HQ)
#' 
#' Compute the hazard quotient as the ratio between a value and a reference value.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each column (i.e. there is one
#'  reference value for each row of the matrix).
#' 
#' \loadmathjax
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @param values Numeric vector or matrix. Values for which the hazard quotients are to be computed.
#' @param references Numeric vector. Reference values associated with the values.
#' @return Vector or matrix (according to `values`) of computed hazard quotients.
#' 
#' @author Gauthier Magnin
#' @references Price PS, Han X (2011).
#'             Maximum cumulative ratio (MCR) as a tool for assessing the value of performing a cumulative risk assessment.
#'             *International Journal of Environmental Research and Public Health*. 8(6): 2212-2225.
#'             <https://doi.org/10.3390/ijerph8062212>.
#' @seealso [`hazard_index`], [`maximum_hazard_quotient`].
#' 
#' @examples
#' hazard_quotient(c(1,2,3,4,5), c(1,2,3,4,5))
#' hazard_quotient(c(a = 1, b = 2, c = 3, d = 4, e = 5), c(1,2,3,4,5))
#' hazard_quotient(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                 ncol = 2, dimnames = list(letters[1:5])),
#'                 references = c(1,2,3,4,5))
#' 
#' @md
#' @export
hazard_quotient = function(values, references) {
  
  if (is.vector(values) && length(values) != length(references))
    stop("values and references must be the same length.")
  if (is.matrix(values) && nrow(values) != length(references))
    stop("Length of references must be equal to the number of rows of values.")
  
  # Distinction vector et matrix
  if (is.vector(values)) return(values / references)
  if (is.matrix(values)) {
    hq = apply(values, 2, function(column) column / references)
    
    # Cas où la matrice ne possède qu'une seule ligne, le résultat de apply est un vecteur
    if (is.vector(hq)) hq = matrix(hq, nrow = 1)
    
    colnames(hq) = paste0("V", seq_len(ncol(values)))
    return(hq)
  }
}


#' Hazard Index (HI)
#' 
#' Compute the hazard index as the sum of hazard quotients (i.e. the sum of ratios between values and
#'  reference values).
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each column to compute the hazard
#'  quotients (i.e. it must have one reference value for each row of the matrix) before computing the
#'  hazard index. Thus, call the function with the argument `hq` is faster.
#' 
#' If `values` or `hq` is a matrix, one hazard index is computed on each column.
#' 
#' \loadmathjax
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^N HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to N}
#'  where \eqn{HQ} denotes the hazard quotients `hq` and \eqn{N} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' The multiple usages proposed implies the argument `hq` to be explicitly named in the function call.
#' 
#' @usage
#' hazard_index(values, references)
#' hazard_index(hq)
#' @param values Numeric vector or matrix. Values for which the hazard index is to be computed.
#' @param references Numeric vector. Reference values associated with the values.
#' @param hq Numeric vector or matrix. **H**azard **q**uotients on which to compute the hazard index(es).
#' @return Numeric value or vector (according to `values` or `hq`) of the computed hazard index(es).
#' 
#' @author Gauthier Magnin
#' @references Price PS, Han X (2011).
#'             Maximum cumulative ratio (MCR) as a tool for assessing the value of performing a cumulative risk assessment.
#'             *International Journal of Environmental Research and Public Health*. 8(6): 2212-2225.
#'             <https://doi.org/10.3390/ijerph8062212>.
#' @seealso [`hazard_quotient`].
#' 
#' @examples
#' hazard_index(c(1,2,3,4,5), c(1,2,3,4,5))
#' hazard_index(hq = hazard_quotient(c(1,2,3,4,5), c(1,2,3,4,5)))
#' hazard_index(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1) ncol = 2),
#'              references = c(1,2,3,4,5))
#' 
#' @md
#' @export
hazard_index = function(values = NULL, references = NULL,
                        hq = NULL) {
  
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  # Distinction vector et matrix
  if (is.vector(hq)) return(sum(hq))
  if (is.matrix(hq)) return(colSums(hq))
}


#' Maximum Hazard Quotient (MHQ)
#' 
#' Search for the maximum of hazard quotients.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each column to compute the hazard
#'  quotients (i.e. it must have one reference value for each row of the matrix) before searching for
#'  the maximum hazard quotient. Thus, call the function with the argument `hq` is faster.
#' 
#' If `values` or `hq` is a matrix, one maximum hazard quotient is searched for each column.
#' 
#' \loadmathjax
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,N\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients `hq` and \eqn{N} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' The multiple usages proposed implies the argument `hq` to be explicitly named in the function call.
#' 
#' @usage
#' maximum_hazard_quotient(values, references)
#' maximum_hazard_quotient(hq)
#' @param values Numeric vector or matrix. Values for which to search for the maximum hazard quotient.
#' @param references Numeric vector. Reference values associated with the values.
#' @param hq Numeric vector or matrix. **H**azard **q**uotients on which to search for the maximum.
#' @return Numeric value or vector (according to `values` or `hq`) of the maximum hazard quotient(s).
#' 
#' @author Gauthier Magnin
#' @references Price PS, Han X (2011).
#'             Maximum cumulative ratio (MCR) as a tool for assessing the value of performing a cumulative risk assessment.
#'             *International Journal of Environmental Research and Public Health*. 8(6): 2212-2225.
#'             <https://doi.org/10.3390/ijerph8062212>.
#' @seealso [`hazard_quotient`], [`top_hazard_quotient`].
#' 
#' @examples
#' maximum_hazard_quotient(c(1,2,3,4,5), c(1,2,3,4,5))
#' maximum_hazard_quotient(hq = hazard_quotient(c(1,2,3,4,5), c(1,2,3,4,5)))
#' maximum_hazard_quotient(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                         ncol = 2),
#'                         references = c(1,2,3,4,5))
#' 
#' @md
#' @export
maximum_hazard_quotient = function(values = NULL, references = NULL,
                                   hq = NULL) {
  
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  # Distinction vector et matrix
  if (is.vector(hq)) return(max(hq))
  if (is.matrix(hq)) return(apply(hq, 2, "max"))
}


#' Maximum Cumulative Ratio (MCR)
#' 
#' Compute the maximum cumulative ratio as the ratio between a hazard index and a maximum hazard quotient.
#' It represents the magnitude of the hazard that is underestimated by not performing a cumulative risk
#'  assessment, given the values and references generating the hazard index and maximum hazard quotient.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each column to compute the hazard
#'  indexes (i.e. it must have one reference value for each row of the matrix) before searching for the
#'  maximum hazard quotient then computing the maximum cumulative ratio. Thus, call the function with
#'  the arguments `hi` and `mhq` is faster.
#' 
#' If `values` is a matrix (or `hi` and `mhq` are vectors larger than 1), one maximum cumulative ratio
#'  is computed for each column (or value, respectively).
#' 
#' \loadmathjax
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index `hi` and \eqn{MHQ} denotes the maximum hazard quotient `mhq`.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^N HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to N}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{N} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,N\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{N} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' The multiple usages proposed implies the arguments `hi` and `mhq` to be explicitly named in the
#'  function call.
#' 
#' @usage
#' maximum_cumulative_ratio(values, references)
#' maximum_cumulative_ratio(hi, mhq)
#' @param values Numeric vector or matrix. Values for which the maximum cumulative ratio is to be computed.
#' @param references Numeric vector. Reference values associated with the values.
#' @param hi Numeric value or vector. **H**azard **i**ndex(es) on which to compute the maximum cumulative
#'  ratio(s).
#' @param mhq Numeric value or vector. **M**aximum **h**azard **q**uotient(s) associated with the hazard
#'  index(es).
#' @return Numeric value or vector (according to `values` or `hi` and `mhq`) of the maximum cumulative
#'  ratio(s).
#' 
#' @author Gauthier Magnin
#' @references Price PS, Han X (2011).
#'             Maximum cumulative ratio (MCR) as a tool for assessing the value of performing a cumulative risk assessment.
#'             *International Journal of Environmental Research and Public Health*. 8(6): 2212-2225.
#'             <https://doi.org/10.3390/ijerph8062212>.
#' @seealso [`hazard_index`], [`maximum_hazard_quotient`], [`reciprocal_of_mcr`].
#' 
#' @examples
#' maximum_cumulative_ratio(c(1,2,3,4,5), c(1,2,3,4,5))
#' maximum_cumulative_ratio(hi = hazard_index(c(1,2,3,4,5), c(1,2,3,4,5)),
#'                          mhq = maximum_hazard_quotient(c(1,2,3,4,5), c(1,2,3,4,5)))
#' maximum_cumulative_ratio(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                          ncol = 2),
#'                          references = c(1,2,3,4,5))
#' 
#' @md
#' @export
maximum_cumulative_ratio = function(values = NULL, references = NULL,
                                    hi = NULL, mhq = NULL) {
  
  if (is.null(hi)) hi = hazard_index(values, references)
  if (is.null(mhq)) mhq = maximum_hazard_quotient(values, references)
  
  return(hi/mhq)
}


#' Missed Toxicity
#' 
#' Compute the hazard missed if a cumulative risk assessment is not performed, given the values and
#'  references generating the maximum cumulative ratio.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each column to compute the hazard
#'  indexes (i.e. it must have one reference value for each row of the matrix) before searching for the
#'  maximum hazard quotient, computing the maximum cumulative ratio then computing the missed toxicity.
#'  Thus, call the function with the argument `mcr` is faster.
#' 
#' If `values` is a matrix (or `mcr` is a vector larger than 1), one missed toxicity value is computed
#'  for each column (or value, respectively).
#' 
#' \loadmathjax
#' The missed toxicity of the vector \eqn{i} is given by:
#'  \mjdeqn{Missed toxicity_i = 1 - \frac{1}{MCR_i}}{Missed toxiciy_i = 1 - 1 / MCR_i}
#'  where \eqn{MCR} denotes the maximum cumulative ratio `mcr`.
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^N HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to N}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{N} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,N\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{N} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' The multiple usages proposed implies the argument `mcr` to be explicitly named in the function call.
#' 
#' @usage
#' missed_toxicity(values, references)
#' missed_toxicity(mcr)
#' @param values Numeric vector or matrix. Values for which the missed toxicity is to be computed.
#' @param references Numeric vector. Reference values associated with the values.
#' @param mcr Numeric value or vector. **M**aximum **c**umulative **r**atio(s) for which to compute the
#'  missed toxicity.
#' @return Numeric value or vector (according to `values` or `mcr`) of the missed toxicity.
#' 
#' @author Gauthier Magnin
#' @references Price PS, Han X (2011).
#'             Maximum cumulative ratio (MCR) as a tool for assessing the value of performing a cumulative risk assessment.
#'             *International Journal of Environmental Research and Public Health*. 8(6): 2212-2225.
#'             <https://doi.org/10.3390/ijerph8062212>.
#' @seealso [`maximum_cumulative_ratio`].
#' 
#' @examples
#' missed_toxicity(c(1,2,3,4,5), c(1,2,3,4,5))
#' missed_toxicity(mcr = maximum_cumulative_ratio(c(1,2,3,4,5), c(1,2,3,4,5)))
#' missed_toxicity(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), ncol = 2),
#'                 references = c(1,2,3,4,5))
#' 
#' @md
#' @export
missed_toxicity = function(values = NULL, references = NULL,
                           mcr = NULL) {
  
  if (is.null(mcr)) mcr = maximum_cumulative_ratio(values, references)
  return(1 - 1 / mcr)
}


