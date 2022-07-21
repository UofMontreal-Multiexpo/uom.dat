#' @include list_manager.R utils.R
NULL


#### Maximum Cumulative Ratio approach - main indicators ####

#' Hazard Quotient (HQ)
#' 
#' Compute the hazard quotient as the ratio between a value and a reference value.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' \loadmathjax
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @param values Numeric vector or matrix. Values for which the hazard quotients are to be computed.
#' @param references Numeric vector. Reference values associated with the `values`.
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
#' hazard_quotient(c(A = 1, B = 2, C = 3, D = 4, E = 5), c(1,2,3,4,5))
#' 
#' hazard_quotient(
#'   values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5)
#' )
#' 
#' @md
#' @export
hazard_quotient = function(values, references) {
  
  if (is.vector(values) && length(values) != length(references))
    stop("values and references must be the same length.")
  if (is.matrix(values) && ncol(values) != length(references))
    stop("Length of references must be equal to the number of columns of values.")
  
  # Distinction between vector and matrix cases
  if (is.vector(values)) return(values / references)
  if (is.matrix(values)) {
    hq = apply(values, 1, function(row) row / references)
    
    # Case where the matrix contains sets of only one value, the result of apply is a vector
    if (is.vector(hq)) {
      hq = matrix(hq, ncol = 1)
      colnames(hq) = colnames(values)
    } else {
      hq = t(hq)
    }
    
    rownames(hq) = rownames(values)
    return(hq)
  }
}


#' Hazard Index (HI)
#' 
#' Compute the hazard index as the sum of hazard quotients (i.e. the sum of ratios between values and
#'  reference values).
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients before computing the
#'  hazard index. Thus, call the function with the argument `hq` is faster (if it is already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` or `hq` is a matrix, one hazard index is computed for each row.
#' 
#' \loadmathjax
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the argument `hq` must be explicitly named in the function call.
#' 
#' @usage
#' hazard_index(values, references)
#' hazard_index(hq)
#' @param values Numeric vector or matrix. Values for which the hazard index is to be computed.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hq Numeric vector or matrix. **H**azard **q**uotients on which to compute the hazard index
#'  or indices.
#' @return Numeric value or vector (according to `values` or `hq`) of the computed hazard index or
#'  indices.
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
#' 
#' hazard_index(
#'   values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5)
#' )
#' 
#' @md
#' @export
hazard_index = function(values = NULL, references = NULL,
                        hq = NULL) {
  
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  # Distinction between vector and matrix cases
  if (is.vector(hq)) return(sum(hq))
  if (is.matrix(hq)) return(rowSums(hq))
}


#' Maximum Hazard Quotient (MHQ)
#' 
#' Search for the maximum of hazard quotients, also called the primary hazard quotient.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients before searching for
#'  the maximum hazard quotient. Thus, call the function with the argument `hq` is faster (if it is
#'  already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` or `hq` is a matrix, one maximum hazard quotient is searched for each row.
#' 
#' \loadmathjax
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the argument `hq` must be explicitly named in the function call.
#' 
#' @usage
#' maximum_hazard_quotient(values, references)
#' maximum_hazard_quotient(hq)
#' @param values Numeric vector or matrix. Values for which to search for the maximum hazard quotient.
#' @param references Numeric vector. Reference values associated with the `values`.
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
#' 
#' maximum_hazard_quotient(
#'   values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5)
#' )
#' 
#' @md
#' @export
maximum_hazard_quotient = function(values = NULL, references = NULL,
                                   hq = NULL) {
  
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  # Distinction between vector and matrix cases
  if (is.vector(hq)) return(max(hq))
  if (is.matrix(hq)) return(apply(hq, 1, "max"))
}


#' Maximum Cumulative Ratio (MCR)
#' 
#' Compute the maximum cumulative ratio as the ratio between a hazard index and a maximum hazard quotient.
#' It represents the magnitude of the hazard that is underestimated by not performing a cumulative risk
#'  assessment, given the values and references generating the hazard index and maximum hazard quotient.
#' It is bounded by \eqn{1} and \eqn{n}, the number of components. A value close to \eqn{1} means that
#'  one component is responsible for nearly all the toxicity. A value of \eqn{n} means that all
#'  components have equal toxicities.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard index
#'  before searching for the maximum hazard quotient then computing the maximum cumulative ratio.
#'  Thus, call the function with the arguments `hi` and `mhq` is faster (if they are already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a matrix (or `hi` and `mhq` are vectors larger than 1), one maximum cumulative ratio
#'  is computed for each row (or value, respectively).
#' 
#' \loadmathjax
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the arguments `hi` and `mhq` must be explicitly named in the
#'  function call.
#' 
#' @usage
#' maximum_cumulative_ratio(values, references)
#' maximum_cumulative_ratio(hi, mhq)
#' @param values Numeric vector or matrix. Values for which the maximum cumulative ratio is to be computed.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hi Numeric value or vector. **H**azard **i**ndex or indices on which to compute the maximum
#'  cumulative ratio(s).
#' @param mhq Numeric value or vector. **M**aximum **h**azard **q**uotient(s) associated with the hazard
#'  index or indices `hi`.
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
#' 
#' maximum_cumulative_ratio(
#'   values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5)
#' )
#' 
#' @md
#' @export
maximum_cumulative_ratio = function(values = NULL, references = NULL,
                                    hi = NULL, mhq = NULL) {
  
  if (is.null(hi)) hi = hazard_index(values, references)
  if (is.null(mhq)) mhq = maximum_hazard_quotient(values, references)
  
  return(hi / mhq)
}


#' Missed Toxicity
#' 
#' Compute the hazard missed if a cumulative risk assessment is not performed, given the values and
#'  references generating the maximum cumulative ratio.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard index
#'  before searching for the maximum hazard quotient, computing the maximum cumulative ratio then
#'  computing the missed toxicity. Thus, call the function with the argument `mcr` is faster (if it is
#'  already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a matrix (or `mcr` is a vector larger than 1), one missed toxicity value is computed
#'  for each row (or value, respectively).
#' 
#' \loadmathjax
#' The missed toxicity of the vector \eqn{i} is given by:
#'  \mjdeqn{Missed~toxicity_i = 1 - \frac{1}{MCR_i}}{Missed toxiciy_i = 1 - 1 / MCR_i}
#'  where \eqn{MCR} denotes the maximum cumulative ratio.
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the argument `mcr` must be explicitly named in the function call.
#' 
#' @usage
#' missed_toxicity(values, references)
#' missed_toxicity(mcr)
#' @param values Numeric vector or matrix. Values for which the missed toxicity is to be computed.
#' @param references Numeric vector. Reference values associated with the `values`.
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
#' 
#' missed_toxicity(
#'   values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5)
#' )
#' 
#' @md
#' @export
missed_toxicity = function(values = NULL, references = NULL,
                           mcr = NULL) {
  
  if (is.null(mcr)) mcr = maximum_cumulative_ratio(values, references)
  return(1 - 1 / mcr)
}



#### Maximum Cumulative Ratio approach - additional indicators ####

#' Reciprocal of Maximum Cumulative Ratio
#' 
#' Compute the reciprocal of maximum cumulative ratio, which measures the percentage contribution of
#'  the maximum hazard quotient to the total hazard.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard index
#'  before searching for the maximum hazard quotient, computing the maximum cumulative ratio then
#'  computing the reciprocal. Thus, call the function with the arguments `hi` and `mhq` is faster and
#'  call it with the argument `mcr` is even faster (if they are already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a matrix (or `hi` and `mhq` are vectors larger than 1, or `mcr` is such a vector),
#' one reciprocal of maximum cumulative ratio is computed for each row (or value, respectively).
#' 
#' \loadmathjax
#' The reciprocal of the maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{Reciprocal~of~MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
#'  where \eqn{MCR}, \eqn{MHQ} and {HI} denotes the maximum cumulative ratio, the maximum hazard
#'  quotient and the hazard index respectively.
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the arguments `hi`, `mhq` and `mcr` must be explicitly named in
#'  the function call.
#' 
#' @usage
#' reciprocal_of_mcr(values, references)
#' reciprocal_of_mcr(hi, mhq)
#' reciprocal_of_mcr(mcr)
#' @param values Numeric vector or matrix. Values for which the reciprocal of maximum cumulative ratio
#'  is to be computed.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hi Numeric value or vector. **H**azard **i**ndex or indices for which to compute the reciprocal
#'  of maximum cumulative ratio(s).
#' @param mhq Numeric value or vector. **M**aximum **h**azard **q**uotient(s) associated with the hazard
#'  index or indices `hi`.
#' @param mcr Numeric value or vector. **M**aximum **c**umulative **r**atio(s) on which to compute the
#'  reciprocal.
#' @return Numeric value or vector (according to `values`, to `hi` and `mhq` or to `mcr`) of the
#'  reciprocal of maximum cumulative ratio(s).
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`maximum_cumulative_ratio`], [`hazard_index`], [`maximum_hazard_quotient`].
#' 
#' @examples
#' reciprocal_of_mcr(c(1,2,3,4,5), c(1,2,3,4,5))
#' reciprocal_of_mcr(hi = hazard_index(c(1,2,3,4,5), c(1,2,3,4,5)),
#'                   mhq = maximum_hazard_quotient(c(1,2,3,4,5), c(1,2,3,4,5)))
#' reciprocal_of_mcr(mcr = maximum_cumulative_ratio(c(1,2,3,4,5), c(1,2,3,4,5)))
#' 
#' reciprocal_of_mcr(
#'   values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5)
#' )
#' 
#' @md
#' @export
reciprocal_of_mcr = function(values = NULL, references = NULL,
                             hi = NULL, mhq = NULL,
                             mcr = NULL) {
  
  if (!is.null(mcr)) return(1 / mcr)
  
  if (is.null(hi)) hi = hazard_index(values, references)
  if (is.null(mhq)) mhq = maximum_hazard_quotient(values, references)
  return(mhq / hi)
}


#' Top Hazard Quotient (THQ)
#' 
#' Identify the top `k` hazard quotients, i.e. the named values which produce the `k` highest hazard
#'  quotients.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients before identifying the
#'  highest ones. Thus, call the function with the argument `hq` is faster (if it is already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` or `hq` is a matrix, `k` hazard quotients are highlighted for each row.
#' 
#' If the number of hazard quotients that are greater than or equal to the \eqn{k-th} greater hazard
#'  quotient is greater than `k`, only the first `k` values are considered and in the order given.
#'  For example, if `hq = c(D = 5, B = 1, C = 3, A = 3)` and `k = 2`, the return is `c(D = 5, C = 3)`.
#' 
#' If `values` or `hq` contains \eqn{0}s and `k` implies some of these \eqn{0}s to be considered as
#'  top hazard quotients, the corresponding values returned are `NA`.
#' 
#' \loadmathjax
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the arguments `hq` and `k` must be explicitly named in the
#'  function call.
#' 
#' @usage
#' top_hazard_quotient(values, references, k = NULL)
#' top_hazard_quotient(hq, k = NULL)
#' @param values Numeric named vector or matrix. Values for which the top hazard quotients are to be
#'  identified.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hq Numeric named vector or matrix. **H**azard **q**uotients whose highest values are to be
#'  identified.
#' @param k Number of hazard quotients to highlight. Default is the integer part of the maximum cumulative
#'  ratio computed from `values` and `references` or from `hq`.
#' @return Vector or list of vectors (according to `values` or `hq`) of the `k` highest hazard quotients.
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`hazard_quotient`], [`maximum_hazard_quotient`].
#' 
#' @examples
#' top_hazard_quotient(c(A = 1, B = 2, C = 3, D = 4, E = 5), c(5,4,3,2,1),
#'                     k = 3)
#' top_hazard_quotient(hq = hazard_quotient(c(A = 1, B = 2, C = 3, D = 4, E = 5),
#'                                          c(5,4,3,2,1)),
#'                     k = 3)
#' 
#' top_hazard_quotient(
#'   values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5)
#' )
#' 
#' @md
#' @export
top_hazard_quotient = function(values = NULL, references = NULL,
                               hq = NULL,
                               k = NULL) {
  
  if (!is.null(k) && k < 1) stop("k must be greater than 0.")
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  # Recursive calls if there are several sets of values
  if (is.matrix(hq)) {
    thq_list = apply(hq, 1, function(row) list(top_hazard_quotient(hq = row, k = k)))
    return(lapply(thq_list, "[[", 1))
  }
  
  if (is.null(k)) {
    # Default value: integer part of the maximum cumulative ratio, or 1
    if (all(hq == 0)) k = 1
    else {
      k = trunc(maximum_cumulative_ratio(hi = hazard_index(hq = hq),
                                         mhq = maximum_hazard_quotient(hq = hq)))
    }
  } else if (k > length(hq)) {
    k = length(hq)
  }
  
  # Turn values equal to 0 into NA values
  hq[hq == 0] = NA_real_
  if (is_named(hq)) names(hq)[is.na(hq)] = NA_character_
  
  if (all(is.na(hq))) return(hq[seq_len(k)])
  if (k == 1)         return(hq[which.max(hq)])
  return(sort(hq, decreasing = TRUE)[seq_len(k)])
}


#' Classify mixture into the four MIAT groups
#' 
#' Classify mixtures into four groups according to the CEFIC-MIAT (Mixtures Industry Ad-hoc Team)
#'  decision tree, each one requiring a different risk management strategy. The groups describe the
#'  following situations:
#'  * Group I: the mixture presents a potential risk already based on individual components.
#'  * Group II: the assessment does not identify a concern.
#'  * Group IIIA: the majority of the risk offered by the mixture is driven by one component.
#'  * Group IIIB: the potential risk is driven by multiple components.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indices
#'  before searching for the maximum hazard quotients, computing the maximum cumulative ratios then
#'  classifying the mixtures. Thus, call the function with the arguments `hi` and `mhq` is faster and
#'  call it with the argument `mcr` is even faster (if they are already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a matrix (or `hi`, `mhq` and `mcr` are vectors larger than 1), one class is assigned
#'  for each row (or each value, respectively).
#' 
#' \loadmathjax
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the arguments `hi`, `mhq` and `mcr` must be explicitly named in
#'  the function call.
#' 
#' @usage
#' classify_mixture(values, references)
#' classify_mixture(hi, mhq)
#' classify_mixture(hi, mhq, mcr)
#' @param values Numeric vector or matrix. Values of the mixture(s) to classify.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hi Numeric value or vector. **H**azard **i**ndex or indices of the mixture(s) to classify.
#' @param mhq Numeric value or vector. **M**aximum **h**azard **q**uotient(s) associated with the hazard
#'  index or indices `hi`.
#' @param mcr Numeric value or vector. **M**aximum **c**umulative **r**atio(s) associated with `hi` and
#'  `mhq`.
#' @return Character value or vector (according to `values` or `hi` and `mhq`) of the groups assigned to
#'  the mixture(s).
#' 
#' @author Gauthier Magnin
#' @references
#' Reyes JM, Price PS (2018).
#' An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#' *Environment International*, 112, 77-84.
#' <https://doi.org/10.1016/j.envint.2017.12.008>.
#' 
#' De Brouwere K, et al. (2014).
#' Application of the maximum cumulative ratio (MCR) as a screening tool for the evaluation of mixtures in residential indoor air.
#' *The Science of the Total Environment*, 479-480, 267-276.
#' <https://doi.org/10.1016/j.scitotenv.2014.01.083>.
#' @seealso [`hazard_index`], [`maximum_hazard_quotient`], [`maximum_cumulative_ratio`].
#' 
#' @examples
#' classify_mixture(c(1,2,3,4,5), c(1,2,3,4,5))
#' classify_mixture(hi = hazard_index(c(1,2,3,0.5), c(1,2,3,0.5)),
#'                  mhq = maximum_hazard_quotient(c(1,2,3,0.5), c(1,2,3,0.5)))
#' 
#' classify_mixture(
#'   values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                   nrow = 3, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2", "set.3"), LETTERS[1:4])),
#'   references = c(1,2,3,0.5)
#' )
#' 
#' @md
#' @export
classify_mixture = function(values = NULL, references = NULL,
                            hi = NULL, mhq = NULL, mcr = NULL) {
  
  if (is.null(hi)) hi = hazard_index(values, references)
  if (is.null(mhq)) mhq = maximum_hazard_quotient(values, references)
  if (is.null(mcr)) mcr = maximum_cumulative_ratio(hi = hi, mhq = mhq)
  
  if (length(hi) != length(mhq) || length(mhq) != length(mcr))
    stop("hi, mhq and mcr must be the same length.")
  
  groups = character(length(hi))
  groups[mhq >= 1] = "I"
  groups[groups == "" & hi <= 1] = "II"
  groups[groups == "" & mcr < 2] = "IIIA"
  groups[groups == ""] = "IIIB"
  
  names(groups) = names(eval(expression(hi, mhq, mcr)[which.max(sapply(list(hi, mhq, mcr), is_named))]))
  return(groups)
}



#### Maximum Cumulative Ratio approach - summary functions ####

#' Summary of indicators of the MCR approach
#' 
#' Compute a set of indicators of the MCR approach, given values and references.
#' 
#' @details
#' If `values` is a vector, the reference values are directly associated with these values.
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#'  
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' If several values are equal to the maximum hazard quotient, the name retained as the top hazard
#'  quotient is the first one considering the given order.
#'  
#' \loadmathjax
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#' 
#' The reciprocal of the maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{Reciprocal~of~MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
#' 
#' The missed toxicity of the vector \eqn{i} is given by:
#'  \mjdeqn{Missed~toxicity_i = 1 - \frac{1}{MCR_i}}{Missed toxiciy_i = 1 - 1 / MCR_i}
#'  
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' @param values Numeric vector or matrix, or list of numeric vectors.
#'  Values whose indicators of the MCR approach are to be computed.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @return List if `values` is a vector; data frame otherwise.
#'  Contains the main indicators of the MCR approach computed on the given `values`:
#'  * **n**: number of values different from 0.
#'  * **HI**: Hazard Index.
#'  * **MCR**: Maximum Cumulative Ratio.
#'  * **Reciprocal**: Reciprocal of the maximum cumulative ratio.
#'  * **Group**: MIAT group.
#'  * **THQ**: Top Hazard Quotient.
#'  * **MHQ**: Maximum Hazard Quotient.
#'  * **Missed**: Hazard missed if a cumulative risk assessment is not performed.
#' 
#' @author Gauthier Magnin
#' @references 
#' Price PS, Han X (2011).
#' Maximum cumulative ratio (MCR) as a tool for assessing the value of performing a cumulative risk assessment.
#' *International Journal of Environmental Research and Public Health*. 8(6): 2212-2225.
#' <https://doi.org/10.3390/ijerph8062212>.
#'             
#' Reyes JM, Price PS (2018).
#' An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#' *Environment International*, 112, 77-84.
#' <https://doi.org/10.1016/j.envint.2017.12.008>.
#' 
#' De Brouwere K, et al. (2014).
#' Application of the maximum cumulative ratio (MCR) as a screening tool for the evaluation of mixtures in residential indoor air.
#' *The Science of the Total Environment*, 479-480, 267-276.
#' <https://doi.org/10.1016/j.scitotenv.2014.01.083>.
#' @seealso [`mcr_chart`], [`hazard_index`], [`maximum_cumulative_ratio`], [`reciprocal_of_mcr`],
#'          [`classify_mixture`], [`top_hazard_quotient`], [`maximum_hazard_quotient`], [`missed_toxicity`].
#' 
#' @examples
#' ## MCR summary on vectors and matrices
#' mcr_summary(c(A = 1, B = 2, C = 3, D = 4, E = 5), c(1,2,3,4,5))
#' mcr_summary(
#'   values = matrix(sample(seq(0.1, 1, by = 0.1), 50, replace = TRUE),
#'                   nrow = 10, byrow = TRUE,
#'                   dimnames = list(paste0("set.", 1:10), LETTERS[1:5])),
#'   references = sample(seq(1,5), 5, replace = TRUE)
#' )
#' 
#' ## MCR summary on lists
#' mcr_summary(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                           set.2 = c(A = 0.2),
#'                           set.3 = c(B = 0.3, C = 0.4)),
#'             references = c(A = 1, B = 2, C = 3))
#' mcr_summary(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                           set.2 = c(A = 0.2),
#'                           set.3 = c(B = 0.3, C = 0.4)),
#'             references = list(c(1, 2),
#'                               1,
#'                               c(2, 3)))
#' 
#' @md
#' @export
mcr_summary = function(values, references) {
  
  # Specific case in which values is a list
  if (is.list(values)) return(mcr_summary_for_list(values, references))
  
  # Summary of unusable values
  unusable_summary = list(n = 0, HI = NA_real_, MCR = NA_real_, Reciprocal = NA_real_,
                          Group = NA_character_, THQ = NA_character_,
                          MHQ = NA_real_, Missed = NA_real_)
  
  # Unusable vector of values
  if (is.vector(values) && (length(values) == 0 || all(values == 0))) {
    return(unusable_summary)
  }
  
  # Computations of the indicators
  hq  = hazard_quotient(values, references)
  hi  = hazard_index(hq = hq)
  mhq = maximum_hazard_quotient(hq = hq)
  mcr = maximum_cumulative_ratio(hi = hi, mhq = mhq)
  mt  = missed_toxicity(mcr = mcr)
  
  rmcr   = reciprocal_of_mcr(mcr = mcr)
  groups = classify_mixture(hi = hi, mhq = mhq, mcr = mcr)
  
  # Distinction between vector and matrix cases
  if (is.vector(values)) {
    thq = if (is_named(values)) names(top_hazard_quotient(hq = hq, k = 1)) else NA_character_
    
    return(list(n = sum(values != 0),
                HI = hi, MCR = mcr, Reciprocal = rmcr, Group = groups,
                THQ = thq, MHQ = mhq, Missed = mt))
  }
  
  # Matrix case
  n = apply(values, 1, function(v) sum(v != 0))
  if (is_named(values)[2]) {
    thq = names(unlist(unname(top_hazard_quotient(hq = hq, k = 1))))
  } else thq = NA_character_
  
  hi[n == 0] = unusable_summary$HI
  mcr[n == 0] = unusable_summary$MCR
  rmcr[n == 0] = unusable_summary$Reciprocal
  groups[n == 0] = unusable_summary$Group
  thq[n == 0] = unusable_summary$THQ
  mhq[n == 0] = unusable_summary$MHQ
  mt[n == 0] = unusable_summary$Missed
  
  return(data.frame(n = n, HI = hi, MCR = mcr, Reciprocal = rmcr, Group = groups,
                    THQ = thq, MHQ = mhq, Missed = mt,
                    stringsAsFactors = FALSE))
}


#' Summary of indicators of the MCR approach, on list
#' 
#' Compute a set of indicators of the MCR approach, given values and references.
#' 
#' @details
#' If `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' If several values are equal to the maximum hazard quotient, the name retained as the top hazard
#'  quotient is the first one considering the given order.
#'  
#' \loadmathjax
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#' 
#' The reciprocal of the maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{Reciprocal~of~MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
#' 
#' The missed toxicity of the vector \eqn{i} is given by:
#'  \mjdeqn{Missed~toxicity_i = 1 - \frac{1}{MCR_i}}{Missed toxiciy_i = 1 - 1 / MCR_i}
#'  
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' @template function_not_exported
#' 
#' @param values List of numeric vectors. Values whose indicators of the MCR approach are to be
#'  computed.
#' @param references Numeric named vector or list of numeric vectors. Reference values associated with
#'  the `values`. See 'Details' to know the way it is associated with `values`.
#' @return Data frame containing the main indicators of the MCR approach computed on the given `values`:
#'  * **n**: number of values different from 0.
#'  * **HI**: Hazard Index.
#'  * **MCR**: Maximum Cumulative Ratio.
#'  * **Reciprocal**: Reciprocal of the maximum cumulative ratio.
#'  * **Group**: MIAT group.
#'  * **THQ**: Top Hazard Quotient.
#'  * **MHQ**: Maximum Hazard Quotient.
#'  * **Missed**: Hazard missed if a cumulative risk assessment is not performed.
#' 
#' @author Gauthier Magnin
#' @inherit mcr_summary references
#' @seealso [`mcr_summary`].
#' 
#' @md
#' @keywords internal
mcr_summary_for_list = function(values, references) {
  
  # Different case if references is a list or a vector
  if (is.list(references)) {
    if (length(values) != length(references) ||
        any(sapply(values, length) != sapply(references, length)))
      stop("If values and references are two lists, their lengths and the ones of their elements must match.")
    
    summary = t(sapply(seq_len(length(values)), function (i) mcr_summary(values[[i]], references[[i]])))
    
  } else if (is.vector(references)) {
    if (!is_named(references) || !is_named(values)[2])
      stop("If values is a list and references is a vector, both must contained named values.")
    
    summary = t(sapply(seq_len(length(values)),
                       function (i) mcr_summary(values[[i]], references[names(values[[i]])])))
    
  } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
  
  # If a single set of values
  if (nrow(summary) == 1)
    return(as.data.frame(summary[1, ], row.names = names(values), stringsAsFactors = FALSE))
  
  # Convert the matrix to a data.frame (requires to unlist)
  # Unlist in two steps otherwise the factors are transformed into numeric
  to_return = as.data.frame(apply(summary[, c("n","HI","MCR","Reciprocal","MHQ","Missed")], 2, unlist),
                            row.names = names(values), stringsAsFactors = FALSE)
  to_return[, "Group"] = unlist(summary[, "Group"])
  to_return[, "THQ"] = unlist(summary[, "THQ"])
  
  return(to_return[, colnames(summary)])
}


#' MCR approach scatter plot
#' 
#' Plot \mjeqn{log_{10}(HI)}{log10(HI)} versus \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)} with the
#'  percentage of the reciprocal of the maximum cumulative ratio, the elements producing the top hazard
#'  quotients and the associated MIAT groups.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' The chart being plotted with the `ggplot2` package, it can be modified or completed afterwards using
#'  [`ggplot2::last_plot`] or the returned object.
#' 
#' Color specification can be done using the R predefined color names or hexadecimal values.
#' 
#' In the standard version of the chart, the grey area represents the region in which no point can be
#'  plotted because \eqn{MCR} cannot be lower than 1. In the log version, such a region does not exist.
#'  However, in the latter, points having \eqn{MCR} equal to 1 have an ordinate equal to `-Inf` and
#'  therefore cannot be plotted and generate a warning message.
#' 
#' Points that are on the boundaries of the region of the group I belong to this group.
#'  Points that are on the boundaries of the region of the group II belong to this group.
#'  Points that are on the boundary between the regions of the groups IIIA and IIIB belong to the
#'  group IIIB.
#' 
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indices
#'  before searching for the top and maximum hazard quotients, computing the maximum cumulative ratios
#'  then plot the chart. Thus, call the function with the arguments `hi`, `mcr` and `thq` is faster
#'  (if they are already computed).
#' 
#' \loadmathjax
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#'  
#' The reciprocal of the maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{Reciprocal~of~MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
#'  where \eqn{MCR}, \eqn{MHQ} and {HI} denotes the maximum cumulative ratio, the maximum hazard
#'  quotient and the hazard index respectively.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If several values are equal to the maximum hazard quotient, the name retained as the top hazard
#'  quotient is the first one considering the given order.
#' 
#' @note
#' Due to the multiple possible usages, all arguments except `values` and `references` must be explicitly
#'  named in the function call.
#' 
#' @usage
#' mcr_chart(values, references,
#'           thq_col = NULL,
#'           regions = FALSE,
#'           regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"),
#'           regions_alpha = 0.2,
#'           regions_lab = !regions,
#'           regression = FALSE,
#'           log_transform = TRUE,
#'           plot = TRUE)
#' mcr_chart(hi, mcr, thq,
#'           thq_col = NULL,
#'           regions = FALSE,
#'           regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"),
#'           regions_alpha = 0.2,
#'           regions_lab = !regions,
#'           regression = FALSE,
#'           log_transform = TRUE,
#'           plot = TRUE)
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  chart is to be plotted.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param hi Numeric vector. **H**azard **i**ndexes for which the chart is to be plotted.
#' @param mcr Numeric vector. **M**aximum **c**umulative **r**atios associated with the hazard indices
#'  `hi`.
#' @param thq Numeric named vector or list of numeric named vectors. **T**op **h**azard **q**uotients
#'  associated with the hazard indices `hi`. If list, only the first named value of each element of the
#'  list is considered.
#' @param thq_col Character named vector. Colors to assign to the **t**op **h**azard **q**uotients
#'  elements.
#' @param regions If `TRUE`, the regions corresponding to the MIAT groups are filled with the colors
#'  defined by `regions_col`.
#' @param regions_col Character vector of length 4. Define the colors for the regions of the MIAT groups
#'  (in order: I, II, IIIA and IIIB).
#' @param regions_alpha Value between 0 and 1. Opacity of the regions filled with `regions_col`.
#' @param regions_lab Logical value or vector of length 4. Define if labels for the MIAT groups should
#'  be displayed (in order: I, II, IIIA, IIIB). `TRUE` and `FALSE` are special values for all `TRUE` or
#'  all `FALSE`.
#' @param regression If `TRUE`, the linear regression between the X and Y coordinates of the points
#'  is represented with 95% confidence interval.
#' @param log_transform If `TRUE`, the log version of the chart is plotted (i.e.
#'  \mjeqn{log_{10}(HI)}{log10(HI)} versus \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)}). If `FALSE`,
#'  the standard version of the chart is plotted (i.e. \eqn{HI} versus \eqn{MCR}).
#' @param plot If `FALSE` and the returned object is assigned, the chart is not plotted.
#'  Otherwise, the chart is plotted in the active graphics device.
#' @return Chart created with the `ggplot2` package (invisible) or `NULL` if no points can be plotted
#'  (see 'Details').
#' 
#' @author Gauthier Magnin
#' @references
#' Reyes JM, Price PS (2018).
#' An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#' *Environment International*, 112, 77-84.
#' <https://doi.org/10.1016/j.envint.2017.12.008>.
#'             
#' De Brouwere K, et al. (2014).
#' Application of the maximum cumulative ratio (MCR) as a screening tool for the evaluation of mixtures in residential indoor air.
#' *The Science of the Total Environment*, 479-480, 267-276.
#' <https://doi.org/10.1016/j.scitotenv.2014.01.083>.
#' @seealso [`mcr_summary`], [`maximum_cumulative_ratio`], [`hazard_index`], [`reciprocal_of_mcr`],
#'          [`top_hazard_quotient`], [`classify_mixture`].
#' 
#' @examples
#' ## Creating a matrix of 50*5 values and one reference value for each of the 5
#' ## elements (A, B, C, D and E).
#' v <- matrix(sample(seq(0.1, 1.1, by = 0.1), 250, replace = TRUE),
#'             nrow = 50, byrow = TRUE,
#'             dimnames = list(paste0("set.", 1:50), LETTERS[1:5]))
#' r <- sample(seq(1,5), 5, replace = TRUE)
#' 
#' mcr_chart(v, r, regions = TRUE)
#' mcr_chart(v, r,
#'           thq_col = c(A = "blue", B = "green", C = "red", D = "yellow3", E = "grey"),
#'           regions = FALSE,
#'           regions_lab = c(TRUE, TRUE, FALSE, TRUE),
#'           regression = TRUE)
#' mcr_chart(hi = hazard_index(v, r),
#'           mcr = maximum_cumulative_ratio(v, r),
#'           thq = top_hazard_quotient(v, r))
#' 
#' mcr_chart(v, r, regions = TRUE, log_transform = FALSE)
#' mcr_chart(v, r,
#'           thq_col = c(A = "blue", B = "green", C = "red", D = "yellow3", E = "grey"),
#'           regions = FALSE,
#'           regions_lab = c(TRUE, TRUE, FALSE, TRUE),
#'           regression = TRUE,
#'           log_transform = FALSE)
#' mcr_chart(hi = hazard_index(v, r),
#'           mcr = maximum_cumulative_ratio(v, r),
#'           thq = top_hazard_quotient(v, r),
#'           log_transform = FALSE)
#' 
#' ## MCR chart on list
#' mcr_chart(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                         set.2 = c(A = 0.2),
#'                         set.3 = c(B = 0.3, C = 0.4)),
#'           references = c(A = 1, B = 2, C = 3),
#'           log_transform = TRUE)
#' mcr_chart(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                         set.2 = c(A = 0.2),
#'                         set.3 = c(B = 0.3, C = 0.4)),
#'           references = list(c(1, 2),
#'                             1,
#'                             c(2, 3)),
#'           log_transform = FALSE)
#' 
#' @md
#' @export
mcr_chart = function(values = NULL, references = NULL,
                     hi = NULL, mcr = NULL, thq = NULL,
                     thq_col = NULL, regions = FALSE,
                     regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"), regions_alpha = 0.2,
                     regions_lab = !regions, regression = FALSE, log_transform = TRUE, plot = TRUE) {
  
  # Specific case in which values is a list and not a matrix
  if (is.list(values)) {
    
    # Different case if references is a list or a vector
    if (is.list(references)) {
      
      # Checking that data structures are named
      if (length(values) != length(references) ||
          any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, their lengths and the ones of their elements must match.")
      if (!is_named(values)[2])
        stop("If values is a list, it must contain vectors of named numeric values.")
      
      # Computation of the indicators needed
      hi = sapply(seq_len(length(values)), function(i) hazard_index(values[[i]], references[[i]]))
      mcr = sapply(seq_len(length(values)), function(i) maximum_cumulative_ratio(values[[i]], references[[i]]))
      thq = sapply(seq_len(length(values)), function(i) top_hazard_quotient(values[[i]], references[[i]], k = 1))
      
    } else if (is.vector(references)) {
      
      # Checking that data structures are named
      if (!is_named(references) || !is_named(values)[2])
        stop("If values is a list and references is a vector. Both must contained named values.")
      
      # Computation of the indicators needed
      hi = sapply(values, function(v) hazard_index(v, references[names(v)]))
      mcr = sapply(values, function(v) maximum_cumulative_ratio(v, references[names(v)]))
      thq = sapply(unname(values), function(v) top_hazard_quotient(v, references[names(v)], k = 1))
      
    } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
    
  } else { # Case where values is a matrix or is not given
    
    # Checking that data structures are named
    if (!is.null(values) && !is_named(values)[2]) stop("Columns of values must be named.")
    if (!is.null(thq) && ((is.list(thq) && !is_named(thq)[2]) || (!is.list(thq) && !is_named(thq))))
      stop("thq must be a vector of named numeric values or a list of such vectors.")
    
    # Computation of the indicators needed
    if (is.null(hi)) hi = hazard_index(values, references)
    if (is.null(mcr)) mcr = maximum_cumulative_ratio(values, references, hi = hi)
    if (is.null(thq)) thq = top_hazard_quotient(values, references, k = 1)
  }
  
  
  # Extracting names of top hazard quotients
  thq = if (is.list(thq)) sapply(unname(thq), function(v) names(v)[1]) else names(thq)
  
  # Preparation of the data and of the limits of the graph
  if (log_transform) {
    data = data.frame(x = log10(hi), y = log10(mcr - 1), thq = thq, stringsAsFactors = FALSE)
    
    if (-Inf %in% data$y) {
      nb_inf = sum(data$y == -Inf)
      
      if (nb_inf == nrow(data)) {
        warning(paste("No points can be plotted because their MCR values are all equal to 1.",
                      "Use log_transform = FALSE to see these points."))
        return(NULL)
      }
      warning(paste(nb_inf,
                    if (nb_inf == 1) "point has not been plotted because its MCR value is equal to 1."
                    else "points have not been plotted because their MCR values are equal to 1.",
                    "Use log_transform = FALSE to see all points."))
      data = data[data$y != -Inf, ]
    }
  } else {
    data = data.frame(x = hi, y = mcr, thq = thq, stringsAsFactors = FALSE)
  }
  xlim = c(floor(min(data$x)), ceiling(max(data$x)))
  ylim = c(floor(min(data$y)), ceiling(max(data$y)))
  if (ylim[1] == ylim[2]) ylim = ylim + c(-0.05, 0.05)
  
  # Graph initialization (values, theme and fram)
  chart = ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  
  # Part of the graph specific to the type log or normal
  fun.chart = if (log_transform) plot_mcr_log_part else plot_mcr_standard_part
  chart = fun.chart(chart, xlim, ylim, regions, regions_col, regions_alpha, regions_lab)
  # Default region colors are colorblind safe and print friendly
  
  # If specific colors have to be associated with the THQ
  if (!is.null(thq_col)) chart = chart + ggplot2::scale_color_manual(values = thq_col[sort(unique(thq))])
  chart = chart + ggplot2::guides(color = ggplot2::guide_legend(order = 1))
  
  # Linear regression
  if (regression) chart = chart + ggplot2::geom_smooth(method = "lm", formula = y ~ x)
  
  if (plot) return(graphics::plot(chart))
  return(chart)
}


#' MCR approach scatter plot (log part)
#' 
#' Plot \mjeqn{log_{10}(HI)}{log10(HI)} versus \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)} with the
#'  percentage of the reciprocal of the maximum cumulative ratio, the elements producing the top hazard
#'  quotients and the associated MIAT groups. More precisely, plot the part of the chart that depends
#'  on the logarithm transformation (regions: filling, boundaries, labels; points and axes).
#' \loadmathjax
#' 
#' @details
#' The limits of the graph (`xlim` and `ylim`) are considered without taking into account the expand
#'  limits (see [`ggplot2::expand_limits`]).
#' 
#' The chart being plotted with the `ggplot2` package, it can be modified or completed afterwards using
#'  [`ggplot2::last_plot`] or the returned object.
#' 
#' Color specification can be done using the R predefined color names or hexadecimal values.
#' 
#' @template function_not_exported
#' 
#' @param chart `ggplot` object on which to add the log part.
#' @param xlim Lower and uper limits of the abscissa of the graph.
#' @param ylim Lower and uper limits of the ordinate of the graph.
#' @inheritParams mcr_chart
#' @return `ggplot` object of the chart after plotting the log part.
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`mcr_chart`], [`plot_mcr_standard_part`].
#' 
#' @md
#' @keywords internal
plot_mcr_log_part = function(chart, xlim, ylim,
                             regions, regions_col, regions_alpha, regions_lab) {
  
  # Fonction delimiting group I
  fun.mhq_1 = function(x) log10(10^x - 1)
  xmin_fun = 0.001
  xmax_fun = max(xmin_fun, xlim[2]) + 1
  root_fun = stats::uniroot(fun.mhq_1, c(0, 1))$root   # fun.mhq_1(log10(2)) = 0
  
  # For placement of the label "Group IIIB": maximum abscissa of the curve fun.mhq_1 according to the
  # limit set by ylim (inverse function of fun.mhq_1 := log10(10^y + 1))
  xmax_fun.mhq_1_visible = log10(10^ylim[2] + 1)
  x_to_use = if (xlim[2] <= xmax_fun.mhq_1_visible) xlim[2] else xmax_fun.mhq_1_visible
  x_groupIIIB = if (xlim[1] > 0) (x_to_use + xlim[1]) / 2 else x_to_use / 2
  
  # Text relating to the groups
  if (any(regions_lab)) {
    # Verification of displayed areas (non-display of the text of the areas that are not displayed)
    regions_lab = regions_lab & c(
      xlim[2] > 0 && ylim[1] < fun.mhq_1(xlim[2]), # Bottom-right corner below curve f
      xlim[1] < 0,
      ylim[1] < 0 && xlim[2] > 0 && (xlim[1] < 0 || ylim[1] > fun.mhq_1(xlim[1])), # Bottom-left corner above f
      ylim[2] > 0 && xlim[2] > 0 && (xlim[1] < 0 || ylim[2] > fun.mhq_1(xlim[1])) # Top-left corner above f
    )
    
    chart = chart + ggplot2::annotate(geom = "text",
                                      x = c(xlim[2], xlim[1], root_fun / 2, x_groupIIIB)[regions_lab],
                                      y = c(ylim[1], ylim[1], -0.05, ylim[2])[regions_lab],
                                      hjust = c(1, 0, 0.5, 0.5)[regions_lab],
                                      vjust = c(0, 0, 0.5, 1)[regions_lab],
                                      label = c("Group I", "Group II", "Group IIIA", "Group IIIB")[regions_lab])
  }
  
  # Coloring of the regions
  if (regions) {
    # The overlapping order of the polygons is not configurable at the edges
    # (at the bottom and on the right, a polygon does not reach the provided limit)
    chart = chart +
      # On the left, group II
      ggplot2::geom_polygon(data = data.frame(x = c(-Inf, -Inf, 0, 0),
                                              y = c(-Inf, Inf, Inf, -Inf)),
                            ggplot2::aes(fill = "II"),
                            alpha = regions_alpha) +
      # On the right, group I
      ggplot2::geom_polygon(data = data.frame(x = c(seq(0, xmax_fun, by = 0.001), xmax_fun),
                                              y = c(-Inf, fun.mhq_1(seq(xmin_fun, xmax_fun, by = 0.001)), -Inf)),
                            ggplot2::aes(fill = "I"),
                            alpha = regions_alpha) +
      # In the center, group IIIA
      ggplot2::geom_polygon(data = data.frame(x = c(0, seq(0, root_fun, by = 0.001)),
                                              y = c(0, -Inf, fun.mhq_1(seq(xmin_fun, root_fun, by = 0.001)))),
                            ggplot2::aes(fill = "IIIA"),
                            alpha = regions_alpha) +
      # At the top, group IIIB
      ggplot2::geom_polygon(data = data.frame(x = c(0, 0, seq(root_fun, xmax_fun, by = 0.001), xmax_fun, xmax_fun),
                                              y = c(Inf, 0, fun.mhq_1(seq(root_fun, xmax_fun, by = 0.001)), fun.mhq_1(xmax_fun), Inf)),
                            ggplot2::aes(fill = "IIIB"),
                            alpha = regions_alpha) +
      # Associated legend
      ggplot2::scale_fill_manual(values = stats::setNames(regions_col, c("I", "II", "IIIA", "IIIB")),
                                 name = "MIAT groups",
                                 guide = ggplot2::guide_legend(override.aes = list(color = "black",
                                                                                   linetype = "longdash")))
  }
  
  # Continuation of the graph (points, boundaries, legend, Y axis labels)
  chart = chart +  ggplot2::geom_point(ggplot2::aes(color = factor(thq))) +
    # Horizontal segment separating IIIA and IIIB
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = root_fun, yend = 0),
                          color = "black", linetype = "longdash") +
    # Vertical line separating the group II
    ggplot2::geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
    # Curve separating the group I
    ggplot2::stat_function(fun = fun.mhq_1, xlim = c(xmin_fun, xmax_fun),
                           color = "black", linetype = "longdash") +
    # Axis titles and legend
    ggplot2::labs(x = bquote(log[10]*"(HI)"),
                  y = bquote(atop(log[10]*"(MCR - 1)", "MHQ / HI")),
                  col = "Top Hazard Quotients") +
    # Adding the reciprocal of MCR to the Y axis labels
    ggplot2::scale_y_continuous(labels = function(y) {
      return(paste0(format(round(y, 1), nsmall = 1), "\n",
                    format(round(reciprocal_of_mcr(mcr = 10^y + 1) * 100, 1), nsmall = 1), "%"))
      # 10^y + 1 => MCR value from y = log10(mcr - 1)
    })
  
  return(chart)
}


#' MCR approach scatter plot (standard part)
#' 
#' Plot \eqn{HI} versus \eqn{MCR} with the percentage of the reciprocal of the maximum cumulative ratio,
#'  the elements producing the top hazard quotients and the associated MIAT groups. More precisely, plot
#'  the part of the chart that depends on the non-transformation of the axes (regions: filling,
#'  boundaries, labels; points and axes).
#' 
#' @details
#' The limits of the graph (`xlim` and `ylim`) are considered without taking into account the expand
#'  limits (see [`ggplot2::expand_limits`]).
#' 
#' The chart being plotted with the `ggplot2` package, it can be modified or completed afterwards using
#'  [`ggplot2::last_plot`] or the returned object.
#' 
#' Color specification can be done using the R predefined color names or hexadecimal values.
#' 
#' The grey area represents the region in which no point can be plotted because \eqn{MCR} cannot be lower
#'  than 1.
#' 
#' @template function_not_exported
#' 
#' @param chart `ggplot` object on which to add the standard part.
#' @param xlim Lower and uper limits of the abscissa of the graph.
#' @param ylim Lower and uper limits of the ordinate of the graph.
#' @inheritParams mcr_chart
#' @return `ggplot` object of the chart after plotting the standard part.
#' 
#' @author Gauthier Magnin
#' @references De Brouwere K, et al. (2014).
#'             Application of the maximum cumulative ratio (MCR) as a screening tool for the evaluation of mixtures in residential indoor air.
#'             *The Science of the Total Environment*, 479-480, 267-276.
#'             <https://doi.org/10.1016/j.scitotenv.2014.01.083>.
#' @seealso [`mcr_chart`], [`plot_mcr_log_part`].
#' 
#' @md
#' @keywords internal
plot_mcr_standard_part = function(chart, xlim, ylim,
                                  regions, regions_col, regions_alpha, regions_lab) {
  
  # Text relating to the groups
  if (any(regions_lab)) {
    # Verification of displayed areas (non-display of the text of the areas that are not displayed)
    regions_lab = regions_lab & c(xlim[2] > 1 && xlim[2] > ylim[1],
                                  xlim[1] < 1,
                                  ylim[1] < 2 && xlim[1] < 2 && xlim[2] > 1,
                                  ylim[2] > 2 && xlim[1] < ylim[1] && xlim[2] > 1)
    
    chart = chart + ggplot2::annotate(geom = "text",
                                      x = c(xlim[2],
                                            xlim[1],
                                            1.5,
                                            if (xlim[1] >= 1) (min(xlim[2], ylim[2]) + xlim[1]) / 2 + xlim[1]
                                            else (min(xlim[2], ylim[2]) + 1) / 2
                                          )[regions_lab],
                                      y = c(ylim[1] + (ylim[2] - ylim[1]) / 100, ylim[2], 1.95, ylim[2])[regions_lab],
                                      hjust = c(1, 0, 0.5, 0.5)[regions_lab],
                                      vjust = c(0, 1, 1, 1)[regions_lab],
                                      label = c("Group I", "Group II", "Group IIIA", "Group IIIB")[regions_lab])
  }
  
  # Need to use this limit instead of Inf to correctly plot the boundaries
  xmax = xlim[2] + 1
  
  # Coloring of the regions
  if (regions) {
    # The overlapping order of the polygons is not configurable at the edges
    # (at the bottom and on the right, a polygon does not reach the provided limit)
    chart = chart +
      # On the left, group II
      ggplot2::geom_polygon(data = data.frame(x = c(-Inf, -Inf, 1, 1),
                                              y = c(1, Inf, Inf, 1)),
                            ggplot2::aes(fill = "II"),
                            alpha = regions_alpha) +
      # On the right, group I
      ggplot2::geom_polygon(data = data.frame(x = c(Inf, 1, xmax),
                                              y = c(1, 1, xmax)),
                            ggplot2::aes(fill = "I"),
                            alpha = regions_alpha) +
      # In the center, group IIIA
      ggplot2::geom_polygon(data = data.frame(x = c(1, 1, 2),
                                              y = c(1, 2, 2)),
                            ggplot2::aes(fill = "IIIA"),
                            alpha = regions_alpha) +
      # At the top, group IIIB
      ggplot2::geom_polygon(data = data.frame(x = c(1, 1, Inf, xmax, 2),
                                              y = c(2, Inf, Inf, xmax, 2)),
                            ggplot2::aes(fill = "IIIB"),
                            alpha = regions_alpha) +
      # Associated legend
      ggplot2::scale_fill_manual(values = stats::setNames(regions_col, c("I", "II", "IIIA", "IIIB")),
                                 name = "MIAT groups",
                                 guide = ggplot2::guide_legend(override.aes = list(color = "black",
                                                                                   linetype = "longdash")))
  }
  
  # Continuation of the graph (points, boundaries, legend, Y axis labels)
  chart = chart + ggplot2::geom_point(ggplot2::aes(color = factor(thq))) +
    # Highlighting the impossible area
    ggplot2::geom_polygon(data = data.frame(x = c(-Inf, -Inf, Inf ,Inf),
                                            y = c(-Inf, 1, 1, -Inf)),
                          fill = "lightgrey", alpha = regions_alpha) +
    # Delimitation of the group I
    ggplot2::geom_polygon(data = data.frame(x = c(1, 1, 2),
                                            y = c(1, 2, 2)),
                          color = "black", linetype = "longdash", alpha = 0) +
    # Vertical line separating the group II
    ggplot2::geom_segment(ggplot2::aes(x = 1, y = 2, xend = 1, yend = Inf),
                          color = "black", linetype = "longdash") +
    # Line separating the group I
    ggplot2::geom_segment(ggplot2::aes(x = 2, y = 2, xend = xmax, yend = xmax),
                          color = "black", linetype = "longdash") +
    # Axis titles and legend
    ggplot2::labs(x = "HI",
                  y = bquote(atop("MCR", "MHQ / HI")),
                  col = "Top Hazard Quotients") +
    # Adding the reciprocal of MCR to the Y axis labels
    ggplot2::scale_y_continuous(labels = function(y) {
      return(paste0(y, "\n", format(round(reciprocal_of_mcr(mcr = y) * 100, 1), nsmall = 1), "%"))
    })
  
  return(chart)
}


#' Top Hazard Quotient pairs frequency
#' 
#' Build a contingency table of the counts of each combination of the top two hazard quotients pairs for
#'  which the associated hazard index is greater than 1.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Values and hazard quotients equal to 0 are ignored.
#' 
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indices
#'  before identifying the highest hazard quotients then building the contingency table. Thus, call the
#'  function with the arguments `hq` and `hi` is faster (if they are already computed).
#' 
#' \loadmathjax
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If the number of hazard quotients that are greater than or equal to the second greater hazard
#'  quotient is greater than 2, only the first two values are considered and in the order given.
#'  For example, if the hazard quotients are `D = 5`, `B = 1`, `C = 3` and `A = 3`, the top two hazard
#'  quotients considered are `D` and `C`.
#' 
#' @note
#' Due to the multiple possible usages, all arguments except `values` and `references` must be explicitly
#'  named in the function call.
#' 
#' @usage
#' thq_pairs(values, references,
#'           levels = NULL,
#'           threshold = TRUE,
#'           alone = FALSE)
#' thq_pairs(hq, hi,
#'           levels = NULL,
#'           threshold = TRUE,
#'           alone = FALSE)
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  top two hazard quotients are to be identified.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param hq Numeric named matrix or list of numeric named vectors. **H**azard **q**uotients for which
#'  the top two pairs are to be identified.
#' @param hi Numeric vector. **H**azard **i**ndexes associated with the hazard quotients `hq`.
#' @param levels Levels to consider in the output table. If `NULL`, only use of those that appear in the
#'  pairs.
#' @param threshold If `TRUE`, only values or hazard quotients associated with hazard indices greater
#'  than 1 are considered.
#' @param alone If `TRUE`, take into account single top hazard quotients (i.e. sets of values of length
#'  1). If so, a level named `"NULL"` is added as combination with such top hazard quotients.
#' @return
#' `NULL` if (1) `alone = FALSE` and no set of `values` (or of `hq`) has more than one element different
#'   from 0
#'   or (2) `threshold = TRUE` and no related hazard index is greater than 1
#'   or (3) `threshold = TRUE`, `alone = FALSE` and no set of values meets the two conditions.
#' 
#' Contingency table otherwise. Frequencies of pairs that produce the top two hazard quotients,
#'  considering all sets of values or not (accordingly to the arguments `threshold` and `alone`).
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`thq_by_group`], [`top_hazard_quotient`], [`hazard_quotient`], [`hazard_index`].
#' 
#' @examples
#' 
#' v1 <- matrix(c(1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'              nrow = 2, byrow = TRUE,
#'              dimnames = list(c("set.1", "set.2"), LETTERS[1:5]))
#' r1 <- c(1,2,3,4,5)
#' 
#' thq_pairs(v1, r1)
#' thq_pairs(hq = hazard_quotient(v1, r1),
#'           hi = hazard_index(v1, r1))
#' 
#' ## With and without levels parameter
#' v2 <- matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'              nrow = 3, byrow = TRUE,
#'              dimnames = list(paste0("set.", 1:3), LETTERS[1:4]))
#' r2 <- c(1,2,3,0.5)
#' thq_pairs(v2, r2, levels = LETTERS[1:4])
#' thq_pairs(v2, r2)
#' 
#' ## NULL because all HI are lower than or equal to 1
#' v3 <- matrix(c(.1, .2, .3, .4),
#'              nrow = 2, byrow = TRUE,
#'              dimnames = list(c("set.1", "set.2"), c("A","B")))
#' r3 <- c(5,5)
#' thq_pairs(v3, r3)
#' hazard_index(v3, r3)
#' thq_pairs(v3, r3, threshold = FALSE)
#' 
#' ## Building contingency table from a list
#' v4 <- list(set.1 = c(A = 0.5, B = 0.5),
#'            set.2 = c(A = 1),
#'            set.3 = c(B = 0.5, C = 0.5))
#' thq_pairs(values = v4,
#'           references = list(c(0.3, 0.6),
#'                             0.3,
#'                             c(0.6, 1)))
#' thq_pairs(values = v4,
#'           references = c(A = 0.3, B = 0.6, C = 1))
#' thq_pairs(values = v4,
#'           references = c(A = 0.3, B = 0.6, C = 1),
#'           alone = TRUE)
#' 
#' @md
#' @export
thq_pairs = function(values = NULL, references = NULL,
                     hq = NULL, hi = NULL,
                     levels = NULL, threshold = TRUE, alone = FALSE) {
  
  # Different case if values or hq is a list or a matrix
  if (is.list(values) || is.list(hq)) {
    thq = thq_pairs_for_list(values, references, hq, hi, threshold, alone)
    
  } else if (is.matrix(values) || is.matrix(hq)) {
    thq = thq_pairs_for_matrix(values, references, hq, hi, threshold, alone)
    
  } else {
    if (!is.null(hq)) stop("hq must be a numeric named matrix or list of numeric named vectors.")
    if (!is.null(values)) stop("values must be a numeric named matrix or list of numeric named vectors.")
    stop("One of values or hq must not be NULL.")
  }
  
  if (is.null(thq)) return(NULL)
  
  
  # Definition of the levels to consider in the table
  if (is.null(levels)) levels = sort(unique(c(thq)))
  # If taking into account elements that are alone, adding a "NULL" level
  if (alone && !is.element("NULL", levels)) levels = c(levels, "NULL")
  
  # Creation of a table and application of symmetry
  if (is.vector(thq) == 1) {
    # If pair of THQ for a single set of values
    freq_table = table(factor(thq[1], levels = levels),
                       factor(thq[2], levels = levels))
    
    freq_table[thq[2], thq[1]] = freq_table[thq[1], thq[2]]
  } else {
    # If pairs of THQ for several sets of values
    freq_table = table(factor(thq[1, ], levels = levels),
                       factor(thq[2, ], levels = levels))
    freq_table[lower.tri(freq_table)] = t(freq_table)[lower.tri(freq_table)]
    # Works because THQ in each pairs are sorted
  }
  
  # If taking into account elements that are alone and there are some such elements
  if (alone && sum(freq_table["NULL", ]) != 0) {
    # Moving NULL column and row at the end of the table
    index_null = which(colnames(freq_table) == "NULL")
    indices_reordered = c(seq_len(nrow(freq_table))[-index_null], index_null)
    freq_table = freq_table[indices_reordered, indices_reordered]
  } # Else, NULL is already placed at the end or doesn't exist
  
  # Removal of the dimension names "" (not useful and generate a new line in the output)
  dimnames(freq_table) = unname(dimnames(freq_table))
  return(freq_table)
}


#' Top Hazard Quotient pairs, in list
#' 
#' Identify the top two hazard quotients pairs for which the associated hazard
#'  indices are greater than 1.
#' 
#' @details
#' The reference values can be a vector of named values or a list.
#'  If `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Values and hazard quotients equal to 0 are ignored.
#'  
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indices
#'  before identifying the highest hazard quotients then building the contingency table. Thus, call the
#'  function with the arguments `hq` and `hi` is faster (if they are already computed).
#' 
#' \loadmathjax
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If the number of hazard quotients that are greater than or equal to the second greater hazard
#'  quotient is greater than 2, only the first two values are considered and in the order given.
#'  For example, if the hazard quotients are `D = 5`, `B = 1`, `C = 3` and `A = 3`, the top two hazard
#'  quotients considered are `D` and `C`.
#' 
#' @note
#' Due to the multiple possible usages, all arguments except `values` and `references` must be explicitly
#'  named in the function call.
#' 
#' @template function_not_exported
#' 
#' @usage
#' thq_pairs_for_list(values, references,
#'                    threshold = TRUE,
#'                    alone = FALSE)
#' thq_pairs_for_list(hq, hi,
#'                    threshold = TRUE,
#'                    alone = FALSE)
#' @param values List of numeric named vectors. Vectors of values for which the top two hazard
#'  quotients are to be identified.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param hq List of numeric named vectors. **H**azard **q**uotients for which the top two pairs are
#'  to be identified.
#' @param hi Numeric vector. **H**azard **i**ndexes associated with the hazard quotients `hq`.
#' @param threshold If `TRUE`, only values or hazard quotients associated with hazard indices greater
#'  than 1 are considered.
#' @param alone If `TRUE`, take into account single top hazard quotients (i.e. sets of values of length
#'  1). If so, a level named `"NULL"` is added as combination with such top hazard quotients.
#' @return
#' `NULL` if (1) `alone = FALSE` and no set of `values` (or of `hq`) has more than one element different
#'   from 0
#'   or (2) `threshold = TRUE` and no related hazard index is greater than 1
#'   or (3) `threshold = TRUE`, `alone = FALSE` and no set of values meets the two conditions.
#' 
#' Pairs that produce the top two hazard quotients in the sets of values selected accordingly to the
#'  arguments `threshold` and `alone`. Vector if only one pair; two-row matrix otherwise.
#' 
#' @author Gauthier Magnin
#' @inherit thq_pairs references
#' @seealso [`thq_pairs`], [`thq_pairs_for_matrix`].
#' 
#' @md
#' @keywords internal
thq_pairs_for_list = function(values = NULL, references = NULL,
                              hq = NULL, hi = NULL,
                              threshold = TRUE, alone = FALSE) {
  
  # Checking that data structures are named
  if (!is.null(values) && !is_named(values)[2])
    stop("If values is a list, it must contain vectors of named numeric values.")
  if (!is.null(hq) && !is_named(hq)[2])
    stop("If hq is a list, it must contain vectors of named numeric values.")
  
  # If HI and/or HQ is not given
  if (is.null(hi) || is.null(hq)) {
    
    # Different case if references is a list or a vector
    if (is.list(references)) {
      
      # Checking that the sets of elements in the lists have the same sizes
      if (length(values) != length(references) ||
          any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, their lengths and the ones of their elements must match.")
      
      # Computation of the indicators needed
      if (is.null(hq)) hq = lapply(seq_along(values),
                                   function(i) hazard_quotient(values[[i]], references[[i]]))
      if (is.null(hi)) hi = sapply(seq_along(values),
                                   function(i) hazard_index(values[[i]], references[[i]]))
      
    } else if (is.vector(references)) {
      
      # Checking that references contain named data
      if (!is_named(references)) stop("If values is a list and references is a vector, references must contained named values.")
      
      # Computation of the indicators needed
      if (is.null(hq)) hq = lapply(seq_along(values),
                                   function(i) hazard_quotient(values[[i]], references[names(values[[i]])]))
      if (is.null(hi)) hi = sapply(values,
                                   function(v) hazard_index(v, references[names(v)]))
      
    } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
  }
  
  
  # Ignore HQ equal to 0
  hq = lapply(hq, function(x) x[x != 0])
  
  # Extraction of the HQ to use, according to the criteria on HI and on the length of the sets of values
  hq_to_use = hq[(!threshold | hi > 1) & (alone | sapply(hq, length) != 1)]
  
  # If no HI is greater than 1 or no vector contain more than one value
  if (length(hq_to_use) == 0) return(NULL)
  
  # When a single value, pair with "NULL"
  if (alone) {
    index_alone = which(sapply(hq_to_use, length) == 1)
    hq_to_use[index_alone] = lapply(hq_to_use[index_alone], c, "NULL" = 1)
  }
  
  # Searching for the Top 2 HQ
  if (length(hq_to_use) == 1) {
    # If a single set of values meets the criteria (HI > 1 and/or length > 1)
    thq = names(top_hazard_quotient(hq = hq_to_use[[1]], k = 2))
  } else {
    # If several sets of values meet the criteria (HI > 1 and/or length > 1)
    thq = sapply(hq_to_use, function(hq) sort(names(top_hazard_quotient(hq = hq, k = 2))))
  }
  
  return(thq)
}


#' Top Hazard Quotient pairs, in matrix
#' 
#' Identify the top two hazard quotients pairs for which the associated hazard
#'  indices are greater than 1.
#' 
#' @details
#' The reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' Values and hazard quotients equal to 0 are ignored.
#' 
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indices
#'  before identifying the highest hazard quotients then building the contingency table. Thus, call the
#'  function with the arguments `hq` and `hi` is faster (if they are already computed).
#' 
#' \loadmathjax
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If the number of hazard quotients that are greater than or equal to the second greater hazard
#'  quotient is greater than 2, only the first two values are considered and in the order given.
#'  For example, if the hazard quotients are `D = 5`, `B = 1`, `C = 3` and `A = 3`, the top two hazard
#'  quotients considered are `D` and `C`.
#' 
#' @note
#' Due to the multiple possible usages, all arguments except `values` and `references` must be explicitly
#'  named in the function call.
#' 
#' @template function_not_exported
#' 
#' @usage
#' thq_pairs_for_matrix(values, references,
#'                      threshold = TRUE,
#'                      alone = FALSE)
#' thq_pairs_for_matrix(hq, hi,
#'                      threshold = TRUE,
#'                      alone = FALSE)
#' @param values Numeric named matrix. Vectors of values for which the top two hazard quotients are to
#'  be identified.
#' @param references Numeric vector. Reference values associated with the `values`. See 'Details' to
#'  know the way it is associated with `values`.
#' @param hq Numeric named matrix. **H**azard **q**uotients for which the top two pairs are to be
#'  identified.
#' @param hi Numeric vector. **H**azard **i**ndexes associated with the hazard quotients `hq`.
#' @param threshold If `TRUE`, only values or hazard quotients associated with hazard indices greater
#'  than 1 are considered.
#' @param alone If `TRUE`, take into account single top hazard quotients (i.e. sets of values of length
#'  1). If so, a level named `"NULL"` is added as combination with such top hazard quotients.
#' @return
#' `NULL` if (1) `alone = FALSE` and no set of `values` (or of `hq`) has more than one element different
#'   from 0
#'   or (2) `threshold = TRUE` and no related hazard index is greater than 1
#'   or (3) `threshold = TRUE`, `alone = FALSE` and no set of values meets the two conditions.
#' 
#' Pairs that produce the top two hazard quotients in the sets of values selected accordingly to the
#'  arguments `threshold` and `alone`. Vector if only one pair; two-row matrix otherwise.
#' 
#' @author Gauthier Magnin
#' @inherit thq_pairs references
#' @seealso [`thq_pairs`], [`thq_pairs_for_list`].
#' 
#' @md
#' @keywords internal
thq_pairs_for_matrix = function(values = NULL, references = NULL,
                                hq = NULL, hi = NULL,
                                threshold = TRUE, alone = FALSE) {
  
  # Checking that data structures are named
  if (!is.null(values) && !is_named(values)[2]) stop("Columns of values must be named.")
  if (!is.null(hq) && !is_named(hq)[2]) stop("Columns of hq must be named.")
  
  # Computation of the indicators needed
  if (is.null(hq)) hq = hazard_quotient(values, references)
  if (is.null(hi)) hi = hazard_index(hq = hq)
  
  # Exctraction of the HQ to use, according to the criteria on HI and on length of the sets of values
  hq_to_use = hq[(!threshold | hi > 1) & (alone | apply(hq, 1, function(x) sum(x != 0) != 1)), ,
                 drop = FALSE]
  
  # If no HI is greater than 2 or no vector contain more than one value
  if (nrow(hq_to_use) == 0) return(NULL)
  
  # When a single value, pair with "NULL"
  if (alone) {
    index_alone = apply(hq_to_use, 1, function(x) sum(x != 0) == 1)
    hq_to_use = cbind(hq_to_use, "NULL" = ifelse(index_alone, 1, 0))
  }
  
  # Searching for the Top 2 HQ
  if (nrow(hq_to_use) == 1) {
    # If a single set of values meets the criteria (HI > 1 and/or length > 1)
    thq = names(top_hazard_quotient(hq = hq_to_use[1, ], k = 2))
  } else {
    # If several sets of values meet the criteria (HI > 1 and/or length > 1)
    thq = apply(hq_to_use, 1, function(hq) sort(names(top_hazard_quotient(hq = hq, k = 2))))
  }
  
  return(thq)
}


#' Top Hazard Quotients frequency by group
#' 
#' Build a contingency table of the counts of each combination of name of the element producing the
#'  top hazard quotient with the associated MIAT group. The groups describe the following situations:
#'  * Group I: the mixture presents a potential risk already based on individual components.
#'  * Group II: the assessment does not identify a concern.
#'  * Group IIIA: the majority of the risk offered by the mixture is driven by one component.
#'  * Group IIIB: the potential risk is driven by multiple components.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indices
#'  before searching for the maximum hazard quotients, computing the maximum cumulative ratios, performing
#'  the classification then build the table. Thus, call the function with the arguments `hq` and `groups`
#'  is faster and call it with the arguments `thq` and `groups` is even faster (if they are already
#'  computed).
#' 
#' \loadmathjax
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If several values are equal to the maximum hazard quotient, the name retained as the top hazard
#'  quotient is the first one considering the given order.
#' 
#' @note
#' Due to the multiple possible usages, the arguments `hq`, `thq`, `groups` and `levels` must be
#'  explicitly named in the function call.
#' 
#' @usage
#' thq_by_group(values, references, levels = NULL)
#' thq_by_group(hq, groups, levels = NULL)
#' thq_by_group(thq, groups, levels = NULL)
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  table is to be built.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param hq Numeric named matrix or list of numeric named vectors. **H**azard **q**uotients for which
#'  the table is to be built.
#' @param thq Numeric named vector or list of numeric named vectors. **T**op **h**azard **q**uotients
#'  to use to the count. If list, only the first named value of each element of the list is considered.
#' @param groups Character vector. MIAT groups associated with the hazard quotients `hq` or with the
#'  top hazard quotients `thq`.
#' @param levels Levels to consider in the output table. If `NULL`, only use of those that appear in the
#'  top hazard quotients.
#' @return Contingency table. Frequencies of elements that produce the top hazard quotient with their
#'  associated groups.
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`thq_pairs`], [`classify_mixture`], [`top_hazard_quotient`], [`hazard_quotient`].
#' 
#' @examples
#' ## Creating a matrix of 4*3 values and one reference value for each of the 4
#' ## elements (A, B, C, and D).
#' v <- matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'             nrow = 3, byrow = TRUE,
#'             dimnames = list(paste0("set.", 1:3), LETTERS[1:4]))
#' r <- c(1, 2, 3, 0.5)
#' 
#' ## Without levels parameter and with the different usages
#' thq_by_group(v, r)
#' thq_by_group(hq = hazard_quotient(v, r),
#'              groups = classify_mixture(v, r))
#' thq_by_group(thq = top_hazard_quotient(v, r),
#'              groups = classify_mixture(v, r))
#' 
#' ## With levels parameter
#' thq_by_group(values = v, references = r, levels = LETTERS[1:4])
#' 
#' ## Building contingency table from a list
#' thq_by_group(values = list(c(A = 0.1, B = 0.5),
#'                            c(A = 0.2),
#'                            c(B = 0.3, C = 0.4)),
#'              references = c(A = 1, B = 2, C = 3))
#' thq_by_group(values = list(c(A = 0.1, B = 0.5),
#'                            c(A = 0.2),
#'                            c(B = 0.3, C = 0.4)),
#'              references = list(c(1, 2),
#'                                1,
#'                                c(2, 3)))
#' 
#' @md
#' @export
thq_by_group = function(values = NULL, references = NULL,
                        hq = NULL,
                        thq = NULL,
                        groups = NULL, levels = NULL) {
  
  if (!is.null(thq) && ((is.list(thq) && !is_named(thq)[2]) || (!is.list(thq) && !is_named(thq))))
    stop("thq must be a vector of named numeric values or a list of such vectors.")
  
  # If thq and/or groups is not given
  if (is.null(thq) || is.null(groups)) {
    
    # Specific case in which values or hq is a list and not a matrix
    if (is.list(values) || is.list(hq)) {
      
      # Checking that data structures are named
      if (!is.null(values) && !is_named(values)[2])
        stop("If values is a list, it must contain vectors of named numeric values.")
      if (!is.null(hq) && !is_named(hq)[2])
        stop("If hq is a list, it must contain vectors of named numeric values.")
      
      # If neither HQ nor THQ is given
      if (is.null(thq) && is.null(hq)) {
        
        # Different case if references is a list or a vector
        if (is.list(references)) {
          
          # Checking that data structures have the same lengths
          if (length(values) != length(references) ||
              any(sapply(values, length) != sapply(references, length)))
            stop("If values and references are two lists, their lengths and the ones of their elements must match.")
          
          # Computation of the indicators needed
          if (is.null(thq)) thq = sapply(seq_len(length(values)),
                                         function(i) top_hazard_quotient(values[[i]], references[[i]], k = 1))
          if (is.null(groups)) groups = sapply(seq_len(length(values)),
                                               function(i) classify_mixture(values[[i]], references[[i]]))
          
        } else if (is.vector(references)) {
          
          # Checking that data structures are named
          if (!is_named(references)) stop("If values is a list and references is a vector. Both must contained named values.")
          
          # Computation of the indicators needed
          if (is.null(thq)) thq = sapply(unname(values),
                                         function(v) top_hazard_quotient(v, references[names(v)], k = 1))
          if (is.null(groups)) groups = sapply(values,
                                               function(v) classify_mixture(v, references[names(v)]))
          
        } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
        
      } else if (!is.null(hq)) { # If HQ is given
        # Computation of the indicators needed
        if (is.null(thq)) thq = sapply(unname(hq), function(hqs) top_hazard_quotient(hq = hqs, k = 1))
        if (is.null(groups)) groups = sapply(hq, function(hqs) classify_mixture(hi = hazard_index(hq = hqs),
                                                                                mhq = maximum_hazard_quotient(hq = hqs)))
      }
    } else { # Case where values or hq is a matrix, or neither of the two is given
      
      # Checking that data structures are named
      if (!is.null(values) && !is_named(values)[2]) stop("Columns of values must be named.")
      if (!is.null(hq) && !is_named(hq)[2]) stop("Columns of hq must be named.")
      
      # Computation of the indicators needed
      if (is.null(thq)) {
        if (is.null(hq)) hq = hazard_quotient(values, references)
        thq = top_hazard_quotient(hq = hq, k = 1)
      }
      if (is.null(groups)) {
        if (is.null(hq)) groups = classify_mixture(values, references)
        else groups = classify_mixture(hi = hazard_index(hq = hq),
                                       mhq = maximum_hazard_quotient(hq = hq))
      }
    }
  }
  
  # thq is either a list (hq or values is a list) or a vector (hq or values is a matrix)
  thq_names = if (is.list(thq)) sapply(unname(thq), function(v) names(v)[1]) else names(thq)
  if (!is.null(levels)) thq_names = factor(thq_names, levels = levels)
  freq_table = table(thq_names, factor(groups, levels = c("I", "II", "IIIA", "IIIB")))
  
  # Removal of the dimension name "thq_names" and return
  dimnames(freq_table) = unname(dimnames(freq_table))
  return(freq_table)
}



#### Application of the MCR approach according to classes ####

#' Check data for MCR approach according to classes
#' 
#' Check the naming and structure of the data of values and references on which to apply the MCR approach
#'  according to classes. Stop the execution and print an error message if the data do not satisfy the
#'  naming and structure criteria.
#' 
#' @details
#' The criteria about naming and structure are the following:
#' * If `values` is a vector, its values must be named.
#' * If `values` is a matrix, its columns must be named.
#' * If `values` is a list, it must contain vectors of named values.
#' * If `values` and `references` are two lists, lengths of the elements of these lists must match.
#' * If `values` is a list and `references` is a vector, the values of the latter must be named.
#' 
#' If `references = NULL`, check the `values` only.
#' 
#' @template function_not_exported
#' 
#' @param values Numeric named vector or matrix, or list of numeric named vectors. Data structure
#'  whose naming is to be checked.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`.
#' @param vector Should be `TRUE` if `values` can be a vector.
#' @param matrix Should be `TRUE` if `values` can be a matrix.
#' @param list Should be `TRUE` if `values` can be a list.
#' 
#' @author Gauthier Magnin
#' @seealso [`validate_classes`].
#' @md
#' @keywords internal
check_data_for_mcr_by_class = function(values, references = NULL, vector = TRUE, matrix = TRUE, list = TRUE) {
  
  # Checking the naming of values and references if values can be a list, a matrix, a vector
  if (list && is.list(values)) {
    
    if (is.null(references)){
      if(!is_named(values)[2])
        stop("If values is a list, it must contain vectors of named numeric values.")
    } else {
      if (is.list(references)) {
        if (length(values) != length(references) ||
            any(sapply(values, length) != sapply(references, length)))
          stop("If values and references are two lists, their lengths and the ones of their elements must match.")
        if (!is_named(values)[2])
          stop("If values is a list, it must contain vectors of named numeric values.")
        
      } else if (is.vector(references)) {
        if (!is_named(references) || !is_named(values)[2])
          stop("If values is a list and references is a vector, both must contained named values.")
      } else
        stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
    }
  }
  else if (matrix && is.matrix(values) && !is_named(values)[2]) stop("If values is a matrix, its columns must be named.")
  else if (vector && is.vector(values) && !is_named(values)) stop("If values is a vector, it must have named numeric values.")
}


#' Validate classes for the MCR approach
#' 
#' Check that the classes are given as a list or as a logical matrix. Stop the execution and print an
#' error message if not. If they are given as a list, they are converted into a logical matrix.
#' 
#' @template function_not_exported
#' 
#' @param classes List or logical matrix associating value names with classes.
#'  If list, its names are the value names and the elements are vectors of associated classes.
#'  If logical matrix, its columns are named according to the classes and the row names contain the
#'  value names. A `TRUE` value indicates that a specific name is part of a specific class.
#' @return Classes, as a logical matrix.
#' 
#' @author Gauthier Magnin
#' @seealso [`check_data_for_mcr_by_class`].
#' @md
#' @keywords internal
validate_classes = function(classes) {
  
  if (is.list(classes)) classes = turn_list_into_logical_matrix(classes)
  else if (!is.matrix(classes) || typeof(classes) != "logical")
    stop("classes must be a list or a logical matrix.")
  
  return(classes)
}


#' Summary by class of indicators of the MCR approach
#' 
#' Compute a set of indicators of the MCR approach according to classes, given values and references.
#'  Indicators are computed for each set of values and for each class. For each class, only values
#'  corresponding to this class are considered.
#' 
#' @details
#' If `values` is a vector, the reference values are directly associated with these values.
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#'  
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' If several values are equal to the maximum hazard quotient, the name retained as the top hazard
#'  quotient is the first one considering the given order.
#' 
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is slightly faster.
#'  
#' \loadmathjax
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#' 
#' The reciprocal of the maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{Reciprocal~of~MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
#' 
#' The missed toxicity of the vector \eqn{i} is given by:
#'  \mjdeqn{Missed~toxicity_i = 1 - \frac{1}{MCR_i}}{Missed toxiciy_i = 1 - 1 / MCR_i}
#'  
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' @param values Numeric named vector or matrix, or list of numeric named vectors.
#'  Values whose indicators of the MCR approach are to be computed according to classes.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param classes List or logical matrix associating the `values` names with classes.
#'  If list, its names are those present in `values` and the elements are vectors of associated classes.
#'  If logical matrix, its columns are named according to the classes and the row names
#'  contain the names associated with the `values`. A `TRUE` value indicates that a specific name
#'  is part of a specific class.
#' @param by_set `TRUE` or `FALSE` whether to group results by set of values or by class.
#'  Always `TRUE` if values is a vector.
#' @param all_classes Logical indicating whether all classes must be considered for each set of values
#'  or only those that are actually associated with the set of values. Ignored if `by_set` is `FALSE`.
#' @return Data frame or list of data frames (according to `values`) containing the main indicators of
#'  the MCR approach, computed on the given `values` and for each class encountered (or for all classes,
#'  if `all_classes` is `TRUE`):
#'  * **n**: number of values different from 0.
#'  * **HI**: Hazard Index.
#'  * **MCR**: Maximum Cumulative Ratio.
#'  * **Reciprocal**: Reciprocal of the maximum cumulative ratio.
#'  * **Group**: MIAT group.
#'  * **THQ**: Top Hazard Quotient.
#'  * **MHQ**: Maximum Hazard Quotient.
#'  * **Missed**: Hazard missed if a cumulative risk assessment is not performed.
#' 
#' @author Gauthier Magnin
#' @inherit mcr_summary references
#' @seealso 
#' Summary independent of classes: [`mcr_summary`].
#' 
#' Generic function to apply the MCR approach according to classes: [`mcr_approach_by_class`].
#' 
#' Other functions of the MCR approach applying according to classes: [`mcr_chart_by_class`],
#'  [`thq_pairs_by_class`], [`thq_by_group_by_class`].
#' 
#' Specific indicators: [`hazard_index`], [`maximum_cumulative_ratio`], [`reciprocal_of_mcr`],
#'  [`classify_mixture`], [`top_hazard_quotient`], [`maximum_hazard_quotient`], [`missed_toxicity`].
#' 
#' @examples
#' ## Association of classes (C1 to C8) with elements A, B, C, D and E
#' classes <- list(A = c("C5", "C6", "C8"),
#'                 B = "C8",
#'                 C = c("C3", "C8"),
#'                 D = c("C1", "C3", "C4", "C6"),
#'                 E = c("C2", "C4", "C5", "C7", "C8"))
#' 
#' ## MCR summary by class on vectors
#' mcr_summary_by_class(values = c(A = 1, B = 2, C = 3, D = 4, E = 5),
#'                      references = c(1, 2, 3, 4, 5),
#'                      classes = classes)
#' mcr_summary_by_class(values = c(A = 1, B = 2, C = 3, D = 4),
#'                      references = c(1, 2, 3, 4),
#'                      classes)
#' 
#' ## MCR summary by class on matrices
#' mcr_summary_by_class(
#'   values = matrix(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5),
#'   classes
#' )
#' mcr_summary_by_class(
#'   values = matrix(c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9),
#'                   nrow = 2, byrow = TRUE,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:4])),
#'   references = c(1,2,3,4),
#'   classes
#' )
#' 
#' ## MCR summary by class on lists
#' mcr_summary_by_class(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                                    set.2 = c(A = 0.2),
#'                                    set.3 = c(B = 0.3, C = 0.4)),
#'                      references = c(A = 1, B = 2, C = 3),
#'                      classes)
#' mcr_summary_by_class(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                                    set.2 = c(A = 0.2),
#'                                    set.3 = c(B = 0.3, C = 0.4)),
#'                      references = list(c(1, 2),
#'                                        1,
#'                                        c(2, 3)),
#'                      classes)
#' 
#' @md
#' @export
mcr_summary_by_class = function(values, references, classes,
                                by_set = FALSE, all_classes = FALSE) {
  
  # Checking data naming and use of classes as a logical matrix
  check_data_for_mcr_by_class(values, references)
  classes = validate_classes(classes)
  
  # Force by_set to TRUE if values is a vector
  if (is.vector(values) && !is.list(values)) by_set = TRUE
  
  # Computation of one data frame for each set of values
  if (by_set) {
    
    # Case of a list of values
    if (is.list(values)) {
      # For each set of values, computation of the MCR indicators for each class
      # Different case if references is a list or a vector
      if (is.list(references)) {
        to_return = lapply(seq_along(values),
                           function(i) mcr_summary_by_class(values[[i]], references[[i]],
                                                            classes, by_set, all_classes))
        return(stats::setNames(to_return, names(values)))
      }
      return(lapply(values, function(v) mcr_summary_by_class(v, references[names(v)],
                                                             classes, by_set, all_classes)))
    }
    
    # Case of a matrix of values
    if (is.matrix(values)) {
      # For each set of values, computation of the MCR indicators for each class
      return(apply(values, 1, function(v) mcr_summary_by_class(v, references,
                                                               classes, by_set, all_classes)))
    }
    
    # Case of a single vector of values
    if (is.vector(values)) {
      # For each class, computation of the MCR indicators of the corresponding values and references
      summaries = apply(classes, 2, function(column) {
        new_vr = subset_from_class(values, references, classes, colnames(classes)[parent.frame()$i[]])
        return(mcr_summary(new_vr[["values"]], new_vr[["references"]]))
      })
      
      if (!all_classes) {
        # Removal of classes for which there is no value
        summaries = summaries[sapply(summaries, "[[", "n") != 0]
        
        # Return NULL if the values are not associated with any class
        if (length(summaries) == 0) return(NULL)
      }
      
      # Turn the list of lists into a data frame
      return(do.call(rbind.data.frame, summaries))
    }
  }
  
  # Computation of one data frame for each class
  summaries = apply(classes, 2, function(column) {
    
    # Extraction of the values and references corresponding to the class
    new_vr = subset_from_class(values, references, classes, colnames(classes)[parent.frame()$i[]])
    
    # NA if the class is not represented (different case if values is a list or a matrix)
    if ((is.list(values) && (length(new_vr[["values"]]) == 0 || length(new_vr[["values"]][[1]]) == 0)) ||
        (is.matrix(values) && ncol(new_vr[["values"]]) == 0)) return(NA)
    
    return(mcr_summary(new_vr[["values"]], new_vr[["references"]]))
  })
  
  return(summaries[!is.na(summaries)])
}


#' MCR approach scatter plot by class
#' 
#' Create charts of \mjeqn{log_{10}(HI)}{log10(HI)} versus \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)}
#'  with the percentage of the reciprocal of the maximum cumulative ratio, the elements producing the
#'  top hazard quotients and the associated MIAT groups, according to classes. For each class, one chart
#'  is created from the subset of values corresponding to this class.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#'  
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is slightly faster.
#' 
#' The charts being created with the `ggplot2` package, they can be modified or completed afterwards
#'  using the returned object.
#' 
#' Color specification can be done using the R predefined color names or hexadecimal values.
#' 
#' In the standard version of the chart, the grey area represents the region in which no point can be
#'  plotted because \eqn{MCR} cannot be lower than 1. In the log version, such a region does not exist.
#'  However, in the latter, points having \eqn{MCR} equal to 1 have an ordinate equal to `-Inf` and
#'  therefore cannot be plotted and generate a warning message.
#' 
#' \loadmathjax
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#'  
#' The reciprocal of the maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{Reciprocal~of~MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
#'  where \eqn{MCR}, \eqn{MHQ} and {HI} denotes the maximum cumulative ratio, the maximum hazard
#'  quotient and the hazard index respectively.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If several values are equal to the maximum hazard quotient, the name retained as the top hazard
#'  quotient is the first one considering the given order.
#' 
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  chart are to be created, according to classes.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param classes List or logical matrix associating the `values` names with classes.
#'  If list, its names are those present in `values` and the elements are vectors of associated classes.
#'  If logical matrix, its columns are named according to the classes and the row names
#'  contain the names associated with the `values`. A `TRUE` value indicates that a specific name
#'  is part of a specific class.
#' @inheritParams mcr_chart
#' @param log_transform If `TRUE`, the log version of the charts are created (i.e.
#'  \mjeqn{log_{10}(HI)}{log10(HI)} versus \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)}). If `FALSE`,
#'  the standard version of the charts are created (i.e. \eqn{HI} versus \eqn{MCR}).
#' @param plot If `FALSE` and the returned list is assigned, the charts are not plotted.
#'  Otherwise, the charts are all plotted in the active graphics device.
#'  But if `TRUE` and the returned list is not assigned, the charts are plotted twice.
#' @return List of charts created with the `ggplot2` package or `NULL` if no points can be plotted
#'  (see 'Details'). The length of the list corresponds to the number of classes encountered.
#' 
#' @author Gauthier Magnin
#' @inherit mcr_chart references
#' @seealso 
#' Chart independent of classes: [`mcr_chart`].
#' 
#' Generic function to apply the MCR approach according to classes: [`mcr_approach_by_class`].
#' 
#' Other functions of the MCR approach applying according to classes: [`mcr_summary_by_class`],
#'  [`thq_pairs_by_class`], [`thq_by_group_by_class`].
#' 
#' Specific indicators: [`hazard_index`], [`maximum_cumulative_ratio`], [`reciprocal_of_mcr`],
#'  [`top_hazard_quotient`], [`classify_mixture`].
#' 
#' @examples
#' ## Creating a matrix of 50*5 values and one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 1.1, by = 0.1), 250, replace = TRUE),
#'             nrow = 50, byrow = TRUE,
#'             dimnames = list(paste0("set.", 1:50), LETTERS[1:5]))
#' r <- sample(seq(1,5), 5, replace = TRUE)
#' classes <- list(A = c("C5", "C6", "C8"),
#'                 B = "C8",
#'                 C = c("C3", "C8"),
#'                 D = c("C1", "C3", "C4", "C6"),
#'                 E = c("C2", "C4", "C5", "C7", "C8"))
#' 
#' ## MCR charts on matrices
#' charts1 <- mcr_chart_by_class(v, r, classes, regions = TRUE)
#' View(charts1)
#' plot(charts1$C3)
#' 
#' charts2 <- mcr_chart_by_class(v, r, classes,
#'                               regions = TRUE, log_transform = FALSE)
#' View(charts2)
#' plot(charts2$C3)
#' 
#' ## MCR charts on lists
#' charts3 <- mcr_chart_by_class(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                                             set.2 = c(A = 0.2),
#'                                             set.3 = c(B = 0.3, C = 0.4)),
#'                               references = list(c(1, 2),
#'                                                 1,
#'                                                 c(2, 3)),
#'                               classes,
#'                               log_transform = TRUE)
#' View(charts3)
#' plot(charts3$C8)
#' 
#' charts4 <- mcr_chart_by_class(values = list(set.1 = c(A = 0.1, B = 0.5),
#'                                             set.2 = c(A = 0.2),
#'                                             set.3 = c(B = 0.3, C = 0.4)),
#'                               references = c(A = 1, B = 2, C = 3),
#'                               classes,
#'                               log_transform = FALSE)
#' View(charts4)
#' plot(charts4$C8)
#' 
#' @md
#' @export
mcr_chart_by_class = function(values, references, classes,
                              thq_col = NULL, regions = FALSE,
                              regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"), regions_alpha = 0.2,
                              regions_lab = !regions, regression = FALSE, log_transform = TRUE, plot = FALSE) {
  
  # Checking data naming and use of classes as a logical matrix
  check_data_for_mcr_by_class(values, references, vector = FALSE)
  classes = validate_classes(classes)
  
  # Booleans on all the class names to make explicit the warnings of the calls to mcr_chart
  class_warnings = stats::setNames(logical(ncol(classes)), colnames(classes))
  
  
  # For each class, one graph of the MCR indicators of the corresponding values and references
  charts = apply(classes, 2, function(column) {
    
    # Extraction of the values and references corresponding to the class
    class = colnames(classes)[parent.frame()$i[]]
    new_vr = subset_from_class(values, references, classes, class)
    
    # NA if the class is not represented (different case if values is a list or a matrix)
    if ((is.list(values) && (length(new_vr[["values"]]) == 0 || length(new_vr[["values"]][[1]]) == 0)) ||
        (is.matrix(values) && ncol(new_vr[["values"]]) == 0)) return(NA)
    
    # Catch warning without interrupting the execution of the instruction
    withCallingHandlers(return(mcr_chart(new_vr[["values"]], new_vr[["references"]],
                                         thq_col = thq_col, regions = regions,
                                         regions_col = regions_col, regions_alpha = regions_alpha,
                                         regions_lab = regions_lab, regression = regression, log_transform = log_transform,
                                         plot = plot)),
                        warning = function(w) { class_warnings[class] <<- TRUE })
    
  })
  
  # Display of a message according to the warnings encountered
  if (any(class_warnings)) {
    if (sum(class_warnings) == 1) message("One warning message in chart plotting for class \"",
                                          names(class_warnings)[class_warnings], "\":")
    else message("Warnings messages in chart plotting for classes \"",
                 paste(names(class_warnings)[class_warnings], collapse = "\", \""), "\":")
  }
  
  return(charts[!is.na(charts)])
}


#' Top Hazard Quotient pairs frequency by class
#' 
#' Build contingency tables of the counts of each combination of the top two hazard quotients pairs for
#'  which the associated hazard index is greater than 1, according to classes. For each class, one
#'  table is built from the subset of values corresponding to this class.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Values equal to 0 are ignored.
#'  
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is slightly faster.
#'  
#' \loadmathjax
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If the number of hazard quotients that are greater than or equal to the second greater hazard
#'  quotient is greater than 2, only the first two values are considered and in the order given.
#'  For example, if the hazard quotients are `D = 5`, `B = 1`, `C = 3` and `A = 3`, the top two hazard
#'  quotients considered are `D` and `C`.
#' 
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  top two hazard quotients are to be identified, according to classes.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param classes List or logical matrix associating the `values` names with classes.
#'  If list, its names are those present in `values` and the elements are vectors of associated classes.
#'  If logical matrix, its columns are named according to the classes and the row names
#'  contain the names associated with the `values`. A `TRUE` value indicates that a specific name
#'  is part of a specific class.
#' @param levels Levels to consider in the output tables. If `NULL`, only use of those that appear in the
#'  pairs.
#' @param threshold If `TRUE`, only values associated with hazard indices greater than 1 are considered.
#' @param alone If `TRUE`, take into account single top hazard quotients (i.e. sets of values of length
#'  1). If so, a level named `"NULL"` is added for such top hazard quotients.
#' @return List whose length corresponds to the number of classes encountered, containing for each class:
#' * `NULL` if among the subsets of values corresponding to the class, no one has more than one element
#'   different from 0 or a hazard index greater than 1 (accordingly to the arguments `threshold` and
#'   `alone`).
#' * Contingency table otherwise. Frequencies of pairs that produce the top two hazard quotients.
#' 
#' @author Gauthier Magnin
#' @inherit thq_pairs references
#' @seealso 
#' Contingency table independent of classes: [`thq_pairs`].
#' 
#' Generic function to apply the MCR approach according to classes: [`mcr_approach_by_class`].
#' 
#' Other functions of the MCR approach applying according to classes: [`mcr_summary_by_class`],
#'  [`mcr_chart_by_class`], [`thq_by_group_by_class`].
#' 
#' Specific indicators: [`top_hazard_quotient`], [`hazard_quotient`].
#' 
#' @examples
#' ## Creating a matrix of 50*5 values, one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 2.1, by = 0.1), 250, replace = TRUE),
#'             nrow = 50, byrow = TRUE,
#'             dimnames = list(paste0("set.", 1:50), LETTERS[1:5]))
#' r <- sample(seq(1,5), 5, replace = TRUE)
#' classes <- list(A = c("C5", "C6", "C8"),
#'                 B = "C8",
#'                 C = c("C3", "C8"),
#'                 D = c("C1", "C3", "C4", "C6"),
#'                 E = c("C2", "C4", "C5", "C7", "C8"))
#' 
#' ## Building contingency table from matrix, without and with levels parameter
#' thq_pairs_by_class(v, r, classes)
#' thq_pairs_by_class(v, r, classes, levels = names(classes))
#' 
#' ## Building contingency table from list
#' thq_pairs_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                  set.2 = c(A = 2),
#'                                  set.3 = c(B = 3, C = 4)),
#'                    references = list(c(1, 2),
#'                                      1,
#'                                      c(2, 3)),
#'                    classes)
#' thq_pairs_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                  set.2 = c(A = 2),
#'                                  set.3 = c(B = 3, C = 4)),
#'                    references = c(A = 1, B = 2, C = 3),
#'                    classes,
#'                    levels = LETTERS[1:3])
#' 
#' # Use of the parameters alone and threshold
#' thq_pairs_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                  set.2 = c(A = 2),
#'                                  set.3 = c(B = 3, C = 4)),
#'                    references = c(A = 1, B = 2, C = 3),
#'                    classes,
#'                    levels = LETTERS[1:3],
#'                    alone = TRUE)
#' thq_pairs_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                  set.2 = c(A = 2),
#'                                  set.3 = c(B = 3, C = 4)),
#'                    references = c(A = 1, B = 2, C = 3),
#'                    classes,
#'                    levels = LETTERS[1:3],
#'                    threshold = FALSE, alone = TRUE)
#' 
#' @md
#' @export
thq_pairs_by_class = function(values, references, classes,
                              levels = NULL, threshold = TRUE, alone = FALSE) {
  
  # Checking data naming and use of classes as a logical matrix
  check_data_for_mcr_by_class(values, references, vector = FALSE)
  classes = validate_classes(classes)
  
  
  # For each class, one table concerning the corresponding values and references
  tables = apply(classes, 2, function(column) {
    
    # Extraction of the values and references corresponding to the class
    new_vr = subset_from_class(values, references, classes, colnames(classes)[parent.frame()$i[]])
    
    # NA if the class is not represented (different case if values is a list or a matrix)
    if ((is.list(values) && (length(new_vr[["values"]]) == 0 || length(new_vr[["values"]][[1]]) == 0)) ||
        (is.matrix(values) && ncol(new_vr[["values"]]) == 0)) return(NA)
    
    # Return of a list to prevent tables from merging into a single matrix
    # (if levels is used and there is no NA)
    return(list(thq_pairs(new_vr[["values"]], new_vr[["references"]],
                          levels = levels, threshold = threshold, alone = alone)))
  })
  
  # Unlisting is not always to be done
  if ("list" %in% sapply(tables, class)) tables = lapply(tables, "[[", 1)
  return(tables[!is.na(tables)])
}


#' Top Hazard Quotients frequency by group and by class
#' 
#' Build contingency tables of the counts of each combination of name of the element producing the
#'  top hazard quotient with the associated MIAT group, according to classes. For each class, one table
#'  is built from the subset of values corresponding to this class. The groups describe the following
#'  situations:
#'  * Group I: the mixture presents a potential risk already based on individual components.
#'  * Group II: the assessment does not identify a concern.
#'  * Group IIIA: the majority of the risk offered by the mixture is driven by one component.
#'  * Group IIIB: the potential risk is driven by multiple components.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#'  
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is slightly faster.
#' 
#' \loadmathjax
#' The mixtures are assigned to the groups according the following conditions:
#' * Group I: \mjeqn{MHQ_i \ge 1}{MHQ_i >= 1}
#' * Group II: \mjeqn{MHQ_i < 1, HI_i \le 1}{MHQ_i < 1, HI_i <= 1}
#' * Group IIIA: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i < 2}{MHQ_i < 1, HI_i > 1, MCR_i < 2}
#' * Group IIIB: \mjeqn{MHQ_i < 1, HI_i > 1, MCR_i \ge 2}{MHQ_i < 1, HI_i > 1, MCR_i >= 2}
#' 
#' The maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{MCR_i = \frac{HI_i}{MHQ_i}}{MCR_i = HI_i / MHQ_i}
#'  where \eqn{HI} denotes the hazard index and \eqn{MHQ} denotes the maximum hazard quotient.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^n HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to n}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#'  
#' The maximum hazard quotient of the vector \eqn{i} is given by:
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,n\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{n} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' If several values are equal to the maximum hazard quotient, the name retained as the top hazard
#'  quotient is the first one considering the given order.
#' 
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  tables are to be built, according to classes.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param classes List or logical matrix associating the `values` names with classes.
#'  If list, its names are those present in `values` and the elements are vectors of associated classes.
#'  If logical matrix, its columns are named according to the classes and the row names
#'  contain the names associated with the `values`. A `TRUE` value indicates that a specific name
#'  is part of a specific class.
#' @param levels Levels to consider in the output tables. If `NULL`, only use of those that appear in the
#'  top hazard quotients.
#' @return List of contingency tables: frequencies of elements that produce the top hazard quotient with
#'  their associated groups. The length of the list corresponds to the number of classes encountered.
#' 
#' @author Gauthier Magnin
#' @inherit thq_by_group references
#' @seealso 
#' Contingency table independent of classes: [`thq_by_group`].
#' 
#' Generic function to apply the MCR approach according to classes: [`mcr_approach_by_class`].
#' 
#' Other functions of the MCR approach applying according to classes: [`mcr_summary_by_class`],
#'  [`mcr_chart_by_class`], [`thq_pairs_by_class`].
#' 
#' Specific indicators: [`classify_mixture`], [`top_hazard_quotient`], [`hazard_quotient`].
#' 
#' @examples
#' ## Creating a matrix of 50*50 values, one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 2.1, by = 0.1), 250, replace = TRUE),
#'             nrow = 50, byrow = TRUE,
#'             dimnames = list(paste0("set.", 1:50), LETTERS[1:5]))
#' r <- sample(seq(1,5), 5, replace = TRUE)
#' classes <- list(A = c("C5", "C6", "C8"),
#'                 B = "C8",
#'                 C = c("C3", "C8"),
#'                 D = c("C1", "C3", "C4", "C6"),
#'                 E = c("C2", "C4", "C5", "C7", "C8"))
#' 
#' ## Building contingency table from matrix, without and with levels parameter
#' thq_by_group_by_class(v, r, classes)
#' thq_by_group_by_class(v, r, classes, levels = names(classes))
#' 
#' ## Building contingency table from list
#' thq_by_group_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                     set.2 = c(A = 2),
#'                                     set.3 = c(B = 3, C = 4)),
#'                       references = list(c(1, 2),
#'                                         1,
#'                                         c(2, 3)),
#'                       classes)
#' thq_by_group_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                     set.2 = c(A = 2),
#'                                     set.3 = c(B = 3, C = 4)),
#'                       references = c(A = 1, B = 2, C = 3),
#'                       classes,
#'                       levels = LETTERS[1:3])
#' 
#' @md
#' @export
thq_by_group_by_class = function(values, references, classes,
                                 levels = NULL) {
  
  # Checking data naming and use of classes as a logical matrix
  check_data_for_mcr_by_class(values, references, vector = FALSE)
  classes = validate_classes(classes)
  
  
  # For each class, one table concerning the corresponding values and references
  tables = apply(classes, 2, function(column) {
    
    # Extraction of the values and references corresopnding to the class
    new_vr = subset_from_class(values, references, classes, colnames(classes)[parent.frame()$i[]])
    
    # NA if the class is not represented (different case if values is a list or a matrix)
    if ((is.list(values) && (length(new_vr[["values"]]) == 0 || length(new_vr[["values"]][[1]]) == 0)) ||
        (is.matrix(values) && ncol(new_vr[["values"]]) == 0)) return(NA)
    
    # Return of a list to prevent tables from merging into a single matrix
    # (if levels is used and there is no NA)
    return(list(thq_by_group(new_vr[["values"]], new_vr[["references"]], levels = levels)))
  })
  
  # Unlisting is not always to be done
  if ("list" %in% sapply(tables, class)) tables = lapply(tables, "[[", 1)
  return(tables[!is.na(tables)])
}


#' MCR approach by class
#' 
#' Perform the MCR approach according to classes, given values and references. Wrapper of the four
#'  functions allowing to perform the MCR approach according to classes: `mcr_summary_by_class`,
#'  `mcr_chart_by_class`, `thq_pairs_by_class`, `thq_by_group_by_class`.
#' 
#' @details
#' If `values` is a vector, the reference values are directly associated with these values.
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each row.
#'  Therefore, there must be one reference value for each column of the matrix.
#'  
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is slightly faster.
#' 
#' @param values Numeric named vector or matrix, or list of numeric named vectors.
#'  Values on which to perform the MCR approach according to classes.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param classes List or logical matrix associating the `values` names with classes.
#'  If list, its names are those present in `values` and the elements are vectors of associated classes.
#'  If logical matrix, its columns are named according to the classes and the row names
#'  contain the names associated with the `values`. A `TRUE` value indicates that a specific name
#'  is part of a specific class.
#' @param FUN Either a function or a non-empty character string naming the function to apply on each
#'  class, among `mcr_summary`, `mcr_chart`, `thq_pairs`, `thq_by_group`.
#' @param ... Further arguments to the function to which `FUN` refers.
#' @return See 'Value' of the corresponding help page:
#'  * [`mcr_summary_by_class`] if `FUN` is `mcr_summary`.
#'  * [`mcr_chart_by_class`] if `FUN` is `mcr_chart`.
#'  * [`thq_pairs_by_class`] if `FUN` is `thq_pairs`.
#'  * [`thq_by_group_by_class`] if `FUN` is `thq_by_group`.
#' 
#' @author Gauthier Magnin
#' @seealso [`mcr_summary_by_class`], [`mcr_chart_by_class`], [`thq_pairs_by_class`],
#'          [`thq_by_group_by_class`].
#' 
#' @examples
#' ## Creating a matrix of 50*5 values, one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 2.1, by = 0.1), 250, replace = TRUE),
#'             nrow = 50, byrow = TRUE,
#'             dimnames = list(paste0("set.", 1:50), LETTERS[1:5]))
#' r <- sample(seq(1,5), 5, replace = TRUE)
#' classes <- list(A = c("C5", "C6", "C8"),
#'                 B = "C8",
#'                 C = c("C3", "C8"),
#'                 D = c("C1", "C3", "C4", "C6"),
#'                 E = c("C2", "C4", "C5", "C7", "C8"))
#' 
#' ## Application to the 4 possible functions
#' mcr_approach_by_class(values = c(A = 1, B = 2, C = 3, D = 4, E = 5),
#'                       references = r,
#'                       classes,
#'                       mcr_summary)
#' 
#' mcr_approach_by_class(v, r, classes, mcr_chart,
#'                       regions_lab = c(TRUE, TRUE, FALSE, TRUE),
#'                       regression = TRUE,
#'                       log_transform = FALSE)$C8
#' 
#' mcr_approach_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                     set.2 = c(A = 2),
#'                                     set.3 = c(B = 3, C = 4)),
#'                       references = list(c(1, 2),
#'                                         1,
#'                                         c(2, 3)),
#'                       classes,
#'                       FUN = "thq_pairs", levels = names(classes))
#' 
#' mcr_approach_by_class(values = list(set.1 = c(A = 1, B = 5),
#'                                     set.2 = c(A = 2),
#'                                     set.3 = c(B = 3, C = 4)),
#'                       references = c(A = 1, B = 2, C = 3),
#'                       classes,
#'                       FUN = "thq_by_group", levels = names(classes))
#' 
#' @md
#' @export
mcr_approach_by_class = function(values, references, classes, FUN, ...) {
  
  # Verification of the choice of the function (conversion to a character string)
  functions = c("mcr_summary", "mcr_chart", "thq_pairs", "thq_by_group")
  if (class(FUN) == "function") FUN = deparse(substitute(FUN))
  if (!(FUN %in% functions))
    stop("FUN must refer to one of the following functions: ", paste(functions, collapse = ", "), ".")
  
  # Call to the relevant function
  return(do.call(paste0(FUN, "_by_class"),
                 list(values, references, classes, ...)))
}



#### Subsets and reduction of sets ####

#' Subsets of values and references by class
#' 
#' Extract from values and references those corresponding to one specific class.
#' 
#' @details
#' If `values` is a vector, it must have one reference value for each value of the vector.
#' 
#' If `values` is a matrix, it must have one reference value for each column.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is slightly faster.
#' 
#' @param values Numeric named vector or matrix, or list of numeric named vectors.
#'  Values from which to extract a subset according to one specific class.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param classes List or logical matrix associating the `values` names with classes.
#'  If list, its names are those present in `values` and the elements are vectors of associated classes.
#'  If logical matrix, its columns are named according to the classes and the row names
#'  contain the names associated with the `values`. A `TRUE` value indicates that a specific name
#'  is part of a specific class.
#' @param class_name Name of the class of interest, the one for which to extract the corresponding
#'  subsets of `values` and `references`.
#' @return
#' If `references` is `NULL`, subset of values that correspond to the class of interest.\cr
#' If not, list containing:
#' \describe{
#'  \item{`values`}{Subset of values that correspond to the class of interest.}
#'  \item{`references`}{Subset of references that correspond to the class of interest.}
#' }
#' 
#' @author Gauthier Magnin
#' @seealso 
#' Generic function to apply the MCR approach according to classes: [`mcr_approach_by_class`].
#' 
#' Specific functions of the MCR approach applying according to classes: [`mcr_summary_by_class`],
#'  [`mcr_chart_by_class`], [`thq_pairs_by_class`], [`thq_by_group_by_class`].
#' 
#' @examples
#' ## Association of classes (C1 to C8) with elements A, B, C, D and E
#' classes <- list(A = c("C5", "C6", "C8"),
#'                 B = "C8",
#'                 C = c("C3", "C8"),
#'                 D = c("C1", "C3", "C4", "C6"),
#'                 E = c("C2", "C4", "C5", "C7", "C8"))
#' 
#' ## Subsets with values as a vector
#' subset_from_class(values = c(A = 1, B = 2, C = 3, D = 4, E = 5),
#'                   classes = classes,
#'                   class_name = "C8")
#' 
#' ## Subsets with values as a matrix
#' subset_from_class(
#'   values = matrix(c(0.1,0.2,0.3,0.4,0.5,
#'                     0.6,0.7,0.8,0.9,1.),
#'                   nrow = 2, byrow = 2,
#'                   dimnames = list(c("set.1", "set.2"), LETTERS[1:5])),
#'   references = c(1,2,3,4,5),
#'   classes = classes,
#'   class_name = "C8"
#' )
#' 
#' ## Subsets with values as a list
#' v <- list(set.1 = c(A = 0.1, D = 0.5),
#'           set.2 = c(D = 0.2),
#'           set.3 = c(A = 0.3, C = 0.4))
#' 
#' subset_from_class(values = v,
#'                   classes = classes,
#'                   class_name = "C8")
#' 
#' subset_from_class(values = v,
#'                   references = c(A = 1, C = 3, D = 4),
#'                   classes = classes,
#'                   class_name = "C8")
#' 
#' subset_from_class(values = v,
#'                   references = list(c(1, 4),
#'                                     4,
#'                                     c(1, 3)),
#'                   classes = classes,
#'                   class_name = "C8")
#' 
#' @md
#' @export
subset_from_class = function(values, references = NULL, classes, class_name) {
  
  # Checking data naming and use of classes as a logical matrix
  check_data_for_mcr_by_class(values, references)
  classes = validate_classes(classes)
  
  # Names of the elements corresponding to the class
  items_in_class = rownames(classes)[classes[, class_name]]
  
  
  # Case of a list of values
  if (is.list(values)) {
    
    # Subsets of references corresponding to the elements of the class
    if (!is.null(references)) {
      
      if (is.list(references)) {
        references_class = list()
        
        # Adding to the new list of references of each element that is part of the class
        for (i in seq_along(values)) {
          indices = names(values[[i]]) %in% items_in_class
          
          # Does not consider sets containing no element of the class
          if (sum(indices) != 0) {
            name_element = if (is.null(names(values))) length(references_class) + 1 else names(values)[i]
            references_class[[name_element]] = references[[i]][indices]
          }
        }
        names(references_class) = names(references)[which(names(values) %in% names(references_class))]
      } else {
        # If references is a single vector
        references_class = references[items_in_class]
        # Removal of NA values (when names associated with the class are not part of the references)
        references_class = references_class[!is.na(references_class)]
      }
    }
    
    # Subsets of values corresponding to the elements of the class
    values_items = turn_list_into_logical_matrix(lapply(values, names))
    values_items_class = values_items[, items_in_class[items_in_class %in% colnames(values_items)],
                                      drop = FALSE]
    values_class = lapply(seq_len(nrow(values_items_class)), function(r) {
      row = values_items_class[r, ]
      if (ncol(values_items_class) == 1) names(row) = colnames(values_items_class)
      return(values[[r]][names(values[[r]]) %in% names(row)[row]])
      # Use this instead of values[[r]][names(row)[row]] because values can have duplicate names
    })
    names(values_class) = names(values)
    
    # Removal of empty sets, containing no element of the class
    values_class = values_class[lengths(values_class) != 0]
    if (length(values_class) == 0) values_class = stats::setNames(list(), character(0))
  }
  # Case of a matrix of values
  else if (is.matrix(values)) {
    
    # Extraction of the values corresponding to the class
    indices = which(colnames(values) %in% items_in_class)
    values_class = values[, indices]
    
    if (!is.null(references)) references_class = references[indices]
    
    # Convert back to matrix if there was only one column
    if (is.vector(values_class)) {
      values_class = matrix(values_class, ncol = 1)
      colnames(values_class) = colnames(values)[indices]
      rownames(values_class) = rownames(values)
    }
  }
  # Case of a vector of values
  else {
    
    # Extraction of the values corresponding to the class
    indices = which(names(values) %in% items_in_class)
    values_class = values[indices]
    
    if (!is.null(references)) references_class = references[indices]
  }
  
  if (is.null(references)) return(values_class)
  return(list(values = values_class, references = references_class))
}


#' Reduce sets according to the names of their components
#' 
#' For each set of values, apply a function to merge the ones having the same names or to choose one
#'  of them, for each different name found in the set of values. This results in sets of values having
#'  only one value per different name found in the original sets, instead of several ones.
#' 
#' @details
#' If `values` is a vector, the reference values are directly associated with these values.
#' 
#' If `values` is a matrix, the reference values are applied once on each set of values, i.e. on each
#'  row. Therefore, there must be one reference value for each column of the matrix.
#'  
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, it is not processed and is returned as is. Otherwise `references` must
#'  be a list of vectors having the same lengths as those present in `values` so that `values` and
#'  `references` can be matched.
#' 
#' It is assumed that the same reference values are given for same value names of the argument `values`.
#' 
#' The components of each resulting set of values are ordered in the order in which the names are found
#'  in the original set. This is also the case for the references.
#'  
#' @param values Numeric named vector or matrix, or list of numeric named vectors.
#'  Sets of values to reduce.
#' @param references Optional. Numeric vector or list of numeric vectors. Reference values associated
#'  with the `values`.
#' @param FUN Function to apply on each subset of values corresponding to each different name to reduce
#'  it to a single value.
#' @param ignore_zero `TRUE` or `FALSE` whether to ignore values equal to 0.
#' @param ... Further arguments to the function `FUN`.
#' @return 
#' If `references` is `NULL`, sets of values resulting from the application of the function given as
#'  argument on each subset corresponding to each different name associated with the values.\cr
#' If not, list containing:
#' \describe{
#'  \item{`values`}{Sets of values resulting from the application of the function given as argument
#'                  on each subset corresponding to each different name associated with the values.}
#'  \item{`references`}{Subset of references corresponding to the resulting values.}
#' }
#' 
#' @author Gauthier Magnin
#' @examples
#' ## Reduce a set of values given as a vector
#' reduce_sets(values = c(A = 1, A = 3, B = 2, C = 1, C = 5, C = 4),
#'             FUN = max)
#' reduce_sets(values = c(C = 1, C = 5, C = 4, B = 2, A = 1, A = 3),
#'             FUN = function(x) min(x) + 0.5)
#' 
#' ## Reduce sets of values given as a matrix
#' v <- matrix(sample(seq(0.1, 1, by = 0.1), 50, replace = TRUE),
#'             nrow = 10, byrow = TRUE,
#'             dimnames = list(paste0("set.", 1:10), c("A", "B", "A", "C", "C")))
#' reduce_sets(values = v, FUN = mean)
#' 
#' ## Reduce sets of values and associated reference values given as lists
#' reduce_sets(
#'   values = list(set.1 = c(A = 0.1, A = 0.3, B = 0.5),
#'                 set.2 = c(A = 0.2),
#'                 set.3 = c(B = 0.3, B = 0.4, B = 0.7, C = 0.4, C = 0.1)),
#'   references = list(c(1, 1, 2),
#'                     1,
#'                     c(2, 2, 2, 3, 3)),
#'   FUN = median
#' )
#' 
#' @md
#' @export
reduce_sets = function(values, references = NULL, FUN, ignore_zero = TRUE, ...) {
  
  # Checking that the values data structure is named
  if (is.list(values)) {
    if (!is_named(values)[2]) stop("If values is a list, it must contain vectors of named numeric values.")
  }
  else if (is.vector(values) && !is_named(values)) stop("If values is a vector, it must have named numeric values.")
  else if (is.matrix(values) && !is_named(values)[2]) stop("If values is a matrix, its columns must be named.")
  
  # Verifications relating to the references
  if (is.list(references)) {
    if (!is.list(values)) stop("If references is a list, values must also be a list.")
    
    if (length(values) != length(references)
        || any(sapply(values, length) != sapply(references, length)))
      stop("If values and references are two lists, their lengths and the ones of their elements must match.")
  }
  
  
  # New sets of values; different case depending on list/vector/matrix
  if (is.list(values)) {
    # Recursion for each element of the list (each set of values)
    new_values = lapply(values, function(v) {
      to_return = stats::setNames(reduce_sets(v, FUN = FUN, ignore_zero = ignore_zero, ...),
                                  unique(names(v)))
      if (ignore_zero) return(unlist(to_return))
      return(to_return)
    })
  }
  else if (is.vector(values)) {
    # Apply the function to each subset of vector corresponding to each name
    new_values = sapply(unique(names(values)), function(name) {
      vn = values[which(names(values) == name)]
      if (ignore_zero) {
        vn = vn[vn != 0]
        if (length(vn) == 0) return(vn)
      }
      return(FUN(vn, ...))
    })
  }
  else if (is.matrix(values)) {
    # Setting a wrapper to the given function to ignore zeros
    if (ignore_zero) {
      FUN_to_apply = function(x, ...) {
        x = x[x != 0]
        if (length(x) == 0) return(0)
        return(FUN(x, ...))
      }
    } else FUN_to_apply = FUN
    
    # For each subset of the matrix corresponding to each name
    new_values = sapply(unique(colnames(values)), function(name) {
      # Application of the function to each row
      cols = which(colnames(values) == name)
      if (length(cols) == 1) return(sapply(values[, cols], FUN_to_apply, ...))
      return(apply(values[, cols], 1, FUN_to_apply, ...))
    })
  }
  
  # Return if references is not given
  if (is.null(references)) return(new_values)
  
  # New references
  if (is.list(references)) { # values and references are lists
    # For each set of values
    new_references = lapply(values, function(v) {
      # Finding the indices of the first elements associated with each name
      indices = sapply(unique(names(v)), function(name) which(names(v) == name)[1])
      # Keep the references associated with these indices only
      return(references[[parent.frame()$i[]]][indices])
    })
    names(new_references) = names(references)
  }
  else if (is.matrix(values)) { # values is matrix
    new_references = references[match(colnames(new_values), colnames(values))]
  }
  else if (!is.list(values)) { # values is vector
    new_references = references[match(names(new_values), names(values))]
  }
  else if (is.list(values)) { # values is list and references is vector
    new_references = references
  }
  
  return(list(values = new_values, references = new_references))
}


