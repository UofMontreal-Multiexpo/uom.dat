#' @include list_manager.R utils.R
NULL


#### Maximum Cumulative Ratio approach - main indicators ####

#' Hazard Quotient (HQ)
#' 
#' Compute the hazard quotient as the ratio between a value and a reference value.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
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
#' hazard_quotient(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                 ncol = 2, dimnames = list(LETTERS[1:5])),
#'                 references = c(1,2,3,4,5))
#' 
#' @md
#' @export
hazard_quotient = function(values, references) {
  
  if (is.vector(values) && length(values) != length(references))
    stop("values and references must be the same length.")
  if (is.matrix(values) && nrow(values) != length(references))
    stop("Length of references must be equal to the number of rows of values.")
  
  # Distinction between vector and matrix cases
  if (is.vector(values)) return(values / references)
  if (is.matrix(values)) {
    hq = apply(values, 2, function(column) column / references)
    
    # Case where the matrix contains only one set of values, the result of apply is a vector
    if (is.vector(hq)) {
      hq = matrix(hq, nrow = 1)
      rownames(hq) = rownames(values)
    }
    
    colnames(hq) = colnames(values)
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` or `hq` is a matrix, one hazard index is computed on each column.
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
#' hazard_index(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), ncol = 2),
#'              references = c(1,2,3,4,5))
#' 
#' @md
#' @export
hazard_index = function(values = NULL, references = NULL,
                        hq = NULL) {
  
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  # Distinction between vector and matrix cases
  if (is.vector(hq)) return(sum(hq))
  if (is.matrix(hq)) return(colSums(hq))
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` or `hq` is a matrix, one maximum hazard quotient is searched for each column.
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
#' maximum_hazard_quotient(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                         ncol = 2),
#'                         references = c(1,2,3,4,5))
#' 
#' @md
#' @export
maximum_hazard_quotient = function(values = NULL, references = NULL,
                                   hq = NULL) {
  
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  # Distinction between vector and matrix cases
  if (is.vector(hq)) return(max(hq))
  if (is.matrix(hq)) return(apply(hq, 2, "max"))
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a matrix (or `hi` and `mhq` are vectors larger than 1), one maximum cumulative ratio
#'  is computed for each column (or value, respectively).
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
#' @param hi Numeric value or vector. **H**azard **i**ndex(es) on which to compute the maximum cumulative
#'  ratio(s).
#' @param mhq Numeric value or vector. **M**aximum **h**azard **q**uotient(s) associated with the hazard
#'  index(es) `hi`.
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a matrix (or `mcr` is a vector larger than 1), one missed toxicity value is computed
#'  for each column (or value, respectively).
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a matrix (or `hi` and `mhq` are vectors larger than 1, or `mcr` is such a vector),
#' one reciprocal of maximum cumulative ratio is computed for each column (or value, respectively).
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
#' @param hi Numeric value or vector. **H**azard **i**ndex(es) for which to compute the reciprocal of
#'  maximum cumulative ratio(s).
#' @param mhq Numeric value or vector. **M**aximum **h**azard **q**uotient(s) associated with the hazard
#'  index(es) `hi`.
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
#' reciprocal_of_mcr(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                   ncol = 2),
#'                   references = c(1,2,3,4,5))
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` or `hq` is a matrix, `k` hazard quotients are highlight for each column.
#' 
#' If the number of hazard quotients that are greater than or equal to the \eqn{k-th} greater hazard
#'  quotient is greater than `k`, only the first `k` values are considered and in the order given.
#'  For example, if `hq = c(D = 5, B = 1, C = 3, A = 3)` and `k = 2`, the return is `c(D = 5, C = 3)`.
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
#'  identify.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hq Numeric named vector or matrix. **H**azard **q**uotients whose highest values are to be
#'  identify.
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
#' top_hazard_quotient(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                     ncol = 2, dimnames = list(LETTERS[1:5])),
#'                     references = c(1,2,3,4,5))
#' 
#' @md
#' @export
top_hazard_quotient = function(values = NULL, references = NULL,
                               hq = NULL,
                               k = NULL) {
  
  if (!is.null(k) && k < 1) stop("k must be greater than 0.")
  if (is.null(hq)) hq = hazard_quotient(values, references)
  
  if (is.matrix(hq)) {
    thq_list = apply(hq, 2, function(column) list(top_hazard_quotient(hq = column, k = k)))
    return(lapply(thq_list, "[[", 1))
  }
  
  if (is.null(k)) {
    # Default value: integer part of the maximum cumulative ratio
    k = trunc(maximum_cumulative_ratio(hi = hazard_index(hq = hq),
                                       mhq = maximum_hazard_quotient(hq = hq)))
  } else if (k > length(hq)) {
    k = length(hq)
  }
  
  if (k == 1) return(hq[which.max(hq)])
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
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
#'  before searching for the maximum hazard quotients, computing the maximum cumulative ratios then
#'  classifying the mixtures. Thus, call the function with the arguments `hi` and `mhq` is faster and
#'  call it with the argument `mcr` is even faster (if they are already computed).
#' 
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a matrix (or `hi`, `mhq` and `mcr` are vectors larger than 1), one class is assigned
#'  for each column (or each value, respectively).
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
#' @param hi Numeric value or vector. **H**azard **i**ndex(es) of the mixture(s) to classify.
#' @param mhq Numeric value or vector. **M**aximum **h**azard **q**uotient(s) associated with the hazard
#'  index(es) `hi`.
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
#' classify_mixture(values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                                  ncol = 3),
#'                  references = c(1,2,3,0.5))
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#'  
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
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
#'  * **n**: number of values.
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
#' mcr_summary(values = matrix(sample(seq(0.1, 1, by = 0.1), 50, replace = TRUE),
#'                             ncol = 10, dimnames = list(LETTERS[1:5])),
#'             references = sample(seq(1,5), 5, replace = TRUE))
#' 
#' ## MCR summary on lists
#' mcr_summary(values = list(c(A = 0.1, B = 0.5),
#'                           c(A = 0.2),
#'                           c(B = 0.3, C = 0.4)),
#'             references = c(A = 1, B = 2, C = 3))
#' mcr_summary(values = list(c(A = 0.1, B = 0.5),
#'                           c(A = 0.2),
#'                           c(B = 0.3, C = 0.4)),
#'             references = list(c(1, 2),
#'                               1,
#'                               c(2, 3)))
#' 
#' @md
#' @export
mcr_summary = function(values, references) {
  
  # Specific case in which values is a list
  if (is.list(values)) return(mcr_summary_for_list(values, references))
  
  # Computations of the indicators
  hq = hazard_quotient(values, references)
  hi = hazard_index(hq = hq)
  mhq = maximum_hazard_quotient(hq = hq)
  mcr = maximum_cumulative_ratio(hi = hi, mhq = mhq)
  mt = missed_toxicity(mcr = mcr)
  
  rmcr = reciprocal_of_mcr(mcr = mcr)
  groups = classify_mixture(hi = hi, mhq = mhq, mcr = mcr)
  
  # Distinction between vector and matrix cases
  if (is.vector(values)) {
    thq = if (is_named(values)) names(top_hazard_quotient(hq = hq, k = 1)) else NA
    return(list(n = length(values),
                HI = hi, MCR = mcr, Reciprocal = rmcr, Group = groups,
                THQ = thq, MHQ = mhq, Missed = mt))
  }
  thq = if (is_named(values)[1]) names(unlist(unname(top_hazard_quotient(hq = hq, k = 1)))) else NA
  return(data.frame(n = apply(values, 2, length),
                    HI = hi, MCR = mcr, Reciprocal = rmcr, Group = groups,
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
#'  * **n**: number of values.
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
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
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
#'  before searching for the top and maximum hazard quotients, computing the maximum cumulative ratios
#'  then plot the chart. Thus, call the function with the arguments `hi`, `mcr` and `thq` is faster
#'  (if they are already computed). This is true only if `values` is not a list.
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
#' @param mcr Numeric vector. **M**aximum **c**umulative **r**atios associated with the hazard indexes
#'  `hi`.
#' @param thq Numeric named vector or list of numeric named vectors. **T**op **h**azard **q**uotients
#'  associated with the hazard indexes `hi`. If list, only the first named value of each element of the
#'  list is considered.
#' @param thq_col Character named vector. Colors to assign to the **t**op **h**azard **q**uotients
#'  elements.
#' @param regions If `TRUE`, the regions corresponding to the MIAT groups are filled with the colors
#'  defined by `regions_col`.
#' @param regions_col Character vector of length 4. Define the colors for the regions of the MIAT groups
#'  (in order: I, II, IIIA and IIIB).
#' @param regions_alpha Value between 0 and 1. Transparency of the regions filled with `regions_col`.
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
#' ## Creating a matrix of 5*50 values and one reference value for each of the 5
#' ## elements (A, B, C, D and E).
#' v <- matrix(sample(seq(0.1, 1.1, by = 0.1), 250, replace = TRUE),
#'             ncol = 50, dimnames = list(LETTERS[1:5]))
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
#' mcr_chart(values = list(c(A = 0.1, B = 0.5),
#'                         c(A = 0.2),
#'                         c(B = 0.3, C = 0.4)),
#'           references = c(A = 1, B = 2, C = 3),
#'           log_transform = FALSE)
#' mcr_chart(values = list(c(A = 0.1, B = 0.5),
#'                         c(A = 0.2),
#'                         c(B = 0.3, C = 0.4)),
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
  
  # Cas spécifiques dans lequel values est une liste et non une matrice
  if (is.list(values)) {
    
    # Différence si references est une liste ou un vecteur
    if (is.list(references)) {
      
      # Vérification que les structures de données sont nommées
      if (length(values) != length(references) ||
          any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, their lengths and the ones of their elements must match.")
      if (!is_named(values)[2])
        stop("If values is a list, it must contain vectors of named numeric values.")
      
      # Calcul des indicateurs nécessaires
      hi = sapply(seq_len(length(values)), function(i) hazard_index(values[[i]], references[[i]]))
      mcr = sapply(seq_len(length(values)), function(i) maximum_cumulative_ratio(values[[i]], references[[i]]))
      thq = sapply(seq_len(length(values)), function(i) top_hazard_quotient(values[[i]], references[[i]], k = 1))
      
    } else if (is.vector(references)) {
      
      # Vérification que les structures de données sont nommées
      if (!is_named(references) || !is_named(values)[2])
        stop("If values is a list and references is a vector. Both must contained named values.")
      
      # Calcul des indicateurs nécessaires
      hi = sapply(values, function(v) hazard_index(v, references[names(v)]))
      mcr = sapply(values, function(v) maximum_cumulative_ratio(v, references[names(v)]))
      thq = sapply(unname(values), function(v) top_hazard_quotient(v, references[names(v)], k = 1))
      
    } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
    
  } else { # Cas où values est une matrice ou n'est pas renseigné
    
    # Vérification que les structures de données sont nommées
    if (!is.null(values) && !is_named(values)[1]) stop("Rows of values must be named.")
    if (!is.null(thq) && ((is.list(thq) && !is_named(thq)[2]) || !is_named(thq)))
      stop("thq must be a vector of named numeric values or a list of such vectors.")
    
    # Calcul des données manquantes
    if (is.null(hi)) hi = hazard_index(values, references)
    if (is.null(mcr)) mcr = maximum_cumulative_ratio(values, references, hi = hi)
    if (is.null(thq)) thq = top_hazard_quotient(values, references, k = 1)
  }
  
  
  # Récupération des noms des top
  thq = if (is.list(thq)) sapply(unname(thq), function(v) names(v)[1]) else names(thq)
  
  # Préparation des données et limites du graphique
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
  
  # Initialisation du graphique (valeurs, thème et cadre)
  chart = ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  
  # Partie du graphique spécifique au type log ou normal
  fun.chart = if (log_transform) plot_mcr_log_part else plot_mcr_standard_part
  chart = fun.chart(chart, xlim, ylim, regions, regions_col, regions_alpha, regions_lab)
  # Default region colors are colorblind safe and print friendly
  
  # Si des couleurs spécifiques doivent être associés aux THQ
  if (!is.null(thq_col)) chart = chart + ggplot2::scale_color_manual(values = thq_col)
  
  # Régression linéaire
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
  
  # Fonction de délimitation du groupe I
  fun.mhq_1 = function(x) log10(10^x - 1)
  xmin_fun = 0.001
  xmax_fun = max(xmin_fun, xlim[2]) + 1
  root_fun = stats::uniroot(fun.mhq_1, c(0, 1))$root   # fun.mhq_1(log10(2)) = 0
  
  # Pour placement du label "Group IIIB" : abscisse maximale de la courbe fun.mhq_1 selon la limite
  # fixée par ylim (fonction inverse de fun.mhq_1 := log10(10^y + 1))
  xmax_fun.mhq_1_visible = log10(10^ylim[2] + 1)
  x_to_use = if (xlim[2] <= xmax_fun.mhq_1_visible) xlim[2] else xmax_fun.mhq_1_visible
  x_groupIIIB = if (xlim[1] > 0) (x_to_use + xlim[1]) / 2 else x_to_use / 2
  
  # Texte relatif aux groupes
  if (any(regions_lab)) {
    # Vérification des zones affichées (non-affichage du texte des zones qui ne sont pas affichées)
    regions_lab = regions_lab & c(
      xlim[2] > 0 && ylim[1] < fun.mhq_1(xlim[2]), # Coin bas-droite en-dessous de la courbe f
      xlim[1] < 0,
      ylim[1] < 0 && xlim[2] > 0 && (xlim[1] < 0 || ylim[1] > fun.mhq_1(xlim[1])), # Coin bas-gauche au dessus de f
      ylim[2] > 0 && xlim[2] > 0 && (xlim[1] < 0 || ylim[2] > fun.mhq_1(xlim[1])) # Coin haut-gauche au dessus de f
    )
    
    chart = chart + ggplot2::annotate(geom = "text",
                                      x = c(xlim[2], xlim[1], root_fun / 2, x_groupIIIB)[regions_lab],
                                      y = c(ylim[1], ylim[1], -0.05, ylim[2])[regions_lab],
                                      hjust = c(1, 0, 0.5, 0.5)[regions_lab],
                                      vjust = c(0, 0, 0.5, 1)[regions_lab],
                                      label = c("Group I", "Group II", "Group IIIA", "Group IIIB")[regions_lab])
  }
  
  # Coloration des régions
  if (regions) {
    chart = chart +
      # A gauche, groupe II
      ggplot2::geom_polygon(data = data.frame(x = c(-Inf, -Inf, 0, 0),
                                              y = c(-Inf, Inf, Inf, -Inf)),
                            ggplot2::aes(fill = "II"),
                            alpha = regions_alpha) +
      # A droite, groupe I
      ggplot2::geom_polygon(data = data.frame(x = c(seq(0, xmax_fun, by = 0.001), xmax_fun),
                                              y = c(-Inf, fun.mhq_1(seq(xmin_fun, xmax_fun, by = 0.001)), -Inf)),
                            ggplot2::aes(fill = "I"),
                            alpha = regions_alpha) +
      # Au centre, groupe IIIA
      ggplot2::geom_polygon(data = data.frame(x = c(0, seq(0, root_fun, by = 0.001)),
                                              y = c(0, -Inf, fun.mhq_1(seq(xmin_fun, root_fun, by = 0.001)))),
                            ggplot2::aes(fill = "IIIA"),
                            alpha = regions_alpha) +
      # En haut, groupe IIIB
      ggplot2::geom_polygon(data = data.frame(x = c(0, 0, seq(root_fun, xmax_fun, by = 0.001), xmax_fun, xmax_fun),
                                              y = c(Inf, 0, fun.mhq_1(seq(root_fun, xmax_fun, by = 0.001)), fun.mhq_1(xmax_fun), Inf)),
                            ggplot2::aes(fill = "IIIB"),
                            alpha = regions_alpha) +
      # Légende associée
      ggplot2::scale_fill_manual(values = stats::setNames(regions_col, c("I", "II", "IIIA", "IIIB")),
                                 name = "MIAT groups",
                                 guide = ggplot2::guide_legend(override.aes = list(color = "black",
                                                                                   linetype = "longdash")))
  }
  
  # Suite du graphique (points, délimitations, légende, labels axe Y)
  chart = chart +  ggplot2::geom_point(ggplot2::aes(color = factor(thq))) +
    # Segment horizontal séparant IIIA et IIIB
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = root_fun, yend = 0),
                          color = "black", linetype = "longdash") +
    # Droite vertical séparant le groupe II du reste
    ggplot2::geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
    # Courbe séparant le groupe I du reste
    ggplot2::stat_function(fun = fun.mhq_1, xlim = c(xmin_fun, xmax_fun),
                           color = "black", linetype = "longdash") +
    # Titres des axes et légende
    ggplot2::labs(x = bquote(log[10]*"(HI)"),
                  y = bquote(atop(log[10]*"(MCR - 1)", "MHQ / HI")),
                  col = "Top Hazard Quotients") +
    # Ajout de la réciproque de MCR aux labels de l'axe Y
    ggplot2::scale_y_continuous(labels = function(y) {
      return(paste0(format(round(y, 1), nsmall = 1), "\n",
                    format(round(reciprocal_of_mcr(mcr = 10^y + 1) * 100, 1), nsmall = 1), "%"))
      # 10^y + 1 => valeur de mcr à partir de y = log10(mcr - 1)
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
  
  # Texte relatif aux groupes
  if (any(regions_lab)) {
    # Vérification des zones affichées (non-affichage du texte des zones qui ne sont pas affichées)
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
  
  # Nécessité d'utiliser cette limite au lieu de Inf pour tracer correctement les délimitations
  xmax = xlim[2] + 1
  
  # Coloration des régions
  if (regions) {
    chart = chart +
      # A gauche, groupe II
      ggplot2::geom_polygon(data = data.frame(x = c(-Inf, -Inf, 1, 1),
                                              y = c(1, Inf, Inf, 1)),
                            ggplot2::aes(fill = "II"),
                            alpha = regions_alpha) +
      # A droite, groupe I
      ggplot2::geom_polygon(data = data.frame(x = c(Inf, 1, xmax),
                                              y = c(1, 1, xmax)),
                            ggplot2::aes(fill = "I"),
                            alpha = regions_alpha) +
      # Au centre, groupe IIIA
      ggplot2::geom_polygon(data = data.frame(x = c(1, 1, 2),
                                              y = c(1, 2, 2)),
                            ggplot2::aes(fill = "IIIA"),
                            alpha = regions_alpha) +
      # En haut, groupe IIIB
      ggplot2::geom_polygon(data = data.frame(x = c(1, 1, Inf, xmax, 2),
                                              y = c(2, Inf, Inf, xmax, 2)),
                            ggplot2::aes(fill = "IIIB"),
                            alpha = regions_alpha) +
      # Légende associée
      ggplot2::scale_fill_manual(values = stats::setNames(regions_col, c("I", "II", "IIIA", "IIIB")),
                                 name = "MIAT groups",
                                 guide = ggplot2::guide_legend(override.aes = list(color = "black",
                                                                                   linetype = "longdash")))
  }
  
  # Suite du graphique (points, délimitations, légende, labels axe Y)
  chart = chart + ggplot2::geom_point(ggplot2::aes(color = factor(thq))) +
    # Mise en évidence de la zone impossible
    ggplot2::geom_polygon(data = data.frame(x = c(-Inf, -Inf, Inf ,Inf),
                                            y = c(-Inf, 1, 1, -Inf)),
                          fill = "lightgrey", alpha = regions_alpha) +
    # Délimitation du groupe IIIA
    ggplot2::geom_polygon(data = data.frame(x = c(1, 1, 2),
                                            y = c(1, 2, 2)),
                          color = "black", linetype = "longdash", alpha = 0) +
    # Droite vertical séparant le groupe II du reste
    ggplot2::geom_segment(ggplot2::aes(x = 1, y = 2, xend = 1, yend = Inf),
                          color = "black", linetype = "longdash") +
    # Droite séparant le groupe I du reste
    ggplot2::geom_segment(ggplot2::aes(x = 2, y = 2, xend = xmax, yend = xmax),
                          color = "black", linetype = "longdash") +
    # Titres des axes et légende
    ggplot2::labs(x = "HI",
                  y = bquote(atop("MCR", "MHQ / HI")),
                  col = "Top Hazard Quotients") +
    # Ajout de la réciproque de MCR aux labels de l'axe Y
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Values and hazard quotients equal to 0 are ignored.
#' 
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
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
#' @param threshold If `TRUE`, only values or hazard quotients associated with hazard indexes greater
#'  than 1 are considered.
#' @param alone If `TRUE`, take into account single top hazard quotients (i.e. sets of values of length
#'  1). If so, a level named `"NULL"` is added as combination with such top hazard quotients.
#' @return
#' `NULL` if (1) `alone = FALSE` and no set of `values` (or `hq`) has more than one element different
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
#' thq_pairs(values = matrix(c(1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                           ncol = 2, dimnames = list(LETTERS[1:5])),
#'           references = c(1,2,3,4,5))
#' thq_pairs(hq = hazard_quotient(matrix(c(1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                       ncol = 2, dimnames = list(LETTERS[1:5])),
#'                                c(1,2,3,4,5)),
#'           hi = hazard_index(matrix(c(1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                    ncol = 2, dimnames = list(LETTERS[1:5])),
#'                             c(1,2,3,4,5)))
#' 
#' ## With and without levels parameter
#' thq_pairs(values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                           ncol = 3, dimnames = list(LETTERS[1:4])),
#'           references = c(1, 2, 3, .5),
#'           levels = LETTERS[1:4])
#' thq_pairs(values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                           ncol = 3, dimnames = list(LETTERS[1:4])),
#'           references = c(1,2,3,0.5))
#' 
#' ## NULL because all HI are lower than or equal to 1
#' thq_pairs(values = matrix(c(.1, .2, .3, .4),
#'                           ncol = 2, dimnames = list(c("A","B"))),
#'           references = c(5,5))
#' hazard_index(values = matrix(c(.1, .2, .3, .4),
#'                              ncol = 2, dimnames = list(c("A","B"))),
#'              references = c(5,5))
#' thq_pairs(values = matrix(c(.1, .2, .3, .4),
#'                           ncol = 2, dimnames = list(c("A","B"))),
#'           references = c(5,5),
#'           threshold = FALSE)
#' 
#' ## Building contingency table from a list
#' thq_pairs(values = list(c(A = 0.5, B = 0.5),
#'                         c(A = 1),
#'                         c(B = 0.5, C = 0.5)),
#'           references = list(c(0.3, 0.6),
#'                             0.3,
#'                             c(0.6, 1)))
#' thq_pairs(values = list(c(A = 0.5, B = 0.5),
#'                         c(A = 1),
#'                         c(B = 0.5, C = 0.5)),
#'           references = c(A = 0.3, B = 0.6, C = 1))
#' thq_pairs(values = list(c(A = 0.5, B = 0.5),
#'                         c(A = 1),
#'                         c(B = 0.5, C = 0.5)),
#'           references = c(A = 0.3, B = 0.6, C = 1),
#'           alone = TRUE)
#' 
#' @md
#' @export
thq_pairs = function(values = NULL, references = NULL,
                     hq = NULL, hi = NULL,
                     levels = NULL, threshold = TRUE, alone = FALSE) {
  
  # Différence si values ou hq est une liste ou une matrice
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
  
  
  # Définition des modalités à considérer dans la table
  if (is.null(levels)) levels = sort(unique(c(thq)))
  # Si prise en compte des éléments seuls, ajout d'une modalité "NULL"
  if (alone && !is.element("NULL", levels)) levels = c(levels, "NULL")
  
  # Création d'une table et application de la symétrie
  if (is.vector(thq) == 1) {
    # Si paire de THQ pour un seul ensemble valeurs
    freq_table = table(factor(thq[1], levels = levels),
                       factor(thq[2], levels = levels))
    
    freq_table[thq[2], thq[1]] = freq_table[thq[1], thq[2]]
  } else {
    # Si paires de THQ pour plusieurs ensembles de valeurs
    freq_table = table(factor(thq[1, ], levels = levels),
                       factor(thq[2, ], levels = levels))
    # Fonctionne car les THQ dans chaque paire ont été triés
    freq_table[lower.tri(freq_table)] = t(freq_table)[lower.tri(freq_table)]
  }
  
  # Si prise en compte des éléments seuls et qu'il y en a effectivement
  if (alone && sum(freq_table["NULL", ]) != 0) {
    # Déplacement des colonne et ligne NULL en fin de table
    index_null = which(colnames(freq_table) == "NULL")
    indices_reordered = c(seq_len(nrow(freq_table))[-index_null], index_null)
    freq_table = freq_table[indices_reordered, indices_reordered]
  } # Sinon, NULL est déjà placé à la fin ou n'existe pas
  
  # Retrait des noms de dimension "" (non utiles et génèrent un retour à la ligne en output)
  dimnames(freq_table) = unname(dimnames(freq_table))
  return(freq_table)
}


#' Top Hazard Quotient pairs, in list
#' 
#' Identify the top two hazard quotients pairs for which the associated hazard
#'  indexes are greater than 1.
#' 
#' @details
#' The reference values can be a vector of named values or a list.
#'  If `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Values and hazard quotients equal to 0 are ignored.
#'  
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
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
#' @param threshold If `TRUE`, only values or hazard quotients associated with hazard indexes greater
#'  than 1 are considered.
#' @param alone If `TRUE`, take into account single top hazard quotients (i.e. sets of values of length
#'  1). If so, a level named `"NULL"` is added as combination with such top hazard quotients.
#' @return
#' `NULL` if (1) `alone = FALSE` and no set of `values` (or `hq`) has more than one element different
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
  
  # Vérification que les structures de données sont nommées
  if (!is.null(values) && !is_named(values)[2])
    stop("If values is a list, it must contain vectors of named numeric values.")
  if (!is.null(hq) && !is_named(hq)[2])
    stop("If hq is a list, it must contain vectors of named numeric values.")
  
  # Si HI et/ou HQ n'est pas renseigné
  if (is.null(hi) || is.null(hq)) {
    
    # Différence si references est une liste ou un vecteur
    if (is.list(references)) {
      
      # Vérification que les ensembles de valeurs des listes ont les mêmes longueurs
      if (length(values) != length(references) ||
          any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, their lengths and the ones of their elements must match.")
      
      # Calcul des indicateurs manquants
      if (is.null(hq)) hq = lapply(seq_along(values),
                                   function(i) hazard_quotient(values[[i]], references[[i]]))
      if (is.null(hi)) hi = sapply(seq_along(values),
                                   function(i) hazard_index(values[[i]], references[[i]]))
      
    } else if (is.vector(references)) {
      
      # Vérification que references contient des données nommées
      if (!is_named(references)) stop("If values is a list and references is a vector, references must contained named values.")
      
      # Calcul des indicateurs manquants
      if (is.null(hq)) hq = lapply(seq_along(values),
                                   function(i) hazard_quotient(values[[i]], references[names(values[[i]])]))
      if (is.null(hi)) hi = sapply(values,
                                   function(v) hazard_index(v, references[names(v)]))
      
    } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
  }
  
  
  # Ignore HQ equal to 0
  hq = lapply(hq, function(x) x[x != 0])
  
  # Sélection des HQ à utiliser, selon les critères sur HI et sur la taille des ensembles de valeurs
  hq_to_use = hq[(!threshold | hi > 1) & (alone | sapply(hq, length) != 1)]
  
  # Si aucun HI n'est supérieur à 1 ou qu'aucun vecteur ne contient plus d'une valeur
  if (length(hq_to_use) == 0) return(NULL)
  
  # Quand une seule valeur, paire avec "NULL"
  if (alone) {
    index_alone = which(sapply(hq_to_use, length) == 1)
    hq_to_use[index_alone] = lapply(hq_to_use[index_alone], c, "NULL" = 0)
  }
  
  # Recherche des 2 Top HQ
  if (length(hq_to_use) == 1) {
    # Si un seul ensemble de valeurs satisfait les critères (HI > 1 et/ou length > 1)
    thq = names(top_hazard_quotient(hq = hq_to_use[[1]], k = 2))
  } else {
    # Si plusieurs ensembles de valeurs satisfont les critères (HI > 1 et/ou length > 1)
    thq = sapply(hq_to_use, function(hq) sort(names(top_hazard_quotient(hq = hq, k = 2))))
  }
  
  return(thq)
}


#' Top Hazard Quotient pairs, in matrix
#' 
#' Identify the top two hazard quotients pairs for which the associated hazard
#'  indexes are greater than 1.
#' 
#' @details
#' The reference values are applied once on each column (i.e. it must have one reference value for each
#'  row of the matrix).
#' 
#' Values and hazard quotients equal to 0 are ignored.
#' 
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
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
#' @param threshold If `TRUE`, only values or hazard quotients associated with hazard indexes greater
#'  than 1 are considered.
#' @param alone If `TRUE`, take into account single top hazard quotients (i.e. sets of values of length
#'  1). If so, a level named `"NULL"` is added as combination with such top hazard quotients.
#' @return
#' `NULL` if (1) `alone = FALSE` and no set of `values` (or `hq`) has more than one element different
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
  
  # Vérification que les structures de données sont nommées
  if (!is.null(values) && !is_named(values)[1]) stop("Rows of values must be named.")
  if (!is.null(hq) && !is_named(hq)[1]) stop("Rows of hq must be named.")
  
  # Calcul des données manquantes
  if (is.null(hq)) hq = hazard_quotient(values, references)
  if (is.null(hi)) hi = hazard_index(hq = hq)
  
  # Sélection des HQ à utiliser, selon les critères sur HI et sur la taille des ensembles de valeurs
  hq_to_use = hq[, (!threshold | hi > 1) & (alone | apply(hq, 2, function(x) sum(x != 0) != 1)),
                 drop = FALSE]
  
  # Si aucun HI n'est supérieur à 1 ou qu'aucun vecteur ne contient plus d'une valeur
  if (ncol(hq_to_use) == 0) return(NULL)
  
  # Quand une seule valeur, paire avec "NULL"
  if (alone) {
    index_alone = apply(hq_to_use, 2, function(x) sum(x != 0) == 1)
    hq_to_use = rbind(hq_to_use, "NULL" = ifelse(index_alone, 1, 0))
  }
  
  # Recherche des 2 Top HQ
  if (ncol(hq_to_use) == 1) {
    # Si un seul ensemble de valeurs satisfait les critères (HI > 1 et/ou length > 1)
    thq = names(top_hazard_quotient(hq = hq_to_use[, 1], k = 2))
  } else {
    # Si plusieurs ensembles de valeurs satisfont les critères (HI > 1 et/ou length > 1)
    thq = apply(hq_to_use, 2, function(hq) sort(names(top_hazard_quotient(hq = hq, k = 2))))
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
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
#'             ncol = 3, dimnames = list(LETTERS[1:4]))
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
  
  # Si thq et/ou groups n'est pas renseigné
  if (is.null(thq) || is.null(groups)) {
  
    # Cas spécifique dans lequel values ou hq est une liste et non une matrice
    if (is.list(values) || is.list(hq)) {
      
      # Vérification que les structures de données sont nommées
      if (!is.null(values) && !is_named(values)[2])
        stop("If values is a list, it must contain vectors of named numeric values.")
      if (!is.null(hq) && !is_named(hq)[2])
        stop("If hq is a list, it must contain vectors of named numeric values.")
      
      # Si ni HQ ni THQ n'est renseigné
      if (is.null(thq) && is.null(hq)) {
      
        # Différence si references est une liste ou un vecteur
        if (is.list(references)) {
          
          # Vérification que les structures de données sont nommées
          if (length(values) != length(references) ||
              any(sapply(values, length) != sapply(references, length)))
            stop("If values and references are two lists, their lengths and the ones of their elements must match.")
          
          # Calcul des indicateurs manquants
          if (is.null(thq)) thq = sapply(seq_len(length(values)),
                                         function(i) top_hazard_quotient(values[[i]], references[[i]], k = 1))
          if (is.null(groups)) groups = sapply(seq_len(length(values)),
                                               function(i) classify_mixture(values[[i]], references[[i]]))
          
        } else if (is.vector(references)) {
          
          # Vérification que les structures de données sont nommées
          if (!is_named(references)) stop("If values is a list and references is a vector. Both must contained named values.")
          
          # Calcul des indicateurs manquants
          if (is.null(thq)) thq = sapply(unname(values),
                                         function(v) top_hazard_quotient(v, references[names(v)], k = 1))
          if (is.null(groups)) groups = sapply(values,
                                               function(v) classify_mixture(v, references[names(v)]))
          
        } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
        
      } else if (!is.null(hq)) { # Si HQ est renseigné
        # Calcul des indicateurs manquants
        if (is.null(thq)) thq = sapply(unname(hq), function(hqs) top_hazard_quotient(hq = hqs, k = 1))
        if (is.null(groups)) groups = sapply(hq, function(hqs) classify_mixture(hi = hazard_index(hq = hqs),
                                                                                mhq = maximum_hazard_quotient(hq = hqs)))
      }
    } else { # Cas où values ou hq est une matrice, ou aucun des deux n'est renseigné
    
      # Vérification que les structures de données sont nommées
      if (!is.null(values) && !is_named(values)[1]) stop("Rows of values must be named.")
      if (!is.null(hq) && !is_named(hq)[1]) stop("Rows of hq must be named.")
      
      # Calcul des données manquantes
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
  
  # Soit thq est une list (hq ou values est une liste) soit thq est un vecteur (hq ou values est une matrice)
  thq_names = if (is.list(thq)) sapply(unname(thq), function(v) names(v)[1]) else names(thq)
  if (!is.null(levels)) thq_names = factor(thq_names, levels = levels)
  freq_table = table(thq_names, factor(groups, levels = c("I", "II", "IIIA", "IIIB")))
  
  # Retrait du nom de dimension "thq_names" et retour
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
#' * If `values` is a matrix, its rows must be named.
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
#' @seealso [`mcr_summary_by_class`] [`mcr_chart_by_class`], [`thq_pairs_by_class`],
#'          [`thq_by_group_by_class`].
#' @md
#' @keywords internal
check_data_for_mcr_by_class = function(values, references = NULL, vector = TRUE, matrix = TRUE, list = TRUE) {
  
  # Vérification du nommage de values et references si values peut être une liste, une matrice, un vecteur
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
  else if (matrix && is.matrix(values) && !is_named(values)[1]) stop("If values is a matrix, its rows must be named.")
  else if (vector && is.vector(values) && !is_named(values)) stop("If values is a vector, it must have named numeric values.")
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#'  
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is faster.
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
#' @param all_classes Logical indicating whether all classes must be considered for each set of values
#'  or only those that are actually associated with the set of values.
#' @return Data frame or list of data frames (according to `values`) containing the main indicators of
#'  the MCR approach, computed on the given `values` and for each class encountered (or for all classes,
#'  if `all_classes` is `TRUE`):
#' * **n**: number of values.
#' * **HI**: Hazard Index.
#' * **MCR**: Maximum Cumulative Ratio.
#' * **Reciprocal**: Reciprocal of the maximum cumulative ratio.
#' * **Group**: MIAT group.
#' * **THQ**: Top Hazard Quotient.
#' * **MHQ**: Maximum Hazard Quotient.
#' * **Missed**: Hazard missed if a cumulative risk assessment is not performed.
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
#'                      references = c(1,2,3,4,5),
#'                      classes = classes)
#' mcr_summary_by_class(values = c(A = 1, B = 2, C = 3, D = 4),
#'                      references = c(1, 2, 3, 4),
#'                      classes)
#' 
#' ## MCR summary by class on matrices
#' mcr_summary_by_class(values = matrix(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.),
#'                                      ncol = 2,
#'                                      dimnames = list(LETTERS[1:5],
#'                                                      c("V1", "V2"))),
#'                      references = c(1,2,3,4,5),
#'                      classes)
#' mcr_summary_by_class(values = matrix(c(0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9),
#'                                      ncol = 2,
#'                                      dimnames = list(LETTERS[1:4],
#'                                                      c("V1", "V2"))),
#'                      references = c(1,2,3,4),
#'                      classes)
#' 
#' ## MCR summary by class on lists
#' mcr_summary_by_class(values = list(V1 = c(A = 0.1, B = 0.5),
#'                                    V2 = c(A = 0.2),
#'                                    V3 = c(B = 0.3, C = 0.4)),
#'                      references = c(A = 1, B = 2, C = 3),
#'                      classes)
#' mcr_summary_by_class(values = list(V1 = c(A = 0.1, B = 0.5),
#'                                    V2 = c(A = 0.2),
#'                                    V3 = c(B = 0.3, C = 0.4)),
#'                      references = list(c(1, 2),
#'                                        1,
#'                                        c(2, 3)),
#'                      classes)
#' 
#' @md
#' @export
mcr_summary_by_class = function(values, references, classes, all_classes = FALSE) {
  
  # Utilisation des classes sous forme de matrice binaire
  if (is.list(classes)) classes = turn_list_into_logical_matrix(classes)
  else if (!is.matrix(classes) || typeof(classes) != "logical")
    stop("classes must be a list or a logical matrix.")
  
  # Vérification que les structures de données sont nommées
  check_data_for_mcr_by_class(values, references)
  
  
  # Cas d'une liste de valeurs
  if (is.list(values)) {
    # Pour chaque ensemble de valeurs, calcul des indicateurs MCR pour chaque classe
    # Différence si references est une liste ou un vecteur
    if (is.list(references)) {
      to_return = lapply(seq_along(values),
                         function(i) mcr_summary_by_class(values[[i]], references[[i]],
                                                          classes, all_classes))
      return(stats::setNames(to_return, names(values)))
    }
    return(lapply(values, function(v) mcr_summary_by_class(v, references[names(v)], classes, all_classes)))
  }
  
  # Cas d'une matrice valeurs
  if (is.matrix(values)) {
    # Pour chaque colonne de valeurs, calcul des indicateurs MCR pour chaque classe
    return(apply(values, 2, function(v) mcr_summary_by_class(v, references, classes, all_classes)))
  }
  
  # Cas d'un unique vecteur de valeurs
  if (is.vector(values)) {
    # Pour chaque classe, calcul des indicateurs MCR des valeurs et références correspondantes
    summary = apply(classes, 2, function(column) {
      # Extraction des valeurs correpondant à la classe
      indices = match(rownames(classes)[column], names(values))
      v = values[indices]
      r = references[indices]
      
      # Retrait des NA (lorsque des noms associés à la classe en cours ne font pas partie des valeurs)
      v = v[!is.na(v)]
      if (length(v) == 0) return(list(n = 0, HI = NA_real_, MCR = NA_real_, Reciprocal = NA_real_,
                                      Group = NA_character_, THQ = NA_character_,
                                      MHQ = NA_real_, Missed = NA_real_))
      return(mcr_summary(v, r[!is.na(r)]))
    })
    
    if (!all_classes) {
      # Retrait des classes pour lesquelles il n'y a aucune valeur
      summary = summary[sapply(summary, "[[", "n") != 0]
      # NULL si les valeurs ne sont associées à aucune classe
      if (length(summary) == 0) return(NULL)
    }
    
    # Conversion en deux temps car les facteurs sont transformés en numeric
    to_return = data.frame(matrix(unlist(summary), nrow = length(summary), byrow = TRUE),
                           stringsAsFactors = FALSE)
    rownames(to_return) = names(summary)
    colnames(to_return) = names(summary[[1]])
    to_return[, "Group"] = sapply(summary, "[[", "Group")
    to_return[, "THQ"] = sapply(summary, "[[", "THQ")
    
    return(to_return)
  }
}


#' MCR approach scatter plot by class
#' 
#' Create charts of \mjeqn{log_{10}(HI)}{log10(HI)} versus \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)}
#'  with the percentage of the reciprocal of the maximum cumulative ratio, the elements producing the
#'  top hazard quotients and the associated MIAT groups, according to classes. For each class, one chart
#'  is created from the subset of values corresponding to this class.
#' 
#' @details
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#'  
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is faster.
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
#'  If `TRUE` and the returned list is not assigned, the charts are plotted twice.
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
#' ## Creating a matrix of 5*50 values and one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 1.1, by = 0.1), 250, replace = TRUE),
#'             ncol = 50, dimnames = list(LETTERS[1:5]))
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
#' charts3 <- mcr_chart_by_class(values = list(V1 = c(A = 0.1, B = 0.5),
#'                                             V2 = c(A = 0.2),
#'                                             V3 = c(B = 0.3, C = 0.4)),
#'                               references = list(c(1, 2),
#'                                                 1,
#'                                                 c(2, 3)),
#'                               classes,
#'                               log_transform = TRUE)
#' View(charts3)
#' plot(charts3$C8)
#' 
#' charts4 <- mcr_chart_by_class(values = list(V1 = c(A = 0.1, B = 0.5),
#'                                             V2 = c(A = 0.2),
#'                                             V3 = c(B = 0.3, C = 0.4)),
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
  
  # Utilisation des classes sous forme de matrice binaire
  if (is.list(classes)) classes = turn_list_into_logical_matrix(classes)
  else if (!is.matrix(classes) || typeof(classes) != "logical")
    stop("classes must be a list or a logical matrix.")
  
  # Vérification que les structures de données sont nommées
  check_data_for_mcr_by_class(values, references, vector = FALSE)
  
  # Booléens sur l'ensemble des noms des classes pour expliciter les warnings des appels à mcr_chart
  class_warnings = stats::setNames(logical(ncol(classes)), colnames(classes))
  
  
  # Pour chaque classe, un graphique des indicateurs MCR des valeurs et références correspondantes
  charts = apply(classes, 2, function(column) {
    
    # Extraction des valeurs et références correpondant à la classe
    class = colnames(classes)[parent.frame()$i[]]
    new_vr = subset_from_class(values, references, classes, class)
    
    # NA si la classe n'est pas représentée (cas différent si values est une liste ou une matrice)
    if ((is.list(values) && (length(new_vr[["values"]]) == 0 || length(new_vr[["values"]][[1]]) == 0)) ||
        (is.matrix(values) && nrow(new_vr[["values"]]) == 0)) return(NA)
    
    # Catch warning sans interrompre l'exécution de l'instruction
    withCallingHandlers(return(mcr_chart(new_vr[["values"]], new_vr[["references"]],
                                         thq_col = thq_col, regions = regions,
                                         regions_col = regions_col, regions_alpha = regions_alpha,
                                         regions_lab = regions_lab, regression = regression, log_transform = log_transform,
                                         plot = plot)),
                        warning = function(w) { class_warnings[class] <<- TRUE })
    
  })
  
  # Affichage d'un message en fonction des warnings rencontrés
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' Values equal to 0 are ignored.
#'  
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is faster.
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
#' @param threshold If `TRUE`, only values associated with hazard indexes greater than 1 are considered.
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
#' ## Creating a matrix of 5*50 values, one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 2.1, by = 0.1), 250, replace = TRUE),
#'             ncol = 50, dimnames = list(LETTERS[1:5]))
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
#' thq_pairs_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                  V2 = c(A = 2),
#'                                  V3 = c(B = 3, C = 4)),
#'                    references = list(c(1, 2),
#'                                      1,
#'                                      c(2, 3)),
#'                    classes)
#' thq_pairs_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                  V2 = c(A = 2),
#'                                  V3 = c(B = 3, C = 4)),
#'                    references = c(A = 1, B = 2, C = 3),
#'                    classes,
#'                    levels = LETTERS[1:3])
#' 
#' # Use of the parameters alone and threshold
#' thq_pairs_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                  V2 = c(A = 2),
#'                                  V3 = c(B = 3, C = 4)),
#'                    references = c(A = 1, B = 2, C = 3),
#'                    classes,
#'                    levels = LETTERS[1:3],
#'                    alone = TRUE)
#' thq_pairs_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                  V2 = c(A = 2),
#'                                  V3 = c(B = 3, C = 4)),
#'                    references = c(A = 1, B = 2, C = 3),
#'                    classes,
#'                    levels = LETTERS[1:3],
#'                    threshold = FALSE, alone = TRUE)
#' 
#' @md
#' @export
thq_pairs_by_class = function(values, references, classes,
                              levels = NULL, threshold = TRUE, alone = FALSE) {
  
  # Utilisation des classes sous forme de matrice binaire
  if (is.list(classes)) classes = turn_list_into_logical_matrix(classes)
  else if (!is.matrix(classes) || typeof(classes) != "logical")
    stop("classes must be a list or a logical matrix.")
  
  # Vérification que les structures de données sont nommées
  check_data_for_mcr_by_class(values, references, vector = FALSE)
  
  
  # Pour chaque classe, une table concernant les valeurs et références correspondantes
  tables = apply(classes, 2, function(column) {
    
    # Extraction des valeurs et références correpondant à la classe
    new_vr = subset_from_class(values, references, classes, colnames(classes)[parent.frame()$i[]])
    
    # NA si la classe n'est pas représentée (cas différent si values est une liste ou une matrice)
    if ((is.list(values) && (length(new_vr[["values"]]) == 0 || length(new_vr[["values"]][[1]]) == 0)) ||
        (is.matrix(values) && nrow(new_vr[["values"]]) == 0)) return(NA)
    
    # Retour d'une liste pour éviter que les tables ne fusionnent en une unique matrice
    # (si levels est utilisé et qu'il n'y a aucun NA)
    return(list(thq_pairs(new_vr[["values"]], new_vr[["references"]],
                          levels = levels, threshold = threshold, alone = alone)))
  })
  
  # Le délistage n'est pas toujours à effectuer
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#'  
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is faster.
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
#' ## Creating a matrix of 5*50 values, one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 2.1, by = 0.1), 250, replace = TRUE),
#'             ncol = 50, dimnames = list(LETTERS[1:5]))
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
#' thq_by_group_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                     V2 = c(A = 2),
#'                                     V3 = c(B = 3, C = 4)),
#'                       references = list(c(1, 2),
#'                                         1,
#'                                         c(2, 3)),
#'                       classes)
#' thq_by_group_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                     V2 = c(A = 2),
#'                                     V3 = c(B = 3, C = 4)),
#'                       references = c(A = 1, B = 2, C = 3),
#'                       classes,
#'                       levels = LETTERS[1:3])
#' 
#' @md
#' @export
thq_by_group_by_class = function(values, references, classes,
                                 levels = NULL) {
  
  # Utilisation des classes sous forme de matrice binaire
  if (is.list(classes)) classes = turn_list_into_logical_matrix(classes)
  else if (!is.matrix(classes) || typeof(classes) != "logical")
    stop("classes must be a list or a logical matrix.")
  
  # Vérification que les structures de données sont nommées
  check_data_for_mcr_by_class(values, references, vector = FALSE)
  
  
  # Pour chaque classe, une table concernant les valeurs et références correspondantes
  tables = apply(classes, 2, function(column) {
    
    # Extraction des valeurs et références correpondant à la classe
    new_vr = subset_from_class(values, references, classes, colnames(classes)[parent.frame()$i[]])
    
    # NA si la classe n'est pas représentée (cas différent si values est une liste ou une matrice)
    if ((is.list(values) && (length(new_vr[["values"]]) == 0 || length(new_vr[["values"]][[1]]) == 0)) ||
        (is.matrix(values) && nrow(new_vr[["values"]]) == 0)) return(NA)
    
    # Retour d'une liste pour éviter que les tables ne fusionnent en une unique matrice
    # (si levels est utilisé et qu'il n'y a aucun NA)
    return(list(thq_by_group(new_vr[["values"]], new_vr[["references"]], levels = levels)))
  })
  
  # Le délistage n'est pas toujours à effectuer
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
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#'  
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' If `classes` is a list, it will be turned into a logical matrix before processing. Thus, call the
#'  function with such a matrix is faster.
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
#' ## Creating a matrix of 5*50 values, one reference value for each of the 5
#' ## elements (A, B, C, D and E) and association of classes (C1 to C8) with
#' ## these elements.
#' v <- matrix(sample(seq(0.1, 2.1, by = 0.1), 250, replace = TRUE),
#'             ncol = 50, dimnames = list(LETTERS[1:5]))
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
#' mcr_approach_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                     V2 = c(A = 2),
#'                                     V3 = c(B = 3, C = 4)),
#'                       references = list(c(1, 2),
#'                                         1,
#'                                         c(2, 3)),
#'                       classes,
#'                       FUN = "thq_pairs", levels = names(classes))
#' 
#' mcr_approach_by_class(values = list(V1 = c(A = 1, B = 5),
#'                                     V2 = c(A = 2),
#'                                     V3 = c(B = 3, C = 4)),
#'                       references = c(A = 1, B = 2, C = 3),
#'                       classes,
#'                       FUN = "thq_by_group", levels = names(classes))
#' 
#' @md
#' @export
mcr_approach_by_class = function(values, references, classes, FUN, ...) {
  
  # Vérification du choix de la fonction (conversion en chaîne de caractères)
  functions = c("mcr_summary", "mcr_chart", "thq_pairs", "thq_by_group")
  if (class(FUN) == "function") FUN = deparse(substitute(FUN))
  if (!(FUN %in% functions))
    stop("FUN must refer to one of the following functions: ", paste(functions, collapse = ", "), ".")
  
  # Appel à la fonction concernée
  return(do.call(paste0(FUN, "_by_class"),
                 list(values, references, classes, ...)))
}



#### Subsets and reduction of sets ####

#' Subsets of values and references by class
#' 
#' Extract from values and references those corresponding to one specific class.
#' 
#' @details
#' If `values` is a matrix, it must have one reference value for each row.
#' 
#' If `values` is a list, the reference values can be a vector of named values or a list. In this case,
#'  if `references` is a vector, there must be one reference for each name present in `values`.
#'  Otherwise, `references` is a list of vectors having the same lengths as those present in `values`
#'  so that `values` and `references` can be matched.
#' 
#' @param values Numeric named matrix or list of numeric named vectors. Values from which to extract a
#'  subset according to one specific class.
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
#' ## Subsets with values as a matrix
#' subset_from_class(values = matrix(c(0.1,0.2,0.3,0.4,0.5,
#'                                     0.6,0.7,0.8,0.9,1.),
#'                                   ncol = 2,
#'                                   dimnames = list(LETTERS[1:5], c("V1", "V2"))),
#'                   references = c(1,2,3,4,5),
#'                   classes = classes,
#'                   class_name = "C8")
#' 
#' ## Subsets with values as a list
#' v <- list(V1 = c(A = 0.1, D = 0.5),
#'           V2 = c(D = 0.2),
#'           V3 = c(A = 0.3, C = 0.4))
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
  
  # Vérification que les structures de données sont nommées
  check_data_for_mcr_by_class(values, references, vector = FALSE)
  
  # Utilisation des classes sous forme de matrice binaire
  if (is.list(classes)) classes = turn_list_into_logical_matrix(classes)
  else if (!is.matrix(classes) || typeof(classes) != "logical")
    stop("classes must be a list or a logical matrix.")
  
  # Noms des éléments qui correspondent à la classe
  items_in_class = rownames(classes)[classes[, class_name]]
  
  
  # Cas d'une liste de valeurs
  if (is.list(values)) {
    
    # Sous-ensembles de references correspondant aux éléments de la classe
    if (!is.null(references)) {
      
      if (is.list(references)) {
        references_class = list()
        
        # Ajout à la nouvelle liste des références de chaque élément faisant partie de la classe
        for (i in seq_along(values)) {
          indices = names(values[[i]]) %in% items_in_class
          
          # Ne considère pas les ensembles ne contenant aucun élément de la classe
          if (sum(indices) != 0) {
            name_element = if (is.null(names(values))) length(references_class) + 1 else names(values)[i]
            references_class[[name_element]] = references[[i]][indices]
          }
        }
      } else {
        # Si references est un unique vecteur
        references_class = references[items_in_class]
        # Retrait des NA (lorsque des noms associés à la classe ne font pas partie des références)
        references_class = references_class[!is.na(references_class)]
      }
    }
    
    # Sous-ensembles de values correspondant aux éléments de la classe
    values_items = turn_list_into_logical_matrix(lapply(values, names))
    values_class = apply(
      values_items[, items_in_class[items_in_class %in% colnames(values_items)],
                   drop = FALSE],
      1,
      function(row) {
        values[[parent.frame()$i[]]][
          names(values[[parent.frame()$i[]]]) %in% names(row)[row]
          # Use this instead of names(row)[row] because values can have duplicate names
        ]
      })
    # Retrait des ensembles vides, ne contenant aucun élément de la classe
    values_class = values_class[lengths(values_class) != 0]
  }
  # Cas d'une matrice valeurs
  else if (is.matrix(values)) {
    
    # Extraction des valeurs correpondant à la classe
    # et retrait des NA (lorsque des noms associés à la classe ne font pas partie des valeurs)
    indices = match(items_in_class, rownames(values))
    values_class = values[indices[!is.na(indices)], ]
    
    if (!is.null(references)) references_class = references[indices[!is.na(indices)]]
    
    # Reconversion en matrice s'il n'y avait qu'une seule ligne
    if (is.vector(values_class)) {
      values_class = t(values_class)
      rownames(values_class) = rownames(values)[indices[!is.na(indices)]]
    }
  }
  
  if (is.null(references)) return(values_class)
  return(list(values = values_class, references = references_class))
}


#' Reduce sets according to the names of their components
#' 
#' For each set of values, apply a function to merge the ones having the same names or choose one
#'  of them, for each different name found in the set of values. This results in sets of values having
#'  only one value per different name found in the original sets, instead of several ones.
#' 
#' @details
#' If `values` and `references` are two lists, `references` must be a list of vectors having the same
#'  lengths as those present in `values` so that `values` and `references` can be matched.
#' 
#' If `references` is not a list, it is not processed and is returned as is. Otherwise, it is assumed
#'  that the same reference values are given for same value names in one set of values of the argument
#'  `values`.
#' 
#' The components of each resulting set of values are ordered in the order in which the names are found
#'  in the original set.
#'  
#' @param values Numeric named vector or matrix, or list of numeric named vectors.
#'  Sets of values to reduce.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`.
#' @param FUN Function to apply on each subset of values corresponding to each different name to reduce
#'  it to a single value.
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
#'             ncol = 10, dimnames = list(c("A", "B", "A", "C", "C")))
#' reduce_sets(values = v, FUN = mean)
#' 
#' ## Reduce sets of values and associated reference values given as lists
#' reduce_sets(values = list(c(A = 0.1, A = 0.3, B = 0.5),
#'                           c(A = 0.2),
#'                           c(B = 0.3, B = 0.4, B = 0.7, C = 0.4, C = 0.1)),
#'             references = list(c(1, 1, 2),
#'                               1,
#'                               c(2, 2, 2, 3, 3)),
#'             FUN = median)
#' 
#' @md
#' @export
reduce_sets = function(values, references = NULL, FUN, ...) {
  
  # Vérification que la structure de données de values est nommée
  if (is.list(values)) {
    # (Décomposition du if en 2 à cause du suivant : une liste est aussi un vecteur)
    if (!is_named(values)[2]) stop("If values is a list, it must contain vectors of named numeric values.")
  }
  else if (is.vector(values) && !is_named(values)) stop("If values is a vector, it must have named numeric values.")
  else if (is.matrix(values) && !is_named(values)[1]) stop("If values is a matrix, its rows must be named.")
  
  # Vérifications relatives aux références
  if (!is.null(references)) {
    if (!is.list(values)) stop("If references is a list, values must also be a list.")
    
    if (length(values) != length(references)
        || any(sapply(values, length) != sapply(references, length)))
      stop("If values and references are two lists, their lengths and the ones of their elements must match.")
  }
  
  
  # Nouveaux ensembles de valeurs ; cas différent en fonction de list/vector/matrix
  if (is.list(values)) {
    # Récursivité pour chaque élément de la liste (chaque ensemble de valeurs)
    new_values = lapply(values, function(v) {
      stats::setNames(reduce_sets(v, FUN = FUN, ...), unique(names(v)))
    })
  }
  else if (is.vector(values)) {
    # Applique la fonction à chaque sous-ensemble du vecteur correspondant à chaque nom
    new_values = sapply(unique(names(values)), function(name) {
      FUN(values[which(names(values) == name)], ...)
    })
  }
  else if (is.matrix(values)) {
    # Pour chaque sous-ensemble de la matrice correspondant à chaque nom
    new_values = t(sapply(unique(rownames(values)), function(name) {
      # Application de la fonction à chaque colonne
      rows = which(rownames(values) == name)
      if (length(rows) == 1) return(sapply(values[rows, ], FUN, ...))
      return(apply(values[rows, ], 2, FUN, ...))
    }))
  }
  
  # Retour si references non renseigné
  if (is.null(references)) return(new_values)
  
  # Nouvelles références
  if (!is.list(references)) new_references = references
  else {
    # Pour chaque ensemble de valeurs
    new_references = lapply(values, function(v) {
      # Recherche des indices des premiers éléments associés à chaque nom
      indices = sapply(unique(names(v)), function(name) which(names(v) == name)[1])
      # Conservation des références associées à ces indices uniquement
      return(references[[parent.frame()$i[]]][indices])
    })
  }
  return(list(values = new_values, references = new_references))
}


