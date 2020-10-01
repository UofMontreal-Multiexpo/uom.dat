#' @include utils.R
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
    if (is.vector(hq)) {
      hq = matrix(hq, nrow = 1)
      rownames(hq) = rownames(values)
    }
    
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
#'  \mjdeqn{HI_i = \sum_{j = 1}^N HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to N}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{N} denotes the number of hazard quotients.
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
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,N\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{N} denotes the number of hazard quotients.
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
  
  return(hi/mhq)
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
#' Due to the multiple possible usages, the argument `mcr` msut be explicitly named in the function call.
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
#' Compute the reciprocal of maximum cumulative ratio.
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


#' Top Hazard Quotients (THQ)
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
#'  For example, if `hq = c(d = 5, b = 1, c = 3, a = 3)` and `k = 2`, the return is `c(d = 5, c = 3)`.
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
#' top_hazard_quotient(values, references, k)
#' top_hazard_quotient(hq, k)
#' @param values Numeric named vector or matrix. Values for which the top hazard quotients are to be
#'  identify.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hq Numeric named vector or matrix. **H**azard **q**uotients whose highest values are to be
#'  identify.
#' @param k Number of hazard quotients to highlight. Default is the integer part of the maximum cumulative
#'  ratio computed from `values` and `references`.
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
#' top_hazard_quotient(c(a = 1, b = 2, c = 3, d = 4, e = 5), c(5,4,3,2,1),
#'                     k = 3)
#' top_hazard_quotient(hq = hazard_quotient(c(a = 1, b = 2, c = 3, d = 4, e = 5),
#'                                          c(5,4,3,2,1)),
#'                     k = 3)
#' top_hazard_quotient(values = matrix(c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                     ncol = 2, dimnames = list(letters[1:5])),
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
    # Valeur par défaut
    k = trunc(maximum_cumulative_ratio(hi = hazard_index(hq = hq),
                                       mhq = maximum_hazard_quotient(hq = hq)))
  } else if (k > length(hq)) {
    k = length(hq)
  }
  return(sort(hq, decreasing = TRUE)[seq_len(k)])
}


#' Classify mixture into the four MIAT groups
#' 
#' Classify mixtures into four groups according to the CEFIC-MIAT (Mixtures Industry Ad-hoc Team)
#'  decision tree, each one requiring a different risk management strategy.
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
#'  for each column (or value, respectively).
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
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,N\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{N} denotes the number of hazard quotients.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^N HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to N}
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
#'  Values whose indicators of the MCR approach are to be computed.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @return Data frame of the main indicators of the MCR approach, computed on the given `values`:
#' * **HI**: Hazard Index.
#' * **MCR**: Maximum Cumulative Ratio.
#' * **Reciprocal**: Reciprocal of the maximum cumulative ratio.
#' * **Group**: MIAT group.
#' * **THQ**: Top Hazard Quotient.
#' * **MHQ**: Maximum Hazard Quotient.
#' * **Missed**: Hazard missed if a cumulative risk assessment is not performed.
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
#' ## MCR summary on vector and matrix
#' mcr_summary(c(a = 1, b = 2, c = 3, d = 4, e = 5, c(1,2,3,4,5))
#' mcr_summary(values = matrix(sample(seq(0.1, 1, by = 0.1), 50, replace = TRUE),
#'                             ncol = 10, dimnames = list(letters[1:5])),
#'             references = sample(seq(1,5), 5, replace = TRUE))
#' 
#' ## MCR summary on list
#' mcr_summary(values = list(c(a = 0.1, b = 0.5),
#'                           c(a = 0.2),
#'                           c(b = 0.3, c = 0.4)),
#'             references = c(a = 1, b = 2, c = 3))
#' mcr_summary(values = list(c(a = 0.1, b = 0.5),
#'                           c(a = 0.2),
#'                           c(b = 0.3, c = 0.4)),
#'             references = list(c(1, 2),
#'                               1,
#'                               c(2, 3)))
#' 
#' @md
#' @export
mcr_summary = function(values, references) {
  
  # Cas spécifiques dans lequel values est une liste
  if (is.list(values)) return(mcr_summary_for_list(values, references))
  
  
  # Vérification que la structure de données est nommée
  if (is.matrix(values) && !is.named(values)[1]) stop("If values is a matrix, its rows must be named.")
  else if (is.vector(values) && !is.named(values)) stop("If values is a vector, it must have named numeric values.")
  
  # Calcul des indicateurs
  hq = hazard_quotient(values, references)
  hi = hazard_index(hq = hq)
  mhq = maximum_hazard_quotient(hq = hq)
  mcr = maximum_cumulative_ratio(hi = hi, mhq = mhq)
  mt = missed_toxicity(mcr = mcr)
  
  rmcr = reciprocal_of_mcr(mcr = mcr)
  groups = classify_mixture(hi = hi, mhq = mhq, mcr = mcr)
  
  # Différence vector/matrix
  if (is.vector(values)) {
    return(data.frame(HI = hi, MCR = mcr, Reciprocal = rmcr, Group = groups,
                      THQ = names(top_hazard_quotient(hq = hq, k = 1)),
                      MHQ = mhq, Missed = mt,
                      row.names = NULL))
  }
  return(data.frame(HI = hi, MCR = mcr, Reciprocal = rmcr, Group = groups,
                    THQ = names(unlist(unname(top_hazard_quotient(hq = hq, k = 1)))),
                    MHQ = mhq, Missed = mt))
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
#'  \mjdeqn{MHQ_i = HQ_{M,i} = \max_{j \in \lbrace 1,...,N\rbrace} HQ_{i,j}}{MHQ_i = HQ_Mi = max HQ_i}
#'  where \eqn{N} denotes the number of hazard quotients.
#' 
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^N HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to N}
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
#' @param values List of numeric named vectors. Values whose indicators of the MCR approach are to be
#'  computed.
#' @param references Numeric named vector or list of numeric vectors. Reference values associated with
#'  the `values`. See 'Details' to know the way it is associated with `values`.
#' @return Data frame of the main indicators computed on the given `values`:.
#' * **HI**: Hazard Index.
#' * **MCR**: Maximum Cumulative Ratio.
#' * **Reciprocal**: Reciprocal of the maximum cumulative ratio.
#' * **Group**: MIAT group.
#' * **THQ**: Top Hazard Quotient.
#' * **MHQ**: Maximum Hazard Quotient.
#' * **Missed**: Hazard missed if a cumulative risk assessment is not performed.
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
#' @seealso [`mcr_summary`].
#' 
#' @md
#' @keywords internal
mcr_summary_for_list = function(values, references) {
  
  # Différence si references est une liste ou un vecteur
  if (is.list(references)) {
    if (any(sapply(values, length) != sapply(references, length)))
      stop("If values and references are two lists, the lengths of their elements must match.")
    if (!is.named(values)[2])
      stop("If values is a list, it must contain vector of named numeric values.")
    
    summary = t(sapply(seq_len(length(values)), function (i) mcr_summary(values[[i]], references[[i]])))
    
  } else if (is.vector(references)) {
    if (!is.named(references) || !is.named(values)[2])
      stop("If values is a list and references is a vector, both must contained named values.")
    
    summary = t(sapply(seq_len(length(values)),
                       function (i) mcr_summary(values[[i]], references[names(values[[i]])])))
    
  } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
  
  # Unlist en deux temps sinon les facteurs sont transformés en numeric
  to_return = as.data.frame(apply(summary[, c(1,2,3,6,7)], 2, unlist))
  to_return[, "Group"] = unlist(summary[, "Group"])
  to_return[, "THQ"] = unlist(summary[, "THQ"])
  
  return(to_return[, c("HI", "MCR", "Reciprocal", "Group", "THQ", "MHQ", "Missed")])
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
#'           log_transform = TRUE)
#' mcr_chart(hi, mcr, thq,
#'           thq_col = NULL,
#'           regions = FALSE,
#'           regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"),
#'           regions_alpha = 0.2,
#'           regions_lab = !regions,
#'           regression = FALSE,
#'           log_transform = TRUE)
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
#'  defining by `regions_col`.
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
#' @param plot If `FALSE`, the chart is not plotted. If `TRUE`, the chart is plotted in the active
#'  graphics device.
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
#'          [`top_hazard_quotient`].
#' 
#' @examples
#' ## Creating a matrix of 5*50 values and one reference value for each of the 5
#' ## elements ("a", "b", "c", "d" and "e").
#' v <- matrix(sample(seq(0.1, 1.1, by = 0.1), 250, replace = TRUE),
#'             ncol = 50, dimnames = list(letters[1:5]))
#' r <- sample(seq(1,5), 5, replace = TRUE)
#' 
#' mcr_chart(v, r, regions = TRUE)
#' mcr_chart(v, r,
#'           thq_col = c(a = "blue", b = "green", c = "red", d = "yellow3", e = "grey"),
#'           regions = FALSE,
#'           regions_lab = c(TRUE, TRUE, FALSE, TRUE),
#'           regression = TRUE)
#' mcr_chart(hi = hazard_index(v, r),
#'           mcr = maximum_cumulative_ratio(v, r),
#'           thq = top_hazard_quotient(v, r))
#' 
#' mcr_chart(v, r, regions = TRUE, log_transform = FALSE)
#' mcr_chart(v, r,
#'           thq_col = c(a = "blue", b = "green", c = "red", d = "yellow3", e = "grey"),
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
#' mcr_chart(values = list(c(a = 0.1, b = 0.5),
#'                         c(a = 0.2),
#'                         c(b = 0.3, c = 0.4)),
#'           references = c(a = 1, b = 2, c = 3),
#'           log_transform = FALSE)
#' mcr_chart(values = list(c(a = 0.1, b = 0.5),
#'                         c(a = 0.2),
#'                         c(b = 0.3, c = 0.4)),
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
      if (any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, the lengths of their elements must match.")
      if (!is.named(values)[2])
        stop("If values is a list, it must contain vector of named numeric values.")
      
      # Calcul des indicateurs nécessaires
      hi = sapply(seq_len(length(values)), function(i) hazard_index(values[[i]], references[[i]]))
      mcr = sapply(seq_len(length(values)), function(i) maximum_cumulative_ratio(values[[i]], references[[i]]))
      thq = sapply(seq_len(length(values)), function(i) top_hazard_quotient(values[[i]], references[[i]], k = 1))
      
    } else if (is.vector(references)) {
      
      # Vérification que les structures de données sont nommées
      if (!is.named(references) || !is.named(values)[2])
        stop("If values is a list and references is a vector. Both must contained named values.")
      
      # Calcul des indicateurs nécessaires
      hi = sapply(values, function(v) hazard_index(v, references[names(v)]))
      mcr = sapply(values, function(v) maximum_cumulative_ratio(v, references[names(v)]))
      thq = sapply(values, function(v) top_hazard_quotient(v, references[names(v)], k = 1))
      
    } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
    
  } else { # Cas où values est une matrice ou n'est pas renseigné
    
    # Vérification que les structures de données sont nommées
    if (!is.null(values) && !is.named(values)[1]) stop("Rows of values must be named.")
    if (!is.null(thq) && ((is.list(thq) && !is.named(thq)[2]) || !is.named(thq)))
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
    data = data.frame(x = log10(hi), y = log10(mcr - 1), thq = thq)
    
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
    data = data.frame(x = hi, y = mcr, thq = thq)
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
  xmax_fun = xlim[2] + 1
  root_fun = uniroot(fun.mhq_1, c(0, 1))$root   # fun.mhq_1(log10(2)) = 0
  
  # Texte relatif aux groupes
  if (any(regions_lab)) {
    # Vérification des zones affichées (non-affichage du texte des zones qui ne sont pas affichées)
    regions_lab = regions_lab & c(ylim[1] < fun.mhq_1(xlim[2]), # Coin bas-droite en-dessous de la courbe f
                                  xlim[1] < 0,
                                  ylim[1] < 0 && xlim[2] > 0 && (xlim[1] < 0 || ylim[1] > fun.mhq_1(xlim[1])), # Coin bas-gauche au dessus de f
                                  ylim[2] > 0 && xlim[2] > 0 && (xlim[1] < 0 || ylim[2] > fun.mhq_1(xlim[1]))) # Coin haut-gauche au dessus de f
    
    chart = chart + ggplot2::annotate(geom = "text",
                                      x = c(xlim[2], xlim[1], root_fun / 2, if (xlim[1] > 0) xlim[2] - xlim[1] / 2 else xlim[2] / 2)[regions_lab],
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
                  y = bquote(log[10]*"(MCR - 1); MHQ / HI"),
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
                                      x = c(xlim[2], xlim[1], 1.5, xlim[2])[regions_lab],
                                      y = c(ylim[1] + (ylim[2] - ylim[1]) / 100, ylim[2], 1.95, ylim[2])[regions_lab],
                                      hjust = c(1, 0, 0.5, 1)[regions_lab],
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
    ggplot2::labs(x = "HI", y = "MCR; MHQ / HI",
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
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
#'  before identifying the highest hazard quotients then building the contingency table. Thus, call the
#'  function with the arguments `hq` and `hi` is faster (if they are already computed). This is true if
#'  `values` is not a list.
#' 
#' \loadmathjax
#' The hazard index of the vector \eqn{i} is given by:
#'  \mjdeqn{HI_i = \sum_{j = 1}^N HQ_{i,j}}{HI_i = sum(HQ_ij) from j = 1 to N}
#'  where \eqn{HQ} denotes the hazard quotients and \eqn{N} denotes the number of hazard quotients.
#' 
#' The hazard quotient of the value \eqn{j} in the vector \eqn{i} is given by:
#'  \mjdeqn{HQ_{i,j} = \frac{V_{i,j}}{RV_j}}{HQ_ij = V_ij / RV_j}
#'  where \eqn{V} denotes the `values` and \eqn{RV} denotes the `references`.
#' 
#' @note
#' Due to the multiple possible usages, the arguments `hi`, `hq` and `levels` must be explicitly named in
#'  the function call.
#' 
#' @usage
#' thq_pairs_freq(values, references, levels = NULL)
#' thq_pairs_freq(hq, hi, levels = NULL)
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  top two hazard quotients are to be identified.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param hq Numeric named matrix. **H**azard **q**uotients for which the top two pairs are be to
#'  identified.
#' @param hi Numeric vector. **H**azard **i**ndexes associated with the hazard quotients `hq`.
#' @param levels Levels to consider in the output table. If `NULL`, only use of those that appear in the
#'  pairs.
#' @return
#' `NULL` if (1) `values` is a matrix having only one line or (2) `values` is a list and no vector has a
#'  hazard index greater than 1 or more than 1 value or (3) no hazard index is greater than 1.
#' 
#' Contingency table otherwise. Frequency of pairs that produced the top two hazard quotients while hazard
#'  index is greater than 1.
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`thq_freq_by_group`], [`top_hazard_quotient`], [`hazard_quotient`], [`hazard_index`].
#' 
#' @examples
#' thq_pairs_freq(values = matrix(c(1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                ncol = 2, dimnames = list(letters[1:5])),
#'                references = c(1,2,3,4,5))
#' thq_pairs_freq(hq = hazard_quotient(matrix(c(1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                            ncol = 2, dimnames = list(letters[1:5])),
#'                                     c(1,2,3,4,5)),
#'                hi = hazard_index(matrix(c(1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
#'                                         ncol = 2, dimnames = list(letters[1:5])),
#'                                  c(1,2,3,4,5)))
#' 
#' ## With and without levels parameter
#' thq_pairs_freq(values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                                ncol = 3, dimnames = list(letters[1:4])),
#'                references = c(1, 2, 3, .5),
#'                levels = letters[1:4])
#' thq_pairs_freq(values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                                ncol = 3, dimnames = list(letters[1:4])),
#'                references = c(1,2,3,0.5))
#' 
#' ## NULL because all HI are lower than or equal to 1
#' thq_pairs_freq(values = matrix(c(.1, .2, .3, .4),
#'                                ncol = 2, dimnames = list(c("a","b"))),
#'                references = c(5,5))
#' hazard_index(values = matrix(c(.1, .2, .3, .4),
#'                              ncol = 2, dimnames = list(c("a","b"))),
#'              references = c(5,5))
#' 
#' ## Building contingency table from a list
#' thq_pairs_freq(values = list(c(a = 0.5, b = 0.5),
#'                              c(a = 1),
#'                              c(b = 0.5, c = 0.5)),
#'                references = c(a = 0.3, b = 0.6, c = 1))
#' thq_pairs_freq(values = list(c(a = 0.5, b = 0.5),
#'                              c(a = 1),
#'                              c(b = 0.5, c = 0.5)),
#'                references = list(c(0.3, 0.6),
#'                                  0.3,
#'                                  c(0.6, 1)))
#' 
#' @md
#' @export
thq_pairs_freq = function(values = NULL, references = NULL,
                          hq = NULL, hi = NULL,
                          levels = NULL) {
  
  # Cas spécifiques dans lequel values est une liste et non une matrice
  if (is.list(values)) {
    
    # Différence si references est une liste ou un vecteur
    if (is.list(references)) {
      
      # Vérification que les structures de données sont nommées
      if (any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, the lengths of their elements must match.")
      if (!is.named(values)[2])
        stop("If values is a list, it must contain vector of named numeric values.")
      
      # Calcul des indicateurs nécessaires
      hq = sapply(seq_len(length(values)), function(i) hazard_quotient(values[[i]], references[[i]]))
      hi = sapply(seq_len(length(values)), function(i) hazard_index(values[[i]], references[[i]]))
      
    } else if (is.vector(references)) {
      
      # Vérification que les structures de données sont nommées
      if (!is.named(references) || !is.named(values)[2])
        stop("If values is a list and references is a vector. Both must contained named values.")
      
      # Calcul des indicateurs nécessaires
      hq = sapply(values, function(v) hazard_quotient(v, references[names(v)]))
      hi = sapply(values, function(v) hazard_index(v, references[names(v)]))
      
    } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
    
    
    # Si aucun HI n'est supérieur à 1 ou qu'aucun vecteur ne contient plus d'une valeur
    if (all(hi <= 1 | sapply(hq, length) == 1)) return(NULL)
    
    hq_to_use = hq[hi > 1 & sapply(hq, length) != 1]
    
    # Si un seul ensemble de valeurs satisfait le critère HI > 1
    if (length(hq_to_use) == 1) {
      thq = names(top_hazard_quotient(hq = hq_to_use[[1]], k = 2))
      
      if (!is.null(levels)) {
        freq_table = table(factor(thq[1], levels = levels),
                           factor(thq[2], levels = levels))
        # Application de la symétrie
        freq_table[thq[2], thq[1]] = freq_table[thq[1], thq[2]]
        return(freq_table)
      }
      return(table(thq[1], thq[2]))
    }
    
    # Si plusieurs ensembles de valeurs satisfont le critère HI > 1
    thq = sapply(hq_to_use, function(hq) sort(names(top_hazard_quotient(hq = hq, k = 2))))
    
    if (!is.null(levels)) {
      freq_table = table(factor(thq[1, ], levels = levels),
                         factor(thq[2, ], levels = levels))
      # Application de la symétrie
      freq_table[lower.tri(freq_table)] = t(freq_table)[lower.tri(freq_table)]
      return(freq_table)
    }
    return(table(thq[1, ], thq[2, ]))
    
    
  } else { # Cas où values est une matrice
  
    # Vérification que les structures de données sont nommées
    if (!is.null(values) && !is.named(values)[1]) stop("Rows of values must be named.")
    if (!is.null(hq) && !is.named(hq)[1]) stop("Rows of hq must be named.")
    
    # Calcul des données manquantes
    if (is.null(hq)) hq = hazard_quotient(values, references)
    if (is.null(hi)) hi = hazard_index(hq = hq)
    
    # Si une seule ligne (un seul élément) ou aucun HI n'est supérieur à 1
    if (nrow(hq) == 1 || all(hi <= 1)) return(NULL)
    
    hq_to_use = hq[, hi > 1]
    
    # Si un seul ensemble de valeurs satisfait le critère HI > 1
    if (is.vector(hq_to_use)) {
      thq = names(top_hazard_quotient(hq = hq_to_use, k = 2))
      
      if (!is.null(levels)) {
        freq_table = table(factor(thq[1], levels = levels),
                           factor(thq[2], levels = levels))
        # Application de la symétrie
        freq_table[thq[2], thq[1]] = freq_table[thq[1], thq[2]]
        return(freq_table)
      }
      return(table(thq[1], thq[2]))
    }
    
    # Si plusieurs ensembles de valeurs satisfont le critère HI > 1
    thq = apply(hq_to_use, 2, function(hq) sort(names(top_hazard_quotient(hq = hq, k = 2))))
    
    if (!is.null(levels)) {
      freq_table = table(factor(thq[1, ], levels = levels),
                         factor(thq[2, ], levels = levels))
      # Application de la symétrie
      freq_table[lower.tri(freq_table)] = t(freq_table)[lower.tri(freq_table)]
      return(freq_table)
    }
    return(table(thq[1, ], thq[2, ]))
  }
}


#' Top Hazard Quotients frequency by group
#' 
#' Build a contingency table of the counts of each combination of name of the element producing the
#'  top hazard quotient with the associated MIAT group.
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
#'  computed). This is true only if `values` is not a list.
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
#' Due to the multiple possible usages, the arguments `hq`, `thq`, `groups` and `levels` must be
#'  explicitly named in the function call.
#' 
#' @usage
#' thq_freq_by_group(values, references, levels = NULL)
#' thq_freq_by_group(hq, groups, levels = NULL)
#' thq_freq_by_group(thq, groups, levels = NULL)
#' @param values Numeric named matrix or list of numeric named vectors. Vectors of values for which the
#'  table is to be build.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`. See 'Details' to know the way it is associated with `values`.
#' @param hq Numeric named matrix. **H**azard **q**uotients for which the table is to be build.
#' @param thq Numeric named vector. **T**op **h**azard **q**uotients to use to the count.
#' @param thq Numeric named vector or list of numeric named vectors. **T**op **h**azard **q**uotients
#'  to use to the count. If list, only the first named value of each element of the list is considered.
#' @param groups Character vector. MIAT groups associated with the hazard quotients `hq` or with the
#'  top hazard quotients `thq`.
#' @param levels Levels to consider in the output table. If `NULL`, only use of those that appear in the
#'  top hazard quotients.
#' @return Contingency table. Frequency of element that produced the top hazard quotient with its
#'  associated group.
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`thq_pairs_freq`], [`classify_mixture`], [`top_hazard_quotient`], [`hazard_quotient`].
#' 
#' @examples
#' ## Creating a matrix of 4*3 values and one reference value for each of the 4
#' ## elements ("a", "b", "c", and "d").
#' v <- matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'             ncol = 3, dimnames = list(letters[1:4]))
#' r <- c(1, 2, 3, 0.5)
#' 
#' ## Without levels parameter and with the different usages
#' thq_freq_by_group(v, r)
#' thq_freq_by_group(hq = hazard_quotient(v, r),
#'                   groups = classify_mixture(v, r))
#' thq_freq_by_group(thq = top_hazard_quotient(v, r),
#'                   groups = classify_mixture(v, r))
#' 
#' ## With levels parameter
#' thq_freq_by_group(values = v, references = r, levels = letters[1:4])
#' 
#' ## Building contingency table from a list
#' thq_freq_by_group(values = list(c(a = 0.1, b = 0.5),
#'                                 c(a = 0.2),
#'                                 c(b = 0.3, c = 0.4)),
#'                   references = c(a = 1, b = 2, c = 3))
#' thq_freq_by_group(values = list(c(a = 0.1, b = 0.5),
#'                                 c(a = 0.2),
#'                                 c(b = 0.3, c = 0.4)),
#'                   references = list(c(1, 2),
#'                                     1,
#'                                     c(2, 3)))
#' 
#' @md
#' @export
thq_freq_by_group = function(values = NULL, references = NULL,
                             hq = NULL,
                             thq = NULL,
                             groups = NULL, levels = NULL) {
  
  # Cas spécifiques dans lequel values est une liste et non une matrice
  if (is.list(values)) {
    
    # Différence si references est une liste ou un vecteur
    if (is.list(references)) {
      
      # Vérification que les structures de données sont nommées
      if (any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, the lengths of their elements must match.")
      if (!is.named(values)[2])
        stop("If values is a list, it must contain vector of named numeric values.")
      
      # Calcul des indicateurs nécessaires
      thq = sapply(seq_len(length(values)), function(i) top_hazard_quotient(values[[i]], references[[i]], k = 1))
      groups = sapply(seq_len(length(values)), function(i) classify_mixture(values[[i]], references[[i]]))
      
    } else if (is.vector(references)) {
      
      # Vérification que les structures de données sont nommées
      if (!is.named(references) || !is.named(values)[2])
        stop("If values is a list and references is a vector. Both must contained named values.")
      
      # Calcul des indicateurs nécessaires
      thq = sapply(values, function(v) top_hazard_quotient(v, references[names(v)], k = 1))
      groups = sapply(values, function(v) classify_mixture(v, references[names(v)]))
      
    } else stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
    
  } else { # Cas où values est une matrice ou n'est pas renseigné
  
    # Vérification que les structures de données sont nommées
    if (!is.null(values) && !is.named(values)[1]) stop("Rows of values must be named.")
    if (!is.null(hq) && !is.named(hq)[1]) stop("Rows of hq must be named.")
    if (!is.null(thq) && ((is.list(thq) && !is.named(thq)[2]) || !is.named(thq)))
      stop("thq must be a vector of named numeric values or a list of such vectors.")
    
    # Calcul des données manquantes
    if (is.null(thq)) {
      if (is.null(hq)) hq = hazard_quotient(values, references)
      thq = top_hazard_quotient(hq = hq,  k = 1)
    }
    if (is.null(groups)) groups = classify_mixture(values, references)
  }
  
  # Soit thq est une list (hq ou values est une matrix) soit thq est une valeur (hq ou values est un vector)
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
#' @param values Numeric named vector or matrix, or list of numeric named vectors. Data structure
#'  whose naming is to be checked.
#' @param references Numeric vector or list of numeric vectors. Reference values associated with the
#'  `values`.
#' @param vector Should be `TRUE` if `values` can be a vector.
#' @param matrix Should be `TRUE` if `values` can be a matrix.
#' @param list Should be `TRUE` if `values` can be a list.
#' 
#' @author Gauthier Magnin
#' @seealso [`mcr_chart_by_class`], [`thq_pairs_freq_by_class`], [`thq_freq_by_group_by_class`].
#' @md
#' @keywords internal
check_data_for_mcr_by_class = function(values, references, vector = TRUE, matrix = TRUE, list = TRUE) {
  
  # Vérification du nommage de values et references si values peut être une liste, une matrice, un vecteur
  if (list && is.list(values)) {
    if (is.list(references)) {
      if (any(sapply(values, length) != sapply(references, length)))
        stop("If values and references are two lists, the lengths of their elements must match.")
      if (!is.named(values)[2])
        stop("If values is a list, it must contain vector of named numeric values.")
      
    } else if (is.vector(references)) {
      if (!is.named(references) || !is.named(values)[2])
        stop("If values is a list and references is a vector, both must contained named values.")
    } else
      stop("If values is a list, references must be a named vector or a list having the exact same lengths as values.")
  }
  else if (matrix && is.matrix(values) && !is.named(values)[1]) stop("If values is a matrix, its rows must be named.")
  else if (vector && is.vector(values) && !is.named(values)) stop("If values is a vector, it must have named numeric values.")
}


#' Summary by class of indicators of the MCR approach
#' 
#' Compute a set of indicators of the MCR approach according to classes, given values and references.
#'  Indicators are computed for each set of values and for each class. For each class, only values
#'  corresponding to this class are considered.
#' 
#' @inherit mcr_summary details
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
#' @return Data frame or list of data frames (according to `values`) of the main indicators of the MCR
#'  approach, computed on the given `values` and for each class encountered:
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
#' @seealso [`mcr_summary`], [`mcr_chart_by_class`], [`thq_pairs_freq_by_class`],
#'          [`thq_freq_by_group_by_class`].
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
mcr_summary_by_class = function(values, references, classes) {
  
  # Vérification des types des paramètres values et references
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
                         function(i) mcr_summary_by_class(values[[i]], references[[i]], classes))
      return(stats::setNames(to_return, names(values)))
    }
    return(lapply(values, function(v) mcr_summary_by_class(v, references[names(v)], classes)))
  }
  
  # Cas d'une matrice valeurs
  if (is.matrix(values)) {
    # Pour chaque colonne de valeurs, calcul des indicateurs MCR pour chaque classe
    return(apply(values, 2, function(v) mcr_summary_by_class(v, references, classes)))
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
      if (length(v) == 0) return(NA)
      return(mcr_summary(v, r[!is.na(r)]))
    })
    # Retrait des classes pour lesquelles il n'y a aucune valeur
    summary = summary[!is.na(summary)]
    
    # Conversion en deux temps car les facteurs sont transformés en numeric
    to_return = data.frame(matrix(unlist(summary), nrow = length(summary), byrow = TRUE))
    rownames(to_return) = names(summary)
    colnames(to_return) = names(summary[[1]])
    to_return[, "Group"] = sapply(summary, "[[", "Group")
    to_return[, "THQ"] = sapply(summary, "[[", "THQ")
    
    return(to_return)
  }
}


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
#' @param classes Logical matrix whose columns are named according to the classes and the row names
#'  contain the names associated with the `values`. A `TRUE` value indicates that a specific name
#'  is part of a specific class.
#' @param class_name Name of the class of interest, the one for which to extract the corresponding
#'  subsets of `values` and `references`.
#' @return \describe{
#'  \item{`values`}{Subset of values that correspond to the class of interest.}
#'  \item{`references`}{Subset of references that correspond to the class of interest.}
#' }
#' 
#' @author Gauthier Magnin
#' @seealso [`mcr_summary_by_class`], [`mcr_chart_by_class`], [`thq_pairs_freq_by_class`],
#'          [`thq_freq_by_group_by_class`].
#' @md
#' @keywords internal
subset_from_class = function(values, references, classes, class_name) {
  
  # Cas d'une liste de valeurs
  if (is.list(values)) {
    
    # Noms des éléments qui correspondent à la classe
    items_in_class = rownames(classes)[classes[, class_name]]
    
    # Sous-ensembles de values et references correspondant aux éléments de la classe
    values_class = list()
    if (is.list(references)) {
      references_class = list()
    } else {
      references_class = references[items_in_class]
      # Retrait des NA (lorsque des noms associés à la classe ne font pas partie des références)
      references_class = references_class[!is.na(references_class)]
    }
    nb_elements = 0
    
    # Ajout aux nouveaux ensembles de chaque élément qui fait partie de la classe
    for (i in seq_along(values)) {
      indices = names(values[[i]]) %in% items_in_class
      
      if (sum(indices) != 0) {
        nb_elements = nb_elements + 1
        values_class[[nb_elements]] = values[[i]][indices]
        if (is.list(references))  references_class[[nb_elements]] = references[[i]][indices]
      }
    }
  }
  # Cas d'une matrice valeurs
  else if (is.matrix(values)) {
    
    # Extraction des valeurs correpondant à la classe
    # et retrait des NA (lorsque des noms associés à la classe ne font pas partie des valeurs)
    indices = match(rownames(classes)[classes[, class_name]], rownames(values))
    values_class = values[indices[!is.na(indices)], ]
    references_class = references[indices[!is.na(indices)]]
    
    # Reconversion en matrice s'il n'y avait qu'une seule ligne
    if (is.vector(values_class)) {
      values_class = t(values_class)
      rownames(values_class) = rownames(values)[indices[!is.na(indices)]]
    }
  }
  
  return(list(values = values_class, references = references_class))
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
#' @param plot If `FALSE`, the charts are not plotted. If `TRUE`, the charts are all plotted in the
#'  active graphics device.
#' @return List of charts created with the `ggplot2` package or `NULL` if no points can be plotted
#'  (see 'Details'). The length of the list corresponds to the number of classes encountered.
#' 
#' @author Gauthier Magnin
#' @inherit mcr_chart references
#' @seealso [`mcr_chart`], [`mcr_summary_by_class`], [`thq_pairs_freq_by_class`],
#'          [`thq_freq_by_group_by_class`].
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
  
  # Vérification des types des paramètres values et references
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
    if (sum(class_warnings) == 1) message("One warning message in chart plotting for class ",
                                          names(class_warnings)[class_warnings], ":")
    else message("Warnings messages in chart plotting for classes ",
                 paste(names(class_warnings)[class_warnings], collapse = ", "), ":")
  }
  
  return(charts[!is.na(charts)])
}
