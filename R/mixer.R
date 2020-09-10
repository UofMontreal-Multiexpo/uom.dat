
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
#' Arguments `values` and `references` are used to compute the hazard quotients before computing the
#'  hazard index. Thus, call the function with the argument `hq` is faster.
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
#'  the maximum hazard quotient. Thus, call the function with the argument `hq` is faster.
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
#'  Thus, call the function with the arguments `hi` and `mhq` is faster.
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
#'  computing the missed toxicity. Thus, call the function with the argument `mcr` is faster.
#' 
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a matrix (or `mcr` is a vector larger than 1), one missed toxicity value is computed
#'  for each column (or value, respectively).
#' 
#' \loadmathjax
#' The missed toxicity of the vector \eqn{i} is given by:
#'  \mjdeqn{Missed toxicity_i = 1 - \frac{1}{MCR_i}}{Missed toxiciy_i = 1 - 1 / MCR_i}
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
#'  call it with the argument `mcr` is even faster.
#' 
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` is a matrix (or `hi` and `mhq` are vectors larger than 1, or `mcr` is such a vector),
#' one reciprocal of maximum cumulative ratio is computed for each column (or value, respectively).
#' 
#' \loadmathjax
#' The reciprocal of the maximum cumulative ratio of the vector \eqn{i} is given by:
#'  \mjdeqn{Reciprocal of MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
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
#'  highest ones. Thus, call the function with the argument `hq` is faster.
#' 
#' If `values` is a matrix, the reference values are applied once on each column (i.e. it must have one
#'  reference value for each row of the matrix).
#' 
#' If `values` or `hq` is a matrix, `k` hazard quotients are highlight for each column.
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


#' Top Hazard Quotient pairs frequency
#' 
#' Build a contingency table of the counts of each combination of the top two hazard quotients pairs for
#'  which the associated hazard index is greater than 1.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
#'  before identifying the highest hazard quotients then building the contingency table. Thus, call the
#'  function with the arguments `hq` and `hi` is faster.
#'  
#' The reference values are applied once on each column of `values` to compute the hazard indexes
#'  (i.e. it must have one reference value for each row of the matrix).
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
#' @param values Numeric named matrix. Vectors of values for which the top two hazard quotients are to be
#'  identified.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hq Numeric named matrix. **H**azard **q**uotients for which the top two pairs are be to
#'  identified.
#' @param hi Numeric vector. **H**azard **i**ndexes associated with the hazard quotients `hq`.
#' @param levels Levels to consider in the output table. If `NULL`, only use of those that appear in the
#'  pairs.
#' @return
#' `NULL` if no hazard index is greater than 1.
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
#' @md
#' @export
thq_pairs_freq = function(values = NULL, references = NULL,
                          hq = NULL, hi = NULL,
                          levels = NULL) {
  
  if (is.null(hq)) hq = hazard_quotient(values, references)
  if (is.null(hi)) hi = hazard_index(hq = hq)
  
  # Si aucun HI n'est supérieur à 1
  if (all(hi <= 1)) return(NULL)
  
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


#' Classify mixture into the four MIAT groups
#' 
#' Classify mixtures into four groups according to the CEFIC-MIAT (Mixtures Industry Ad-hoc Team)
#'  decision tree, each one requiring a different risk management strategy.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
#'  before searching for the maximum hazard quotients, computing the maximum cumulative ratios then
#'  classifying the mixtures. Thus, call the function with the arguments `hi` and `mhq` is faster and
#'  call it with the argument `mcr` is even faster.
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


#' Top Hazard Quotients frequency by group
#' 
#' Build a contingency table of the counts of each combination of name of the element producing the
#'  top hazard quotient with the associated MIAT group.
#' 
#' @details
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
#'  before searching for the maximum hazard quotients, computing the maximum cumulative ratios, performing
#'  the classification then build the table. Thus, call the function with the arguments `hq` and `groups`
#'  is faster and call it with the arguments `thq` and `groups` is even faster.
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
#' @param values Numeric named matrix. Vectors of values for which the table is to be build.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hq Numeric named matrix. **H**azard **q**uotients for which the table is to be build.
#' @param thq Numeric named vector. **T**op **h**azard **q**uotients to use to the count.
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
#' thq_freq_by_group(c(a = 1, b = 2, c = 3, d = 4, e = 5), c(5,4,3,2,1))
#' thq_freq_by_group(hq = hazard_quotient(c(a = 1, b = 2, c = 3, d = 4, e = 5),
#'                                        c(5,4,3,2,1)),
#'                   groups = classify_mixtures(c(a = 1, b = 2, c = 3, d = 4, e = 5),
#'                                              c(5,4,3,2,1)))
#' thq_freq_by_group(thq = top_hazard_quotient(c(a = 1, b = 2, c = 3, d = 4, e = 5),
#'                                             c(5,4,3,2,1)),
#'                   groups = classify_mixtures(c(a = 1, b = 2, c = 3, d = 4, e = 5),
#'                                              c(5,4,3,2,1)))
#' 
#' ## With and without levels parameter
#' thq_freq_by_group(values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                                   ncol = 3, dimnames = list(letters[1:4])),
#'                   references = c(1,2,3,0.5))
#' thq_freq_by_group(values = matrix(c(.1, .2, 1, .4, .5, .6, .7, .8, 3, 1, 1, 1),
#'                                   ncol = 3, dimnames = list(letters[1:4])),
#'                   references = c(1,2,3,0.5),
#'                   levels = letters[1:4])
#' 
#' @md
#' @export
thq_freq_by_group = function(values = NULL, references = NULL,
                             hq = NULL,
                             thq = NULL,
                             groups = NULL, levels = NULL) {
  
  if (is.null(thq)) {
    if (is.null(hq)) hq = hazard_quotient(values, references)
    thq = top_hazard_quotient(hq = hq,  k = 1)
  }
  if (is.null(groups)) groups = classify_mixtures(values, references)
  
  # Soit thq est une list (hq ou values est une matrix) soit thq est une valeur (hq ou values est un vector)
  thq_names = if(is.list(thq)) sapply(thq, names) else names(thq)
  if (!is.null(levels)) thq_names = factor(thq_names, levels = levels)
  freq_table = table(thq_names, factor(groups, levels = c("I", "II", "IIIA", "IIIB")))
  
  # Retrait du nom de dimension "thq_names" et retour
  dimnames(freq_table) = unname(dimnames(freq_table))
  return(freq_table)
}


