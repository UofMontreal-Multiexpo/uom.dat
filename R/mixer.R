
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
#'  highest ones. Thus, call the function with the argument `hq` is faster (if it is already computed).
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
#'  function with the arguments `hq` and `hi` is faster (if they are already computed).
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


#' Top Hazard Quotients frequency by group
#' 
#' Build a contingency table of the counts of each combination of name of the element producing the
#'  top hazard quotient with the associated MIAT group.
#' 
#' @details
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



#### Maximum Cumulative Ratio - summary functions ####

#' MCR approach scatter plot
#' 
#' Plot \mjeqn{log_{10}(HI)}{log10(HI)} versus \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)} with the
#'  percentage of the reciprocal of the maximum cumulative ratio, the elements producing the top hazard
#'  quotients and the associated MIAT groups.
#' 
#' @details
#' The chart being plotted with the `ggplot2` package, it can be modified or completed afterwards using
#'  [`ggplot2::last_plot`] or the returned chart.
#' 
#' Color specification can be done using the R predefined color names or hexadecimal values.
#'  
#' Arguments `values` and `references` are used to compute the hazard quotients and the hazard indexes
#'  before searching for the top and maximum hazard quotients, computing the maximum cumulative ratios,
#'  performing the classification then plot the chart. Thus, call the function with the arguments `hi`,
#'  `mcr`, `thq` and `groups` is faster (if they are already computed).
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
#'  \mjdeqn{Reciprocal of MCR_i = \frac{1}{MCR_i} = \frac{MHQ_i}{HI_i}}{Reciprocal of MCR_i = 1 / MCR_i = MHQ_i / HI_i}
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
#'           thq_col = NULL, regions = FALSE,
#'           regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"), regions_alpha = 0.2,
#'           regions_lab = !regions, regression = FALSE)
#' mcr_chart(hi, mcr, thq, groups,
#'           thq_col = NULL, regions = FALSE,
#'           regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"), regions_alpha = 0.2,
#'           regions_lab = !regions, regression = FALSE)
#' @param values Numeric named matrix. Vectors of values for which the chart is to be plotted.
#' @param references Numeric vector. Reference values associated with the `values`.
#' @param hi Numeric vector. **H**azard **i**ndexes for which the chart is to be plotted.
#' @param mcr Numeric vector. **M**aximum **c**umulative **r**atios associated with the hazard indexes
#'  `hi`.
#' @param thq Numeric named vector or list of numeric named vectors. **T**op **h**azard **q**uotients
#'  associated with the hazard indexes `hi`. If list, only the first named value of each element of the
#'  list is considered.
#' @param groups Character vector. MIAT groups associated with the hazard indexes `hi`.
#' @param thq_col Character named vector. Colors to assign to the **t**op **h**azard **q**uotients
#'  elements.
#' @param regions If `TRUE`, the regions corresponding to the MIAT groups are filled with the colors
#'  defining by `regions_col`.
#' @param regions_col Character vector of length 4. Define the colors for the regions of the MIAT groups
#'  (in order: I, II, IIIA and IIIB).
#' @param regions_alpha Value between 0 and 1. Transparency of the regions filled with `regions_col`.
#' @param regios_lab Logical value or vector of length 4. Define if labels for the MIAT groups should
#'  be displayed (in order: I, II, IIIA, IIIB). `TRUE` and `FALSE` are special values for all `TRUE` or
#'  all `FALSE`.
#' @param regression If `TRUE`, the linear regression between \mjeqn{log_{10}(HI)}{log10(HI)} and
#'  \mjeqn{log_{10}(MCR - 1)}{log10(MCR - 1)} with 95% confidence interval is represented.
#' @return Invisible. Chart created with the `ggplot2` package.
#' 
#' @author Gauthier Magnin
#' @references Reyes JM, Price PS (2018).
#'             An analysis of cumulative risks based on biomonitoring data for six phthalates using the Maximum Cumulative Ratio.
#'             *Environment International*, 112, 77-84.
#'             <https://doi.org/10.1016/j.envint.2017.12.008>.
#' @seealso [`maximum_cumulative_ratio`], [`hazard_index`], [`reciprocal_of_mcr`], [`top_hazard_quotient`].
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
#'           thq = top_hazard_quotient(v, r),
#'           groups = classify_mixture(v, r))
#' 
#' @md
#' @export
mcr_chart = function(values = NULL, references = NULL,
                     hi = NULL, mcr = NULL, thq = NULL, groups = NULL,
                     thq_col = NULL, regions = FALSE,
                     regions_col = c("#b3cde3", "#edf8fb", "#8c96c6", "#88419d"), regions_alpha = 0.2,
                     regions_lab = !regions, regression = FALSE) {
  
  if (is.null(hi)) hi = hazard_index(values, references)
  if (is.null(mcr)) mcr = maximum_cumulative_ratio(values, references, hi = hi)
  if (is.null(thq)) thq = top_hazard_quotient(values, references, k = 1)
  if (is.null(groups)) groups = classify_mixture(hi = hi, mhq = sapply(thq, "[", 1), mcr = mcr)
  
  # Récupération des noms des top et calcul des réciproques de mcr
  thq = sapply(unname(thq), function(v) names(v)[1])
  rmcr = reciprocal_of_mcr(mcr = mcr)
  
  # Préparation des données, fonctions et limites pour le graphique
  data = data.frame(HI = hi, MCR = mcr, Reciprocal = rmcr, Group = groups, THQ = thq,
                    x = log10(hi), y = log10(mcr - 1))
  xlim = c(floor(min(data$x)), ceiling(max(data$x)))
  ylim = c(floor(min(data$y)), ceiling(max(data$y)))
  fun.mhq_1 = function(x) log10(10^x - 1)
  xmin_fun = 0.001
  xmax_fun = xlim[2] + 1
  root_fun = uniroot(fun.mhq_1, c(0, 1))$root   # fun.mhq_1(log10(2)) = 0
  
  
  # Initialisation du graphique
  chart = ggplot2::ggplot(data = data, ggplot2::aes(x = x, y = y))
  
  # Coloration des régions
  if (regions) {
    # Default colors are colorblind safe and print friendly
    
    chart = chart +
      # A gauche, groupe II
      ggplot2::geom_polygon(data = data.frame(x = c(-Inf, -Inf, 0, 0),
                                              y = c(-Inf, Inf, Inf, -Inf)),
                            ggplot2::aes(fill = "II"),
                            alpha = regions_alpha) +
      # A droite, groupe I + délimitation
      ggplot2::geom_polygon(data = data.frame(x = c(seq(0, xmax_fun, by = 0.001), xmax_fun),
                                              y = c(-Inf, fun.mhq_1(seq(xmin_fun, xmax_fun, by = 0.001)), -Inf)),
                            ggplot2::aes(fill = "I", x = x, y = y),
                            color = "black", linetype = "longdash", alpha = regions_alpha) +
      # Au centre, groupe IIIA
      ggplot2::geom_polygon(data = data.frame(x = c(0, seq(0, root_fun, by = 0.001)),
                                              y = c(0, -Inf, fun.mhq_1(seq(xmin_fun, root_fun, by = 0.001)))),
                            ggplot2::aes(fill = "IIIA", x = x, y = y),
                            alpha = regions_alpha) +
      # En haut, groupe IIIB
      ggplot2::geom_polygon(data = data.frame(x = c(0, 0, seq(root_fun, xmax_fun, by = 0.001), xmax_fun, xmax_fun),
                                              y = c(Inf, 0, fun.mhq_1(seq(root_fun, xmax_fun, by = 0.001)), fun.mhq_1(xmax_fun), Inf)),
                            ggplot2::aes(fill = "IIIB", x = x, y = y),
                            alpha = regions_alpha) +
      # Légende associée
      ggplot2:: scale_fill_manual(values = setNames(regions_col, c("I", "II", "IIIA", "IIIB")),
                                  name = "MIAT groups")
  } else {
    # Courbe délimitant le groupe I
    chart = chart + ggplot2::stat_function(fun = fun.mhq_1, xlim = c(xmin_fun, xmax_fun),
                                           color = "black", linetype = "longdash")
  }
  
  # Suite du graphique
  chart = chart + 
    ggplot2::geom_point(ggplot2::aes(color = factor(thq))) +
    # Segment horizontal séparant IIIA et IIIB
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = root_fun, yend = 0),
                          color = "black", linetype = "longdash") +
    # Droite vertical séparant le groupe II du reste
    ggplot2::geom_vline(xintercept = 0, color = "black", linetype = "longdash") +
    # Titres des axes et légende
    ggplot2::labs(x = bquote(log[10]*"(HI)"),
                  y = bquote(log[10]*"(MCR - 1); MHQ / HI"),
                  col = "Top Hazard Quotients") +
    # Ajout de la réciproque de MCR aux labels de l'axe Y
    ggplot2::scale_y_continuous(labels = function(y) {
      return(paste0(format(round(y, 1), nsmall = 1), "\n",
                    format(round(reciprocal_of_mcr(mcr = 10^y + 1) * 100, 1), nsmall = 1), "%"))
      # 10^y + 1 => valeur de mcr à partir de y = log10(mcr - 1)
    }) +
    ggplot2::theme_bw()
  
  # Si des couleurs spécifiques doivent être associés aux THQ
  if (!is.null(thq_col)) chart = chart + ggplot2::scale_color_manual(values = thq_col)
  
  # Texte relatif aux groupes
  if (any(regions_lab)) {
    # Vérification des zones affichées (non-affichage du texte des zones qui ne sont pas affichées)
    regions_lab = regions_lab & c(xlim[2] > 0, xlim[1] < 0, ylim[1] < 0, ylim[2] > 0)
    
    chart = chart + ggplot2::annotate(geom = "text",
                                      x = c(xlim[2], xlim[1], root_fun / 2, xlim[2] / 2)[regions_lab],
                                      y = c(ylim[1], ylim[1], -0.25, ylim[2])[regions_lab],
                                      hjust = c(1, 0, 0.5, 0.5)[regions_lab],
                                      vjust = c(0, 0, 0.5, 1)[regions_lab],
                                      label = c("Group I", "Group II", "Group IIIA", "Group IIIB")[regions_lab])
  }
  
  # Régression linéaire
  if (regression) chart = chart + ggplot2::geom_smooth(method = "lm", formula = y ~ x)
  
  # Recadrage du graphique
  chart = chart + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  
  graphics::plot(chart)
}


