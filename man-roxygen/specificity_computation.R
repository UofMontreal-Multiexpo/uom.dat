
#' @details
#' 
#' The specificy of the pattern \eqn{p} is given by:
#'  \mjdeqn{S_p = \frac{H_{max} - H_p}{H_{max} - H_{min}}}{S_p = (H_max - H_p) / (H_max - H_min)}
#' where:
#' \describe{
#'  \item{}{\mjeqn{H_p = - \sum_{v = 1}^{U_p} \frac{A_{p,v}}{W_p} \times log \left( \frac{A_{p,v}}{W_p} \right)}{H_p = - sum(A_pv / W_p * log(A_pv / W_p)) from v = 1 to U_p}}
#'  \item{}{\mjeqn{H_{max,p} = log(U_p)}{H_max,p = log(U_p)}}
#'  \item{}{\mjeqn{H_{min,p} = log \left( \frac{W_p}{W_p - U_p + 1} \right) + \left( \frac{U_p - 1}{W_p} \times log(W_p - U_p + 1) \right)}{H_min,p = log(W_p / (W_p - U_p + 1)) + (U_p - 1) / W_p * log(W_p - U_p + 1)}}
#'  \item{}{where \mjseqn{A_p} is the set of weights of the nodes containing the pattern \eqn{p},
#'          \eqn{W} is the set of weights of the patterns and
#'          \eqn{U} is the set of frequencies of the patterns.}
#' }
