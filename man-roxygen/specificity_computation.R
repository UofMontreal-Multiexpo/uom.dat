
#' @details
#' 
#' The specificy of the pattern \eqn{p} is given by:
#'  \mjdeqn{S_p = \frac{\max_{q \in P} H_{max,q} - H_p}{\max_{q \in P} H_{max,q} - \min_{q \in P} H_{min,q}}}{S_p = (max H_max - H_p) / (max H_max - min H_min)}
#' where \eqn{P} is the set of patterns and:
#' \describe{
#'  \item{}{\mjeqn{H_p = - \sum_{v = 1}^{U_p} \frac{A_{p,v}}{W_p} \times ln \left( \frac{A_{p,v}}{W_p} \right)}{H_p = - sum(A_pv / W_p * ln(A_pv / W_p)) from v = 1 to U_p}}
#'  \item{}{\mjeqn{H_{max,p} = ln(U_p)}{H_max,p = ln(U_p)}}
#'  \item{}{\mjeqn{H_{min,p} = ln \left( \frac{W_p}{W_p - U_p + 1} \right) + \left( \frac{U_p - 1}{W_p} \times ln(W_p - U_p + 1) \right)}{H_min,p = ln(W_p / (W_p - U_p + 1)) + (U_p - 1) / W_p * ln(W_p - U_p + 1)}}
#'  \item{}{where \mjseqn{A_p} is the set of weights of the nodes containing the pattern \eqn{p},
#'          \eqn{W} is the set of weights of the patterns and
#'          \eqn{U} is the set of frequencies of the patterns.}
#' }
