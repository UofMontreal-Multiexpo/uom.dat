
#' @details
#' 
#' The specificy of the pattern \eqn{p} is given by:
#'  \mjdeqn{S_p = \begin{cases} 1 & \mbox{if } U_p = 1 \cr 0 & \mbox{if } A_{p,1} = A_{p,2} = \ldots = A_{p,U_p} \cr \frac{H_{max,p} - H_p}{H_{max,p} - H_{min,p}} & \mbox{otherwise} \end{cases}}{S_p = 1 if U_p = 1, 0 if A_p1 = A_p2 = ... = A_{p,U_p}, (H_max,p - H_p) / (H_max,p - H_min,p) otherwise}
#' where \mjseqn{U} is the set of frequencies of the patterns, \mjseqn{A_p} is the
#' set of weights of the nodes containing the pattern \eqn{p} and:
#' \describe{
#'  \item{}{\mjeqn{H_p = - \sum_{i = 1}^{U_p} \frac{A_{p,i}}{W_p} \times ln \left( \frac{A_{p,i}}{W_p} \right)}{H_p = - sum(A_pi / W_p * ln(A_pi / W_p)) from i = 1 to U_p}}
#'  \item{}{\mjeqn{H_{max,p} = ln(U_p)}{H_max,p = ln(U_p)}}
#'  \item{}{\mjeqn{H_{min,p} = ln \left( \frac{W_p}{W_p - U_p + 1} \right) + \left( \frac{U_p - 1}{W_p} \times ln(W_p - U_p + 1) \right)}{H_min,p = ln(W_p / (W_p - U_p + 1)) + (U_p - 1) / W_p * ln(W_p - U_p + 1)}}
#'  \item{}{where \mjseqn{W} is the set of weights of the patterns.}
#' }
