
#' @details
#' 
#' The specificy of the pattern \mjseqn{p} is given by:
#'  \mjdeqn{S_p = \begin{cases} 1 & \mbox{if } W_p = 1 \cr 0 & \mbox{if } A_{p,1} = A_{p,2} = \ldots = A_{p,W_p} \cr \frac{H_{max,p} - H_p}{H_{max,p} - H_{min,p}} & \mbox{otherwise.} \end{cases}}{S_p = 1 if W_p = 1, 0 if A_p1 = A_p2 = ... = A_{p,F_p}, (H_max,p - H_p) / (H_max,p - H_min,p) otherwise.}
#' where \mjseqn{W_p} is the weight of the pattern \mjseqn{p}, \mjseqn{A_p} is the
#' set of frequencies of the nodes containing the pattern \mjseqn{p} and:
#' \describe{
#'  \item{}{\mjeqn{H_p = - \sum_{i = 1}^{W_p} \frac{A_{p,i}}{F_p} \times ln \left( \frac{A_{p,i}}{F_p} \right)}{H_p = - sum(A_pi / F_p * ln(A_pi / F_p)) from i = 1 to W_p}}
#'  \item{}{\mjeqn{H_{max,p} = ln(F_p)}{H_max,p = ln(F_p)}}
#'  \item{}{\mjeqn{H_{min,p} = ln \left( \frac{F_p}{F_p - W_p + 1} \right) + \left( \frac{W_p - 1}{F_p} \times ln(F_p - W_p + 1) \right)}{H_min,p = ln(F_p / (F_p - W_p + 1)) + (W_p - 1) / F_p * ln(F_p - W_p + 1)}}
#'  \item{}{where \mjseqn{F_p} is the frequency of the pattern \mjseqn{p}.}
#' }
