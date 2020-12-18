
#' @details
#' 
#' For each pattern, two reporting index values are computed and compared to two thresholds in order
#'  to define its status.
#' 
#' A pattern is:
#' * Persistent when it appears substantially during both the entire period of the
#'   observations (defined by the arguments `period` and `t`) and a shorter period defined by the
#'   arguments `short_limit` and `t`.
#' * Declining when it appears substantially during the entire period but much less during
#'   the shorter period.
#' * Emergent when it appears insignificantly during the entire period but much more during
#'   the shorter period.
#' * Latent when it appears insignificantly during both the entire period and the shorter period.
#' 
#' The reporting indexes of the pattern \eqn{p} which are computed are those at the two temporal limits
#'  given by:
#'  \mjdeqn{RI_{\infty,p} \; = lim_{t_0 \to -\infty} RI_p(t_1,t_0)}{RI_inf,p = lim of RI_p(t_1,t_0) as t approaches -inf}
#'  \mjdeqn{RI_{l,p} = RI_p(t_1, t_1 - l + 1)}{RI_lp = RI_p(t_1, t_1 - l + 1)}
#' where \eqn{l} is the shorter period on which to compute a reporting index and \mjseqn{RI_p(t_1,t_0)}
#'  is the reporting index of the pattern \eqn{p} given by:
#'  \mjdeqn{RI_p(t_1,t_0) = \frac{\sum_{t = t_0}^{t_1} W_{p,t}}{\sum_{q \in P} \sum_{t = t_0}^{t_1} W_{q,t}}}{RI_p(t_1,t_0) = sum W_pt from t = t_0 to t_1 / sum W_{q,t} for q in P and from t = t_0 to t_1}
#' where \eqn{P} is the set of patterns, \mjeqn{W_{p,t}}{W_pt} is the weight of the pattern \eqn{p} in
#'  the observations of the year \eqn{t}, \mjseqn{t_0} and \mjseqn{t_1} are the first and last years
#'  defining the period on which to compute the reporting index.
#' 
#' For each set of reporting indexes:
#' * One threshold \mjseqn{\xi} is computed as follows:
#'   \mjdeqn{\xi = \frac{1}{\sum_{p \in P} RI_p(t_1,t_0)^2}}{xi = 1 / sum(RI_p(t_1,t_0)^2) for p in P}
#' * The patterns are ordered in descending order of their reporting index value and separated by
#'   this threshold.
#' * The reporting index of the \mjseqn{\xi}\out{<sup>th</sup>} pattern is the \eqn{RI} threshold used to
#'   classify the patterns.
#' 
#' The patterns are then classified as follows:
#' * Patterns for which \mjeqn{RI_{\infty,p}}{RI_inf,p} and \mjeqn{RI_{l,p}}{RI_lp} are greater than or
#'   equal to the \eqn{RI} thresholds are classified as "Persistent".
#' * Patterns for which \mjeqn{RI_{\infty,p}}{RI_inf,p} is greater than or equal to the related threshold
#'   and \mjeqn{RI_{l,p}}{RI_lp} is lower than the related threshold are classified as "Declining".
#' * Patterns for which \mjeqn{RI_{\infty,p}}{RI_inf,p} is lower than the related threshold and
#'   \mjeqn{RI_{l,p}}{RI_lp} is greater than or equal to the related threshold are classified as "Emergent".
#' * Patterns for which \mjeqn{RI_{\infty,p}}{RI_inf,p} and \mjeqn{RI_{l,p}}{RI_lp} are lower than the
#'   thresholds are classified as "Latent".
#' 
#' @md
