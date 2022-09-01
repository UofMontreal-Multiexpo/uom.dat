
#' @details
#' 
#' For each pattern, two reporting index values are computed and compared to two thresholds in order
#'  to define its status.
#' 
#' A pattern is:
#' * Persistent when it appears substantially during both the entire period of the
#'   transactions (defined by the arguments `overall_period` and `end`) and a shorter period defined by the
#'   arguments `recent_period` and `end`.
#' * Declining when it appears substantially during the entire period but much less during
#'   the shorter period.
#' * Emergent when it appears insignificantly during the entire period but much more during
#'   the shorter period.
#' * Latent when it appears insignificantly during both the entire period and the shorter period.
#' 
#' The reporting indexes of the pattern \mjseqn{p} are computed at the two temporal limits
#'  \mjseqn{l_1} and \mjseqn{l_2} and are given by:
#'  \mjdeqn{RI_{l_1,p} = RI_p(t_1, t_1 - l_1 + 1)}{RI_l1p = RI_p(t1, t1 - l1 + 1)}
#'  \mjdeqn{RI_{l_2,p} = RI_p(t_1, t_1 - l_2 + 1)}{RI_l2p = RI_p(t1, t1 - l2 + 1)}
#' where \mjseqn{l_1} and \mjseqn{l_2} refer respectively to the longer and shorter period on which to
#'  compute a reporting index, \mjseqn{t_1} is the year of end of the periods and
#'  \mjseqn{RI_p(t_1,t_0)} is the reporting index of the pattern \mjseqn{p} given by:
#'  \mjdeqn{RI_p(t_1,t_0) = \frac{\sum_{t = t_0}^{t_1} F_{p,t}}{\sum_{q \in P} \sum_{t = t_0}^{t_1} F_{q,t}}}{RI_p(t_1,t_0) = sum F_pt from t = t_0 to t_1 / sum F_qt for q in P and from t = t_0 to t_1}
#' where \mjseqn{P} is the set of patterns, \mjeqn{F_{p,t}}{F_pt} is the frequency of the pattern \mjseqn{p}
#'  in the transactions of the year \mjseqn{t}, and \mjseqn{t_0} and \mjseqn{t_1} are the first and last years
#'  defining the period on which to compute the reporting index.
#' 
#' For each one of the two sets of reporting indexes:
#' * One threshold \mjseqn{\xi} is computed as follows:
#'   \mjdeqn{\xi = \left\lceil \frac{1}{\sum_{p \in P} RI_p(t_1,t_0)^2} \right\rceil}{xi = ceiling(1 / sum(RI_p(t_1,t_0)^2) for p in P)}
#' * The patterns are ordered in descending order of their reporting index value.
#' * The reporting index of the \mjseqn{\xi}\out{<sup>th</sup>} pattern is selected as the \mjseqn{RI}
#'   threshold to be used to classify the patterns.
#' 
#' The patterns are then classified as follows:
#' * Patterns for which \mjeqn{RI_{l_1,p}}{RI_l1p} and \mjeqn{RI_{l_2,p}}{RI_l2p} are greater than or
#'   equal to the \mjseqn{RI} thresholds are classified as "Persistent".
#' * Patterns for which \mjeqn{RI_{l_1,p}}{RI_l1p} is greater than or equal to the related threshold
#'   and \mjeqn{RI_{l_2,p}}{RI_l2p} is lower than the related threshold are classified as "Declining".
#' * Patterns for which \mjeqn{RI_{l_1,p}}{RI_l1p} is lower than the related threshold and
#'   \mjeqn{RI_{l_2,p}}{RI_l2p} is greater than or equal to the related threshold are classified as "Emergent".
#' * Patterns for which \mjeqn{RI_{l_1,p}}{RI_l1p} and \mjeqn{RI_{l_2,p}}{RI_l2p} are lower than the
#'   thresholds are classified as "Latent".
#' 
#' @md
