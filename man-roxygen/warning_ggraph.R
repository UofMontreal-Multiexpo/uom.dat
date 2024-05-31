
#' @section Warning:
#' 
#' Several versions of the package `ggraph` experience some issues about
#'  hierarchical edge bundling charts.
#' Such charts are created by the functions `co_occurrence_chart` and
#'  `rules_chart`.
#' 
#' The main issue encountered is that wrong colors are assigned to edges.
#' Note that such an issue does not throw any error or warning messages.
#' One way to spot it is to notice light lines above dark lines.
#' 
#' Note also that some of the newer versions of `ggraph` have the same issues.
#' A simple way to know if the installed version is concerned is to run the
#'  first instruction in section "4.2. Co-occurrence charts" of the vignette
#'  titled "Transaction analyzes" and compare the resulting graph with the one
#'  shown in the document.
#' 
#' The correct operation of these functions has been validated with the version
#' 2.0.5 of `ggraph`. To install it, run the following instruction.
#' \preformatted{
#' remotes::install_version("ggraph", version = "2.0.5")
#' }
#' 
#' @md
