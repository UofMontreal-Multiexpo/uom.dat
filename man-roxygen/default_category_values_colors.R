
#' @details
#' 
#' Default colors associated with the values of each category represented are selected circularly
#'  among the 20 colors of the palette `category20` from D3 (see `ggsci::pal_d3("category20")`).
#' Therefore, if the number of values exceeds `20`, some colors will be used more than once.
#'  For example, the \out{22<sup>nd</sup>} value will share the color of the \out{2<sup>nd</sup>} one.
#' See attribute `categories_colors` of the `SpectralAnalyzer` object to reassign colors to the
#'  category values.
#' 
#' @md
