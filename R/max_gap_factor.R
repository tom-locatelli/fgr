#' Effect of gap size on maximum bending moment
#' @title Maximum Gap Factor
#' @param gap_size The length (m) of the upwind gap.
#' @param ht The height of the tree.
#' @return \code{max_gap_factor}, the effect of gap alone on the maximum bending moment exerted by the wind on a tree.
max_gap_factor_fun <- function(gap_size, ht) { #Common to Roughness and TMC Methods
  g_h <- ifelse(gap_size/ht > 10, 10, gap_size/ht)
  max_gap_factor <- (g_h/10)^0.3467
  return(max_gap_factor)
}
