#' Combined effect of gap size and edge effect on maximum bending moment for the single-tree method.
#' @title Edge Gap Factor.
#' @param spacing The distance (m) between trees.
#' @param ht The height of the tree.
#' @param dist_edge The distance (m) of the tree from the upwind edge.
#' @param gap_size The length (m) of the upwind gap.
#' @return \code{edge_gap_factor}, the combined effect of edge and gap on the maximum bending moment exerted by the wind on a tree. Used in the tmc method.
edge_gap_factor_fun <- function(spacing, ht, dist_edge, gap_size) {
  s_h <- spacing / ht
  if (s_h < 0.075) {s_h <- 0.075}
  if (s_h > 0.45) {s_h <- 0.45}
  if (dist_edge > ht*fgr_constants$tree_heights_inside_forest) {dist_edge <- ht*fgr_constants$tree_heights_inside_forest}
  edge_gap_factor <- ((2.7193 * s_h -0.061) + (-1.273 * s_h + 0.9701) * ((1.1127 * s_h + 0.0311)^(fgr_constants$tree_heights_inside_forest*ht/ht)) +
                        ((-1.273 * s_h + 0.9701) *(((1.1127 * s_h + 0.0311)^(dist_edge/ht)) - ((1.1127 * s_h + 0.0311)^(fgr_constants$tree_heights_inside_forest*ht/ht)))) *
                        max_gap_factor_fun(gap_size, ht)) /
    ((2.7193 * s_h -0.061) + (-1.273 * s_h + 0.9701) * ((1.1127 * s_h + 0.0311)^(fgr_constants$tree_heights_inside_forest*ht/ht)))
  return(edge_gap_factor)
}
