#' Combined effect of gap size and edge effect on maximum bending moment for the single-tree method.
#' @title Edge Gap Factor.
#' @param spacing Mean distance between trees (m).
#' @param equivalent_mean_ht Equivalent mean stand height: the level in the stand responsible for most of the momentum absorption (m).
#' @param dist_edge Distance of tree from the upwind edge (m).
#' @param gap_size Length of the upwind gap (m).
#' @return \code{edge_gap_factor}, the combined effect of edge and gap on the maximum bending moment exerted by the wind on a tree. Used in the tmc method.
edge_gap_factor_fun <- function(spacing, equivalent_mean_ht, dist_edge, gap_size) {
  s_h <- spacing / equivalent_mean_ht
  if (s_h < 0.075) {s_h <- 0.075}
  if (s_h > 0.45) {s_h <- 0.45}
  if (dist_edge > equivalent_mean_ht*fgr_constants$tree_heights_inside_forest) {dist_edge <- equivalent_mean_ht*fgr_constants$tree_heights_inside_forest}
  edge_gap_factor <- ((2.7193 * s_h -0.061) + (-1.273 * s_h + 0.9701) * ((1.1127 * s_h + 0.0311)^(fgr_constants$tree_heights_inside_forest*equivalent_mean_ht/equivalent_mean_ht)) +
                        ((-1.273 * s_h + 0.9701) *(((1.1127 * s_h + 0.0311)^(dist_edge/equivalent_mean_ht)) - ((1.1127 * s_h + 0.0311)^(fgr_constants$tree_heights_inside_forest*equivalent_mean_ht/equivalent_mean_ht)))) *
                        max_gap_factor_fun(gap_size, equivalent_mean_ht)) /
    ((2.7193 * s_h -0.061) + (-1.273 * s_h + 0.9701) * ((1.1127 * s_h + 0.0311)^(fgr_constants$tree_heights_inside_forest*equivalent_mean_ht/equivalent_mean_ht)))
  return(edge_gap_factor)
}
