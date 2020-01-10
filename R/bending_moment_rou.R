#' Calculates the bending moment on a tree resulting from the load applied by the wind.
#' @title Bending moment on a tree under wind loading
#' @param dbh Arithmetic average of the diameter of the stem at breast height, i.e. 1.3m above the ground, for all trees in the stand (cm).
#' @param ht Mean tree height in the stand (m).
#' @param cr_width Width of the tree crown (m).
#' @param cr_depth Depth of the tree crown (m).
#' @param spacing Mean distance between trees (m).
#' @param dist_edge Distance of the the tree from the upwind edge (m).
#' @param gap_size Length of the upwind gap (m).
#' @param uguess Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @return Applied bending moment resulting from the wind load (N).
#' @example bending_moment_rou(25, 20, 6.5, 12, 2.2, 0, 0, 15.6, 0.51, 2.35, 25)
bending_moment_rou <- function(dbh, ht, cr_width, cr_depth, spacing, dist_edge,
                               gap_size, uguess, n_drag, c_drag, drag_upper_limit, ro, fgr_constants) {
  bm_rou <- zpd_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht, fgr_constants) * ro * edge_gap_gust_factor_fun(spacing, ht, dist_edge, gap_size, fgr_constants) *
    ((spacing * uguess * fgr_constants$k) / (log((ht - zpd_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht, fgr_constants)) /
                                     z0_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht, fgr_constants))))^2
  return(bm_rou)
}
