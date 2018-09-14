#' Calculates the bending moment on a tree resulting from the force applied by the wind.
#' @title Bending moment on a tree under wind loading
#' @param dbh The diameter (cm) of the stem measured at breast height.
#' @param ht The height (m) of the tree.
#' @param cr_width The width (m) of the tree crown.
#' @param cr_depth The depth (m) of the tree crown.
#' @param spacing The spacing (m) between trees.
#' @param dist_edge The distance (m) of the the tree from the upwind edge.
#' @param gap_size The length (m) of the upwind gap.
#' @param uguess The speed (m s-1) of the wind.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @return The bending moment (N) exerted on a tree by the applied wind force.
#' @example bending_moment_rou(25, 20, 6.5, 12, 2.2, 0, 0, 15.6, 0.51, 2.35, 25)
bending_moment_rou <- function(dbh, ht, cr_width, cr_depth, spacing, dist_edge,
                               gap_size, uguess, n_drag, c_drag, drag_upper_limit, ro) {
  bm_rou <- zpd_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht) * ro * edge_gap_gust_factor_fun(spacing, ht, dist_edge, gap_size) *
    ((spacing * uguess * fgr_constants$k) / (log((ht - zpd_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht)) /
                                     z0_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht))))^2
  return(bm_rou)
}
