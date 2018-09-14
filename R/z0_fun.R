#' Calculates the length (m) of the canopy surface roughness.
#' @title Canopy Roughness Function.
#' @param cr_width The width (m) of the canopy in windless conditions.
#' @param cr_depth The length (m) of the canopy in windless conditions.
#' @param spacing The spacing (m) between trees.
#' @param uguess The speed (m s-1) of the wind.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @param ht The height (m) of the tree.
#' @return \code{z0}, the length (m) of the surface roughness of the tree canopy.
z0_fun <- function(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht) {
  z0 <- (ht - zpd_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht)) *
    exp((-fgr_constants$k * gammasolved_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit)) + (log(fgr_constants$cw) -1 + fgr_constants$cw^(-1)))
  return(z0)
}
#Calculation of Zero-plane Displacement (d) and Aerodynamic Roughness (z0) from "Simplified Expressions for vegetation roughness and
# zero-plane displacement as functions of canopy height and area index" by M.R. Raupach (1994)
