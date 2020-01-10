#' Calculates the height of the zero plane displacement.
#' @title Zero Plane Displacement Function.
#' @param cr_width Width of the canopy in windless conditions (m).
#' @param cr_depth Length of the canopy in windless conditions (m).
#' @param spacing Mean distance between trees (m).
#' @param uguess Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @param ht Height of the tree. In the 'roughness' method, this is stand mean height. In the TMC method, this is 'equivalent mean height' (m).
#' @return \code{zpd}, the height of the zero plane displacement (m).
zpd_fun <- function(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, ht, fgr_constants) {
  zpd <- (1 - ((1 - exp( - sqrt(fgr_constants$cd1 * lambdacapital_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit)))) /
                 sqrt(fgr_constants$cd1 * lambdacapital_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit)))) * ht
  return(zpd)
}
#Calculation of Zero-plane Displacement (d) and Aerodynamic Roughness (z0) from "Simplified Expressions for vegetation roughness and
#zero-plane displacement as functions of canopy height and area index" by M.R. Raupach (1994)
