#' Calculates the ratio between the critical wind speed at canopy top and the friction velocity (uh / u*).
#' @title Gamma Solved Function.
#' @param cr_width Width of the canopy in windless conditions (m).
#' @param cr_depth Length of the canopy in windless conditions (m).
#' @param spacing Mean distance between trees (m).
#' @param uguess Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @return \code{gammasolved}, the ratio of critical wind speed at canopy top over friction velocity (uh / u*).
gammasolved_fun <- function(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit) {
  gammasolved <- ifelse(lambdacapital_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit) > 0.6, 1/sqrt(fgr_constants$cs + fgr_constants$cr*0.3),
                        1/(fgr_constants$cs + fgr_constants$cr * lambdacapital_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit)/2)^0.5) #corrected by SH Sep 2017
  return(gammasolved)
}
