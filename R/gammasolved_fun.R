#' Calculates the ratio between the critical wind speed at canopy top and the friction velocity (uh / u*).
#' @title Gamma Solved Function.
#' @param cr_width The width (m) of the canopy in windless conditions.
#' @param cr_depth The length (m) of the canopy in windless conditions.
#' @param spacing The spacing (m) between trees.
#' @param uguess The speed (m s-1) of the wind.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @return \code{gammasolved}, the ratio of critical wind speed at canopy top over friction velocity (uh / u*).
gammasolved_fun <- function(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit) {
  gammasolved <- ifelse(lambdacapital_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit) > 0.6, 1/sqrt(fgr_constants$cs + fgr_constants$cr*0.3),
                        1/(fgr_constants$cs + fgr_constants$cr * lambdacapital_fun(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit)/2)^0.5) #corrected by SH Sep 2017
  return(gammasolved)
}
