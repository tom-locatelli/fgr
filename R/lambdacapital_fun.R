#' Calculates the frontal area of the streamlined canopy per ground area.
#' @title Lambda Capital Function.
#' @param cr_width The width (m) of the canopy in windless conditions.
#' @param cr_depth The length (m) of the canopy in windless conditions.
#' @param spacing The spacing (m) between trees.
#' @param uguess The speed (m s-1) of the wind.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @return \code{lambdacapital}, the frontal area of the streamlined crown under per ground area.
lambdacapital_fun <- function(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit) {
  lambdacapital <- 2 * (canopy_breadth_fun(cr_width, uguess, n_drag, c_drag, drag_upper_limit) * cr_depth / spacing^2)
  return(lambdacapital)
}

#The frontal area of the streamlined canopy per ground area (defined by spacing). Effectively, LambdaCapital is drag per unit ground area
