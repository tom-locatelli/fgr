#' Calculates the frontal area of the streamlined canopy per ground area.
#' @title Lambda Capital Function.
#' @param cr_width Width of the canopy in windless conditions (m).
#' @param cr_depth Length of the canopy in windless conditions (m).
#' @param spacing Mean distance between trees (m).
#' @param uguess Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @return \code{lambdacapital}, the frontal area of the streamlined crown under per ground area.
lambdacapital_fun <- function(cr_width, cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit) {
  lambdacapital <- 2 * (canopy_breadth_fun(cr_width, uguess, n_drag, c_drag, drag_upper_limit) * cr_depth / spacing^2)
  return(lambdacapital)
}

#The frontal area of the streamlined canopy per ground area (defined by spacing). Effectively, LambdaCapital is drag per unit ground area
