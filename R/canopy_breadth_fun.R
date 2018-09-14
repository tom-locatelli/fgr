#' Calculates the breadth of the canopy under wind load.
#' @title Canopy Breadth Function.
#' @param cr_width The width (m) of the canopy in windless conditions.
#' @param uguess The speed (m s-1) of the wind.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @return The breadth of the crown under wind loading.
canopy_breadth_fun <- function(cr_width, uguess, n_drag, c_drag, drag_upper_limit) {
  canopy_breadth <- cr_width * drag_fun(uguess, n_drag, c_drag, drag_upper_limit)/2
  return(canopy_breadth)
}
