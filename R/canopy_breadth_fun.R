#' Calculates the breadth of the canopy under wind load.
#' @title Canopy Breadth Function.
#' @param cr_width The width of the canopy in windless conditions (m).
#' @param uguess Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @return The breadth of the crown under wind loading (m).
canopy_breadth_fun <- function(cr_width, uguess, n_drag, c_drag, drag_upper_limit) {
  canopy_breadth <- cr_width * drag_fun(uguess, n_drag, c_drag, drag_upper_limit)/2
  return(canopy_breadth)
}
