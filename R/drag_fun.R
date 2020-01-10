#' Calculates the drag of the canopy under wind load.
#' @title Crown Drag.
#' @param uguess Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @return The porosity of the crown under wind loading.
drag_fun <- function(uguess, n_drag, c_drag, drag_upper_limit) {
  drag <- ifelse (uguess<10, c_drag*10^(-n_drag), ifelse(uguess>drag_upper_limit, c_drag*drag_upper_limit^(-n_drag), c_drag * uguess^(-n_drag)))
  return(drag)
}
