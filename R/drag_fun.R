#' Calculates the drag of the canopy under wind load.
#' @title Crown Drag.
#' @param uguess The speed (m s-1) of the wind.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @return The porosity of the crown under wind loading.
drag_fun <- function(uguess, n_drag, c_drag, drag_upper_limit) {
  drag <- ifelse (uguess<10, c_drag*10^(-n_drag), ifelse(uguess>drag_upper_limit, c_drag*drag_upper_limit^(-n_drag), c_drag * uguess^(-n_drag)))
  return(drag)
}
