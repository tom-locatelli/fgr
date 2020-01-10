#' Converts the critical wind speeds to the wind speed 10 meters above zero plane displacement (for anemometers' data). The wind is assumed to follow a logarithmic profile.
#' @title Elevate
#' @param uh Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param z0 Aerodynamic roughness length of the canopy (m).
#' @param d Zero plane displacement height of the canopy (m).
#' @param ht Height at which the critical wind speed is calculated (m). Note that this differs between the 'roughness' (stand mean height) and 'TMC' (1.05 * stand top height) methods.
#' @return A critical wind speed that can be compared with met data.
elevate <-function(uh, z0, d, ht) {
  u10 <- uh * log( 10/ z0) / log( (ht - d) / z0 )
  return(u10)
}
