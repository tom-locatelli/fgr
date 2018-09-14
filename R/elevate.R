#' Converts the critical wind speeds to the wind speed 10 meters above zero plane displacement (for anemometers' data). The wind is assumed to follow a logarithmic profile.
#' @title Elevate
#' @param uh The critical wind speed (m s-1) calculated with the roughness or single-tree method.
#' @param z0 The roughness (m) of the canopy.
#' @param d The zero plane displacement height (m).
#' @param ht The height of the tree.
#' @return A critical wind speed tht can be compared with met data.
elevate <-function(uh, z0, d, ht) {
  u10 <- uh * log( 10/ z0) / log( (ht - d) / z0 )
  return(u10)
}
