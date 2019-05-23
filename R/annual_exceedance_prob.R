#' Calculates the annual probability of exceeding a critical wind speed.
#' @title Annual Exceedance Probability
#' @param u10 Critical wind speed at standard anemometer height, i.e. 10m above zero-plane displacement height (m s-1).
#' @param weib_a Scale parameter of the Weibull distribution for local wind speeds.
#' @param weib_k Shape parameter of the Weibull distribution for local wind speeds.
#' @return \code{aep}, i.e. the probability of exceeding the critical wind speed of damage.
#' @example annual_exceedance_prob(22.3, 8.6, 1.9)
annual_exceedance_prob <- function(u10, weib_a, weib_k) {
  u_c <- fgr_constants$u_c1 * (weib_k)^3 + fgr_constants$u_c2 * (weib_k)^2 + fgr_constants$u_c3 * weib_k + fgr_constants$u_c4
  u <- (u_c * weib_a)^2
  aaa <- u / fgr_constants$u_a
  aep <- 1 - exp(-1 * exp(-1 * (u10^2 - u) / aaa))
  # In the formula for aep, the part from "exp" onwards is a Fisher-Tippett
  # Type I distribution, We get to this because the extremes of a dataset
  # following a Weibull distribution converge towards a FT1 distribution
  # (and obviously in this Function we are concerned with extremes!)
  return(aep)
}
