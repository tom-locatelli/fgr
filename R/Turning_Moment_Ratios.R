#' Calculate the ratio between the turning moment coefficient before and after thinning in the stand.
#' @param spacing_before The mean spacing (m) between trees in the stand before any thinning.
#' @param spacing_current The current mean spacing (m) between trees in the stand.
#' @param years_since_thin The number of years after the latest thinning.
#' @param cr_width The width (m) of the tree crown.
#' @param cr_depth The length (m) of the tree crown.
#' @param uguess The speed (m s-1) of the wind.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @param ht The height (m) of the tree.
#' @param ci Competition Index (\code{BAL}, \code{Hegyi}) used. Can be \code{None}.
#' @name Turning_Moment_Ratios
#' @title Turning Moment Ratio Functions
NULL

#' @rdname Turning_Moment_Ratios
tm_ratio_simple <- function(spacing_before, spacing_current, years_since_thin) { #1. This one is to be used until evidence of the physics of #3 and #4 are tested. While this derived relationship is based on data generated from #3 and #4, it is quicker to compute as it requires no uguess
  if(years_since_thin > 5) {
    tmr_simple <- 1
  } else {
    tmr_simple <- (0.99 * spacing_current/spacing_before) * (1 - years_since_thin/5) + (years_since_thin/5)
  }
  return(tmr_simple)
}

#' @rdname Turning_Moment_Ratios
tm_ratio_simple_ci <- function(spacing_before, spacing_current, years_since_thin, ci) { #2
  if(years_since_thin > 5) (
    tmrci_simple <- 1
  ) else if (ci == "None") {
    tmrci_simple <- (0.99 * spacing_current/spacing_before) * (1 - years_since_thin/5) + (years_since_thin/5)
  } else { tmrci_simple <- 1}
  return(tmrci_simple)
}

#' @rdname Turning_Moment_Ratios
tm_ratio <- function(spacing_before, spacing_current, years_since_thin, cr_width, cr_depth, uguess, n_drag, c_drag, drag_upper_limit, ht) {#3
  if(years_since_thin > 5) {
    tmr_full <- 1
  } else {
    d_before <- zpd_fun(cr_width, cr_depth, spacing_before, uguess, n_drag, c_drag, drag_upper_limit, ht)
    d_current <- zpd_fun(cr_width, cr_depth, spacing_current, uguess, n_drag, c_drag, drag_upper_limit, ht)
    z0_before <- z0_fun(cr_width, cr_depth, spacing_before, uguess, n_drag, c_drag, drag_upper_limit, ht)
    z0_current <- z0_fun(cr_width, cr_depth, spacing_current, uguess, n_drag, c_drag, drag_upper_limit, ht)
    tmr_full <- (d_current/d_before) * ((spacing_current/spacing_before)^2) * ((log((ht - d_before)/z0_before) / log((ht - d_current)/z0_current))^2) *
      (1 - years_since_thin/5) + (years_since_thin/5)
  }
  return(tmr_full)
}

#' @rdname Turning_Moment_Ratios
tm_ratio_ci <- function(spacing_before, spacing_current, years_since_thin, cr_width, cr_depth, uguess, n_drag, c_drag, drag_upper_limit, ht, ci) { #4
  if(years_since_thin > 5) {
    tmrci_full <- 1
  } else if (ci == "None") {
    d_before <- zpd_fun(cr_width, cr_depth, spacing_before, uguess, n_drag, c_drag, drag_upper_limit, ht)
    d_current <- zpd_fun(cr_width, cr_depth, spacing_current, uguess, n_drag, c_drag, drag_upper_limit, ht)
    z0_before <- z0_fun(cr_width, cr_depth, spacing_before, uguess, n_drag, c_drag, drag_upper_limit, ht)
    z0_current <- z0_fun(cr_width, cr_depth, spacing_current, uguess, n_drag, c_drag, drag_upper_limit, ht)
    tmrci_full <- (d_current/d_before) * ((spacing_current/spacing_before)^2) * ((log((ht - d_before)/z0_before) / log((ht - d_current)/z0_current))^2) *
      (1 - years_since_thin/5) + (years_since_thin/5)
  } else {tmrci_full <- 1}
  return(tmrci_full)
}
