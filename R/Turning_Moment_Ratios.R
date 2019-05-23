#' Calculate the ratio between the turning moment coefficient before and after thinning in the stand.
#' @param spacing_before Mean spacing of trees in the stand before any thinning (m).
#' @param spacing_current Current mean spacing of trees in the stand (m).
#' @param years_since_thin Number of years after the latest thinning.
#' @param cr_width Width of the crown of the "equivalent mean tree" in the stand (m).
#' @param cr_depth Length of the crown of the "equivalent mean tree" in the stand (m).
#' @param uguess Critical wind speed at canopy top calculated with the roughness or single-tree method (m s-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @param ht Equivalent mean stand height: the level in the stand responsible for most of the momentum absorption (m).
#' @param ci Competition Index (\code{bal}, \code{heg}, \code{none}) used.
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
  ) else if (ci == "none") {
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
  } else if (ci == "none") {
    d_before <- zpd_fun(cr_width, cr_depth, spacing_before, uguess, n_drag, c_drag, drag_upper_limit, ht)
    d_current <- zpd_fun(cr_width, cr_depth, spacing_current, uguess, n_drag, c_drag, drag_upper_limit, ht)
    z0_before <- z0_fun(cr_width, cr_depth, spacing_before, uguess, n_drag, c_drag, drag_upper_limit, ht)
    z0_current <- z0_fun(cr_width, cr_depth, spacing_current, uguess, n_drag, c_drag, drag_upper_limit, ht)
    tmrci_full <- (d_current/d_before) * ((spacing_current/spacing_before)^2) * ((log((ht - d_before)/z0_before) / log((ht - d_current)/z0_current))^2) *
      (1 - years_since_thin/5) + (years_since_thin/5)
  } else {tmrci_full <- 1}
  return(tmrci_full)
}
