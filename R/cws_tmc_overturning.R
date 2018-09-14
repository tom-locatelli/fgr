#' Calculates the critical wind speed for overturning using the TMC method.
#' @title Critical wind speed for overturning - Single-tree method.
#' @param tree_ht The height (m) of a tree.
#' @param dbh The dbh (cm) of a tree.
#' @param cr_depth The length (m) of the tree crown.
#' @param cr_width The width (m) of the tree crown.
#' @param spacing_current The current mean spacing (m) between trees in the stand.
#' @param spacing_before The mean spacing (m) between trees in the stand before any thinning.
#' @param years_since_thin The number of years after the latest thinning.
#' @param dist_edge Distance (m) of the mean tree from the upwind edge.
#' @param gap_size Length (m) of the upwind gap.
#' @param stand_mean_ht Height (m) of the mean tree in the stand.
#' @param moe Modulus of Elasticity (MPa) of green wood.
#' @param c_reg Regression coefficients (N m kg-1) of uprooting moment against stem weight.
#' @param stem_vol Volume (m3) of the stem of the tree.
#' @param stem_density Density (kg m-3) of green wood of the stem.
#' @param crown_density Density (kg m-3) of of the crown of the tree.
#' @param snow_depth Depth (cm) of layer of snow on tree crown.
#' @param snow_density Density (kg m-3) of snow.
#' @param ci Competition Index (\code{BAL}, \code{Hegyi}) used. Can be \code{None}.
#' @param ci_value Value of \code{ci}.
#' @return A list including: The critical wind speed (m s-1) for overturning at canopy top height for the single-tree method;
#' The turning moment coefficient; The turning moment ratio; The deflection loading factor; The critical overturning moment;
#' The combined effect of edge and gap on the applied bending moment.
uh_overturning_tmc <- function(tree_ht, dbh, cr_depth, cr_width, spacing_current, spacing_before, years_since_thin, dist_edge, gap_size,
                               stand_mean_ht, moe, c_reg, stem_vol, stem_density, crown_density, snow_depth, snow_density, ci, ci_value) {

  overturning_moment <- critical_moment_overturning(c_reg, stem_density, stem_vol)
  edge_gap_factor <- edge_gap_factor_fun(spacing_current, stand_mean_ht, dist_edge, gap_size)
  tmc <- tc_zero_intercept_fun(dbh, tree_ht, ci, ci_value)
  tmr_simple <- tm_ratio_simple(spacing_before, spacing_current, years_since_thin)

  uguess <- 25 #initial guess wind speed to initiate the iteration
  uguess1 <- uguess
  uh_o <-  uguess / 2
  while (abs(uguess1 - uh_o) > fgr_constants$wind_precision) {
    uguess1 <- uguess
    bm_tmc <- tmc * uguess^2
    dlf_calc <- dlf_fun(bm_tmc, tree_ht, cr_depth, cr_width, stem_vol, dbh, moe, crown_density, stem_density, snow_depth, snow_density)
    uh_o <- sqrt(overturning_moment / (tmc*dlf_calc*tmr_simple*edge_gap_factor))
    uguess <- uh_o
  }
  uh_o_results <- list(uh_o, tmc, tmr_simple, dlf_calc, overturning_moment, edge_gap_factor)
  return(uh_o_results)
}
