#' Calculates the critical wind speed for overturning using the TMC method.
#' @title Critical wind speed for overturning - Single-tree method.
#' @param tree_ht Individual tee height (m).
#' @param dbh Diameter of the stem at breast height, i.e. 1.3m above the ground (cm).
#' @param cr_depth Length of the tree crown (m).
#' @param cr_width Width of the tree crown (m).
#' @param spacing_current Current mean spacing of trees in the stand (m).
#' @param spacing_before Mean spacing of trees in the stand before any thinning (m).
#' @param years_since_thin Number of years since most recent thinning.
#' @param dist_edge Distance of tree from the upwind edge (m).
#' @param gap_size Length of the upwind gap (m).
#' @param stand_mean_ht Arithmetic mean height of the trees in the stand (m).
#' @param moe Modulus of Elasticity of green wood (MPa).
#' @param c_reg Regression coefficient of uprooting moment against stem weight (N m kg-1).
#' @param stem_vol Individual tree stem volume (m3).
#' @param stem_density Density of green wood of the stem (kg m-3).
#' @param crown_density Density of of the tree crown (kg m-3).
#' @param snow_depth Depth of layer of snow on tree crown (cm).
#' @param snow_density Density of snow (kg m-3).
#' @param ci Competition Index (\code{bal}, \code{heg}, \code{none}) used.
#' @param ci_value Value of \code{ci}.
#' @param c_reg Regression coefficients of uprooting moment against stem weight (N m kg-1).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @param equivalent_mean_ht Equivalent mean stand height: the level in the stand responsible for most of the momentum absorption (m).
#' @param stand_cr_depth Length of the crown of the "equivalent mean tree" in the stand (m).
#' @param stand_cr_width Width of the crown of the "equivalent mean tree" in the stand (m).
#' @return A list including: The critical wind speed (m s-1) for overturning at canopy top height for the single-tree method;
#' The turning moment coefficient; The turning moment ratio; The deflection loading factor; The critical overturning moment;
#' The combined effect of edge and gap on the applied bending moment.
#' @name TMC_Overturning_Critical_wind_speed_functions
NULL

#' @rdname TMC_Overturning_Critical_wind_speed_functions
uh_overturning_tmc <- function(tree_ht, dbh, cr_depth, cr_width, spacing_current, spacing_before, years_since_thin, dist_edge, gap_size,
                               moe, c_reg, stem_vol, stem_density, crown_density, snow_depth, snow_density, ci, ci_value,
                               n_drag, c_drag, drag_upper_limit, equivalent_mean_ht, stand_cr_depth, stand_cr_width) {

  overturning_moment <- critical_moment_overturning(c_reg, stem_density, stem_vol)
  edge_gap_factor <- edge_gap_factor_fun(spacing_current, equivalent_mean_ht, dist_edge, gap_size)
  tmc <- tc_zero_intercept_fun(dbh, tree_ht, ci, ci_value)

  uguess <- sqrt(overturning_moment / tmc) #initial guess wind speed to initiate the iteration
  uguess1 <- uguess
  uh_o <-  uguess / 2
  while (abs(uguess1 - uh_o) > fgr_constants$wind_precision) {
    uguess1 <- uguess
    bm_tmc <- tmc * uguess^2
    dlf_calc <- dlf_fun(bm_tmc, tree_ht, cr_depth, cr_width, stem_vol, dbh, moe, crown_density, stem_density, snow_depth, snow_density)
    dlf_used <- ifelse(dlf_calc < 1, fgr_constants$dlf, ifelse(dlf_calc > 2, fgr_constants$dlf, dlf_calc))
    tmr_full <- tm_ratio(spacing_before, spacing_current, years_since_thin, stand_cr_width, stand_cr_depth, uguess, n_drag, c_drag, drag_upper_limit, equivalent_mean_ht)
    uh_o <- sqrt(overturning_moment / (tmc*dlf_used*tmr_full*edge_gap_factor))
    uguess <- uh_o
  }
  uh_o_results <- list(uh_o, tmc, tmr_full, dlf_calc, overturning_moment, edge_gap_factor, dlf_used)
  return(uh_o_results)
}

#Using tmr_simple####
#requires activating uh_overturning_tmc_tmr_simple function in fg_tmc.R script
#Also note that to output tmr_simple, the relevant line in the "#Extracting values" section in fg_tmc.R script needs activated
#' @rdname TMC_Overturning_Critical_wind_speed_functions
uh_overturning_tmc_tmr_simple <- function(tree_ht, dbh, cr_depth, cr_width, spacing_current, spacing_before, years_since_thin, dist_edge, gap_size,
                                          equivalent_mean_ht, moe, c_reg, stem_vol, stem_density, crown_density, snow_depth, snow_density, ci, ci_value) {

  overturning_moment <- critical_moment_overturning(c_reg, stem_density, stem_vol)
  edge_gap_factor <- edge_gap_factor_fun(spacing_current, equivalent_mean_ht, dist_edge, gap_size)
  tmc <- tc_zero_intercept_fun(dbh, tree_ht, ci, ci_value)
  tmr_simple <- tm_ratio_simple(spacing_before, spacing_current, years_since_thin)

  uguess <- 25 #initial guess wind speed to initiate the iteration
  uguess1 <- uguess
  uh_o <-  uguess / 2
  while (abs(uguess1 - uh_o) > fgr_constants$wind_precision) {
    uguess1 <- uguess
    bm_tmc <- tmc * uguess^2
    dlf_calc <- dlf_fun(bm_tmc, tree_ht, cr_depth, cr_width, stem_vol, dbh, moe, crown_density, stem_density, snow_depth, snow_density)
    dlf_used <- ifelse(dlf_calc < 1, fgr_constants$dlf, ifelse(dlf_calc > 2, fgr_constants$dlf, dlf_calc))
    uh_o <- sqrt(overturning_moment / (tmc*dlf_used*tmr_simple*edge_gap_factor))
    uguess <- uh_o
  }
  uh_o_results <- list(uh_o, tmc, tmr_simple, dlf_calc, overturning_moment, edge_gap_factor, dlf_used)
  return(uh_o_results)
}
