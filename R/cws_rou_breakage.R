#' Calculates the critical wind speed for breakage using the roughness method.
#' @title Critical wind speed for breakage - Roughness method.
#' @param mean_ht Arithmetic mean height of the trees in the stand (m).
#' @param mean_dbh Mean dbh of all trees in the stand (cm). Dbh is diameter at breast height, measured at 1.3m above the ground. Essential.
#' @param spacing Mean distance between trees (m).
#' @param dist_edge Distance from the upwind edge (m).
#' @param gap_size Length of the upwind gap (m).
#' @param mean_cr_width Width of the crown of the mean tree in the stand (m).
#' @param mean_cr_depth Depth of the crown of the mean tree in the stand (m).
#' @param moe Modulus of Elasticity of green wood (MPa).
#' @param mor Modulus of Rupture of green wood (MPa).
#' @param fknot Knot factor (dimensionless).
#' @param n_drag N parameter of the drag coefficient formula (dimensionless).
#' @param c_drag C parameter of the drag coefficient formula (dimensionless).
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1).
#' @param stem_vol Stem volume of the mean tree in the stand (m^3).
#' @param stem_density Density of green wood of the stem (kg m-3).
#' @param crown_density Crown density of the mean tree in the stand (kg m-3).
#' @param snow_depth Depth of layer of snow on tree crown (cm).
#' @param snow_density Density of snow (kg m-3).
#' @param ro Air density (kg m-3).
#' @return A list including: The critical wind speed (m s-1) for breakage at zero plane displacement height for the roughness method;
#' The applied bending moment; The zero plane displacement height; The ratio of critical wind speed at canopy top over friction velocity
#' (uh / u*); The deflection loading factor; The critical breaking moment; The combined effect of edge, gap, and gust on the applied
#' bending moment.
uh_breakage_rou <- function(mean_ht, mean_dbh, spacing, dist_edge, gap_size, mean_cr_width, mean_cr_depth, moe, mor, fknot, n_drag, c_drag, drag_upper_limit,
                            stem_vol, stem_density, crown_density, snow_depth, snow_density, ro) {

  edge_gap_gust_factor <- edge_gap_gust_factor_fun(spacing, mean_ht, dist_edge, gap_size)
  breaking_moment <- critical_moment_breakage(mean_dbh, mean_ht, mean_cr_depth, mor, fknot)

  uguess <- 25 #initial guess wind speed to initiate the iteration
  uguess1 <- uguess
  uh_b <-  uguess / 2
  while (abs(uguess1 - uh_b) > fgr_constants$wind_precision) {
    uguess1 <- uguess

    bm_rou <- bending_moment_rou(mean_dbh, mean_ht, mean_cr_width, mean_cr_depth, spacing, dist_edge, gap_size, uguess, n_drag, c_drag, drag_upper_limit, ro)
    dlf_calc <- dlf_fun(bm_rou, mean_ht, mean_cr_depth, mean_cr_width, stem_vol, mean_dbh, moe, crown_density, stem_density, snow_depth, snow_density)
    dlf_used <- ifelse(dlf_calc < 0, fgr_constants$dlf, ifelse(dlf_calc > 2.5, fgr_constants$dlf, dlf_calc))
    zpd <- zpd_fun(mean_cr_width, mean_cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, mean_ht)
    gammasolved <- gammasolved_fun(mean_cr_width, mean_cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit)

    uh_b <- (1/spacing) * sqrt(breaking_moment /  (ro * edge_gap_gust_factor * zpd * dlf_used)) * gammasolved

    uguess <- uh_b}

  uh_b_results <- list(uh_b, bm_rou, zpd, gammasolved, dlf_calc, breaking_moment, edge_gap_gust_factor, dlf_used)
  return(uh_b_results)
}
