#' Calculates the critical wind speed for breakage using the roughness method.
#' @title Critical wind speed for breakage - Roughness method.
#' @param mean_ht The height (m) of the mean tree in the stand.
#' @param mean_dbh The dbh (cm) of the mean tree in the stand.
#' @param spacing The mean spacing (m) between trees in the stand.
#' @param dist_edge Distance (m) of the mean tree from the upwind edge.
#' @param gap_size Length (m) of the upwind gap.
#' @param mean_cr_width Width (m) of the crown of the mean tree in the stand.
#' @param mean_cr_depth Depth (m) of the crown of the mean tree in the stand.
#' @param moe Modulus of Elasticity (MPa) of green wood.
#' @param mor Modulus of Rupture (MPa) of green wood.
#' @param fknot Knot factor. Dimensionless.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}.
#' @param stem_vol Volume (m3) of the stem of the mean tree in the stand.
#' @param stem_density Density (kg m-3) of green wood of the stem.
#' @param crown_density Density (kg m-3) of of the crown of the mean tree in the stand.
#' @param snow_depth Depth (cm) of layer of snow on tree crown.
#' @param snow_density Density (kg m-3) of snow.
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
    zpd <- zpd_fun(mean_cr_width, mean_cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit, mean_ht)
    gammasolved <- gammasolved_fun(mean_cr_width, mean_cr_depth, spacing, uguess, n_drag, c_drag, drag_upper_limit)

    uh_b <- (1/spacing) * sqrt(breaking_moment /  (ro * edge_gap_gust_factor * zpd * dlf_calc)) * gammasolved

    uguess <- uh_b}

  uh_b_results <- list(uh_b, bm_rou, zpd, gammasolved, dlf_calc, breaking_moment, edge_gap_gust_factor)
  return(uh_b_results)
}
