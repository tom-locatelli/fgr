#' Wrapper for the roughness method.
#' @title ForestGALES - Roughness method.
#' @param stand_id The stand identifier. Essential.
#' @param species The tree species under investigation. Essential.
#' @param mean_ht The mean tree height (m) in the stand. Essential (unless \code{top_ht} is provided).
#' @param mean_dbh The mean dbh (cm) of trees in the stand. Essential.
#' @param spacing The current mean spacing (m) between trees in the stand. Essential.
#' @param weib_a The scale parameter of the Weibull distribution of local wind speeds.
#' @param weib_k The shape parameter of the Weibull distribution of local wind speeds.
#' @param full_output Switch between full and minimal outputs.
#' @param top_ht The height (m) of the dominant tree(s) in the stand. Essential (unless \code{mean_ht} is provided).
#' @param mean_cr_width The width (m) of the crown of the mean tree in the stand.
#' @param mean_cr_depth The length (m) of the crown of the mean tree in the stand.
#' @param soil_group The soil type identifier.
#' @param rooting The rooting depth class (1 = Shallow, 2 = Deep, 3 = Medium).
#' @param new_edge Switch to toggle between green upwind edge (0) or brown upwind edge (1)
#' @param gap_size Length (m) of the upwind gap.
#' @param moe Modulus of Elasticity (MPa) of green wood. Advanced Input.
#' @param mor Modulus of Rupture (MPa) of green wood. Advanced Input.
#' @param fknot Knot factor. Dimensionless. Advanced Input.
#' @param stem_vol Volume (m3) of the stem of the tree. Advanced Input.
#' @param crown_vol Volume (m3) of the tree crown. Advanced Input.
#' @param stem_density Density (kg m-3) of green wood of the stem. Advanced Input.
#' @param crown_density Density (kg m-3) of of the crown of the tree. Advanced Input.
#' @param c_reg Regression coefficients (N m kg-1) of uprooting moment against stem weight. Advanced Input.
#' @param n_drag The N parameter of the drag coefficient formula. Dimensionless. Advanced Input.
#' @param c_drag The C parameter of the drag coefficient formula. Dimensionless. Advanced Input.
#' @param drag_upper_limit The experimental maximum wind speed (m*s-1) for calculations of the vaules of \code{n_drag} and \code{c_drag}. Advanced Input.
#' @param snow_depth Depth (cm) of layer of snow on tree crown. Advanced Input.
#' @param snow_density Density (kg m-3) of snow. Advanced Input.
#' @param ro Air density (kg m-3). Advanced Input.
#' @param x Spatial coordinate.
#' @param y Spatial coordinate.
#' @param z Spatial coordinate.
#' @param dams Value of Detailed Aspect Method of Scoring to describe the local wind climate (for UK conditions only).
#' @return If \code{full_output} = 1, a comprehensive list including: stand id; for breakage and overturning, the critical wind speeds (m s-1) at
#'  canopy top height and anemometer's height; zero plane displacement height (m), canopy surface roughness (m); canopy drag; ratio of critical wind speed at
#'  canopy top over friction velocity (uh / u*); drag per unit ground area; the deflection loading factor; the critical breaking moment and the critical
#'  overturning moment; the combined effect of edge, gap, and gustiness on the applied bending moment; the probabilities of damage; the mode of damage;
#'  a summary of the inputs. If \code{full_output} = 0, a much shorter list including: stand id; the critical wind speeds of breakage and overturning,
#'  and the associated probabilities.
fg_rou <- function(stand_id, species, mean_ht, mean_dbh, spacing, full_output = 1, weib_a = NA, weib_k = NA, top_ht = NA, mean_cr_width = NA,
                   mean_cr_depth = NA, soil_group = NA, rooting = NA, new_edge = NA, gap_size = NA, moe = NA, mor = NA, fknot = NA, stem_vol = NA,
                   crown_vol = NA, stem_density = NA, crown_density = NA, c_reg = NA, c_drag = NA, n_drag = NA, drag_upper_limit = NA, snow_depth = NA,
                   snow_density = NA, ro = NA, x = NA, y = NA, z = NA, dams = NA) {

  #1. Check essentials (species, mean_ht&top_ht, mean_dbh, spacing):
  essentials <- c(stand_id, species, mean_dbh, spacing)
  stopifnot(!anyNA(essentials))
  stopifnot(!(is.na(mean_ht) && is.na(top_ht)))

  #2. If mean_ht is not provided, calculate it from top_ht
  param0_height <- species_parameters[species, "param0_height"] #get(paste0("param0_height_", species))
  param1_height <- species_parameters[species, "param1_height"] #get(paste0("param1_height_", species))
  #mean_ht <- ifelse(is.na(mean_ht), param0_height + param1_height *top_ht, mean_ht)
  mean_ht <- ifelse(is.na(mean_ht), top_ht_to_mean_ht(param0_height, param1_height, top_ht), mean_ht)

  #3. Check desirables (mean_cr_width, mean_cr_depth, soil_group, rooting, new_edge, gap_size) and initialise 'default_warning':
  desirables <- c(mean_cr_width, mean_cr_depth, soil_group, rooting, new_edge, gap_size)
  default_warning <- ifelse(anyNA(desirables), "Warning: some inputs were set to default values", NA)

  param0_cr_width <- species_parameters[species, "param0_cr_width"] #get(paste0("param0_cr_width_", species))
  param1_cr_width <- species_parameters[species, "param1_cr_width"] #get(paste0("param1_cr_width_", species))
  #mean_cr_width <- ifelse(is.na(mean_cr_width), param0_cr_width + param1_cr_width * mean_dbh, mean_cr_width)
  mean_cr_width <- ifelse(is.na(mean_cr_width), canopy_width_fun(param0_cr_width, param1_cr_width, mean_dbh), mean_cr_width)
  mean_cr_width <- ifelse (mean_cr_width > 2 * spacing, 2 * spacing, mean_cr_width)
  mean_cr_width <- ifelse(mean_cr_width > mean_ht, mean_ht, mean_cr_width)
  mean_cr_width <- round(mean_cr_width, 2)

  param0_cr_depth <- species_parameters[species, "param0_cr_depth"] #get(paste0("param0_cr_depth_", species))
  param1_cr_depth <- species_parameters[species, "param1_cr_depth"] #get(paste0("param1_cr_depth_", species))
  #mean_cr_depth <- ifelse(is.na(mean_cr_depth), param0_cr_depth + param1_cr_depth * mean_ht, mean_cr_depth)
  mean_cr_depth <- ifelse(is.na(mean_cr_depth), canopy_depth_fun(param0_cr_depth, param1_cr_depth, mean_ht), mean_cr_depth)
  mean_cr_depth <- ifelse(mean_cr_depth > mean_ht, mean_ht, mean_cr_depth)
  mean_cr_depth <- ifelse(mean_cr_depth < 1, 1, mean_cr_depth)
  mean_cr_depth <- round(mean_cr_depth, 2)

  soil_group <- ifelse(is.na(soil_group), 1, soil_group) #defaults to 1: Freely draining mineral soils - Soil Group A
  rooting <- ifelse(is.na(rooting), 3, rooting) #defaults to 3 to choose average of c_reg from combined shallow and deep rooting data

  new_edge <- ifelse(is.na(new_edge), 0, new_edge)
  dist_edge <- ifelse(new_edge == 0, fgr_constants$tree_heights_inside_forest*mean_ht, 0)
  gap_size <- ifelse(is.na(gap_size), 0, gap_size)

  #4. Check Advanced Inputs (moe, mor, fknot, stem_vol, stem_density, crown_density, c_reg, c_drag, n_drag, drag_upper_limit, snow_depth, snow_density, ro). Calculate volume of stem, crown, snow
  moe <- ifelse(is.na(moe), species_parameters[species, "moe"], moe) #get(paste0("moe_", species)), moe)
  mor <- ifelse(is.na(mor), species_parameters[species, "mor"], mor) #get(paste0("mor_", species)), mor)
  stem_density <- ifelse(is.na(stem_density), species_parameters[species, "stem_density"], stem_density) #get(paste0("stem_density_", species)), stem_density)
  crown_density <- ifelse(is.na(crown_density), species_parameters[species, "canopy_density"], crown_density) #get(paste0("canopy_density_", species)), crown_density)
  fknot <- ifelse(is.na(fknot), species_parameters[species, "fknot"], fknot) #get(paste0("fknot_", species)), fknot)
  c_drag <- ifelse(is.na(c_drag), species_parameters[species, "c_drag"], c_drag) #get(paste0("c_drag_", species)), c_drag)
  n_drag <- ifelse(is.na(n_drag), species_parameters[species, "n_drag"], n_drag) #get(paste0("n_drag_", species)), n_drag)
  drag_upper_limit <- ifelse(is.na(drag_upper_limit), species_parameters[species, "drag_upper_limit"], drag_upper_limit) #get(paste0("drag_upper_limit_", species)), drag_upper_limit)
  rb <- species_parameters[species, "rb"] #get(paste0("rb_", species)) #Root Bending Term, from Neild and Wood
  snow_depth <- ifelse(is.na(snow_depth), 0, snow_depth)
  snow_density <- ifelse(is.na(snow_density), fgr_constants$snow_density_default, snow_density)
  ro <- ifelse(is.na(ro), fgr_constants$ro_default, ro)
  #Overturning Moment Multipliers (c_reg, n_drag.m/kg):
  #omm_dataframe <- get(paste0("omm_dataframe_", species))
  c_reg <- ifelse(is.na(c_reg), species_parameters[species, paste0("c_reg_soil_", soil_group, "_rd_", rooting)], c_reg) #get(paste0("c_reg_soil_", soil_group, "_rd_", rooting)), c_reg) #omm_dataframe[rooting, soil_group], c_reg) #'rooting' and 'soil_group' as rows and columns indices
  stem_vol <- ifelse(is.na(stem_vol), stem_vol_fun(species, mean_dbh, mean_ht), stem_vol)
  crown_vol <- ifelse(is.na(crown_vol), 1/3 * pi * mean_cr_depth * (mean_cr_width/2)^2, crown_vol)
  snow_vol <- ifelse(is.na(snow_depth), NA, pi * snow_depth * (mean_cr_width/2)^2)

  #5. Calculate stem weight and extract max stem weight to check confidence of overturning predictions. Calculate crown weight and snow weight
  stem_weight <- stem_vol * stem_density
  #msw_dataframe <- get(paste0("msw_dataframe_", species))
  msw <- species_parameters[species, paste0("msw_soil_", soil_group, "_rd_", rooting)] #msw_dataframe[rooting, soil_group]
  max_stem_weight_warning <- ifelse(stem_weight > msw, "Warning: the weight of the stem exceeds the data used to calculate the resistance to overturning", NA)
  crown_weight <- crown_vol * crown_density
  snow_weight <- ifelse(is.na(snow_vol), NA, snow_vol * snow_density)

  #6. Calculate Critical Wind Speeds and CWS-related outputs: (uh_b, bm_rou, zpd, gammaSolved, dlf_calc, breaking_moment, overturning_moment,
  #edge_gap_gust_factor). Outputs are given as lists, from which they are then extracted.
  #BREAKAGE:
  uh_b_results <- uh_breakage_rou(mean_ht, mean_dbh, spacing, dist_edge, gap_size, mean_cr_width, mean_cr_depth, moe, mor, fknot, n_drag, c_drag, drag_upper_limit,
                                    stem_vol, stem_density, crown_density, snow_depth, snow_density, ro) #output list contains (uh_b, bm_rou, zpd, gammaSolved,
                                    #dlf_calc, breaking_moment, edge_gap_gust_factor)
  #OVERTURNING:
  uh_o_results <- uh_overturning_rou(mean_ht, mean_dbh, spacing, dist_edge, gap_size, mean_cr_width, mean_cr_depth, moe, c_reg, n_drag, c_drag, drag_upper_limit,
                                       stem_vol, stem_density, crown_density, snow_depth, snow_density, ro) #output list contains (uh_b, bm_rou, zpd, gammaSolved,
                                       #dlf_calc, overturning_moment, edge_gap_gust_factor)
  #Extracting values. Some are independent of critical wind speed:
  bm_rou <- as.numeric(uh_b_results)[2] #Bending Moment
  breaking_moment <- as.numeric(uh_b_results)[6]
  overturning_moment <- as.numeric(uh_o_results)[6]
  dlf_calc <- as.numeric(uh_b_results)[5] #Deflection Loading Factor
  edge_gap_gust_factor <- as.numeric(uh_b_results)[7]
  #Others depend on wind speed, so they are output for breakage and overturning:
  uh_b <- as.numeric(uh_b_results)[1] #critical wind speed for breakage at canopy top
  gammasolved_b <- as.numeric(uh_b_results)[4] #Ratio of critical wind speed at canopy top over friction velocity (uh / u*). This for breakage
  zpd_b <- as.numeric(uh_b_results)[3] #zero plane displacement. This for breakage
  uh_o <- as.numeric(uh_o_results)[1] #critical wind speed for overturning at canopy top
  gammasolved_o <- as.numeric(uh_o_results)[4] #Ratio of critical wind speed at canopy top over friction velocity (uh / u*). This for overturning
  zpd_o <- as.numeric(uh_o_results)[3] #zero plane displacement. This for overturning

  #7. Calculate other streamlining parameters (drag, z0 and LambdaCapital). They depend on the CWS so they are output for breakage and overturning:
  z0_b <- z0_fun(mean_cr_width, mean_cr_depth, spacing, uh_b, n_drag, c_drag, drag_upper_limit, mean_ht) #Canopy roughness. This for breakage
  lambdacapital_b <- lambdacapital_fun(mean_cr_width, mean_cr_depth, spacing, uh_b, n_drag, c_drag, drag_upper_limit) #Drag per unit ground area. This for breakage
  drag_b <- drag_fun(uh_b, n_drag, c_drag, drag_upper_limit) #Drag (porosity) of crown. This for breakage
  z0_o <- z0_fun(mean_cr_width, mean_cr_depth, spacing, uh_o, n_drag, c_drag, drag_upper_limit, mean_ht) #Canopy roughness. This for breakage
  lambdacapital_o <- lambdacapital_fun(mean_cr_width, mean_cr_depth, spacing, uh_o, n_drag, c_drag, drag_upper_limit) #Drag per unit ground area. This for overturning
  drag_o <- drag_fun(uh_o, n_drag, c_drag, drag_upper_limit) #Drag (porosity) of crown. This for overturning

  #8. Elevate uh to anemometer height (10m + zpd)
  u10_b <- elevate(uh_b, z0_b, zpd_b, mean_ht) #This for breakage
  u10_o <- elevate(uh_o, z0_o, zpd_o, mean_ht) #This for overturning

  #9. Select CWS and mode of damage
  u_damage <- ifelse(uh_b < uh_o, uh_b, uh_o)
  u10_damage <- ifelse(u10_b < u10_o, u10_b, u10_o)
  mode_of_damage <- ifelse(u10_b < u10_o, "Breakage", "Overturning")

  #10. Check Wind Climate variables: (dams, weib_a, weib_k)
  weib_k <- ifelse(is.na(weib_k), 1.85, weib_k)
  weib_a <- ifelse(is.na(dams) & is.na(weib_a), weib_a, ifelse(is.na(dams), weib_a, ifelse(is.na(weib_a), fgr_constants$dams_to_weibull_a1 + fgr_constants$dams_to_weibull_a2*dams, weib_a)))

  #11. Calculate Probability of (breakage, overturning, damage) and associated return periods
  prob_b <- ifelse(is.na(weib_a), NA, annual_exceedance_prob(u10_b, weib_a, weib_k))
  prob_o <- ifelse(is.na(weib_a), NA, annual_exceedance_prob(u10_o, weib_a, weib_k))
  prob_damage <- ifelse(is.na(weib_a), NA, annual_exceedance_prob(u10_damage, weib_a, weib_k))

  #12. Outputs in long and short forms:
  fg_rou_out_full <- list(stand_id = stand_id, species = species, u10_b = u10_b, uh_b = uh_b, prob_b = prob_b, zpd_b = zpd_b, z0_b = z0_b,
                          drag_b = drag_b, gammasolved_b = gammasolved_b, lambdacapital_b = lambdacapital_b, breaking_moment = breaking_moment,
                          u10_o = u10_o, uh_o = uh_o, prob_o = prob_o, zpd_o = zpd_o, z0_o = z0_o, drag_o = drag_o, gammasolved_o = gammasolved_o,
                          lambdacapital_o = lambdacapital_o, overturning_moment = overturning_moment,
                          mode_of_damage = mode_of_damage, u10_damage = u10_damage, u_damage = u_damage, prob_damage = prob_damage,
                          bm_rou = bm_rou, dlf_calc = dlf_calc, edge_gap_gust_factor = edge_gap_gust_factor,
                          mean_ht = mean_ht, top_ht = top_ht, mean_dbh = mean_dbh, spacing = spacing, mean_cr_width = mean_cr_width,
                          mean_cr_depth = mean_cr_depth, soil_group = soil_group, rooting = rooting, weib_a = weib_a, weib_k = weib_k,
                          new_edge = new_edge, gap_size = gap_size, moe = moe, mor = mor, fknot = fknot, stem_vol = stem_vol, crown_vol = crown_vol,
                          stem_density = stem_density, crown_density = crown_density, stem_weight = stem_weight, crown_weight = crown_weight,
                          c_reg = c_reg, c_drag = c_drag, n_drag = n_drag, drag_upper_limit = drag_upper_limit, snow_depth = snow_depth,
                          snow_density = snow_density, snow_vol = snow_vol, snow_weight = snow_weight, ro = ro, x = x, y = y, z = z, dams = dams,
                          Warning_Variables = default_warning, Max_Stem_Weight_Warning = max_stem_weight_warning)


  fg_rou_out_short <- list(stand_id = stand_id, species = species, u10_damage = u10_damage, mode_of_damage = mode_of_damage,
                           u10_b = u10_b, u10_o = u10_o, prob_damage = prob_damage, prob_b = prob_b, prob_o = prob_o,
                           Warning_Variables = default_warning, Max_Stem_Weight_Warning = max_stem_weight_warning)

  if(full_output == 1) {
    fg_rou_out <- fg_rou_out_full
  } else fg_rou_out <- fg_rou_out_short

  return(fg_rou_out)
}
