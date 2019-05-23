#' Wrapper for the roughness method.
#' @title ForestGALES - Roughness method.
#' @param stand_id Stand identifier. Essential.
#' @param date Date of wind damage risk assessment. Optional (but useful! Mind the variable order in the function call!).
#' @param species Tree species under investigation. Essential.
#' @param mean_ht Arithmetic mean height of the trees in the stand (m). Essential (unless \code{top_ht} is provided).
#' @param mean_dbh Mean dbh of all trees in the stand (cm). Dbh is diameter at breast height, measured at 1.3m above the ground. Essential.
#' @param spacing Mean distance between trees (m). Essential.
#' @param weib_a Scale parameter of the Weibull distribution for local wind speeds.
#' @param weib_k Shape parameter of the Weibull distribution for local wind speeds.
#' @param full_output Switch between full and basic outputs.
#' @param top_ht The height of the dominant tree(s) in the stand (m). Essential (unless \code{mean_ht} is provided).
#' @param mean_cr_width Width of the crown of the mean tree in the stand (m).
#' @param mean_cr_depth Length of the crown of the mean tree in the stand (m).
#' @param soil_group Soil group identifier (1 = freely draining mineral soils; 2 = gleyed mineral soils; 3 = peaty mineral soils; 4 = deep peats).
#' @param rooting Rooting depth class (1 = Shallow, 2 = Deep, 3 = Medium).
#' @param new_edge Switch to toggle between green upwind edge (0) or brown upwind edge (1)
#' @param gap_size Length of the upwind gap (m).
#' @param moe Modulus of Elasticity of green wood (MPa). Advanced Input.
#' @param mor Modulus of Rupture of green wood (MPa). Advanced Input.
#' @param fknot Knot factor (dimensionless). Advanced Input.
#' @param stem_vol Stem volume of the mean tree in the stand (m^3). Advanced Input.
#' @param crown_vol Volume of the crown of the mean tree (m^3). Advanced Input.
#' @param stem_density Density of green wood of the stem (kg m-3). Advanced Input.
#' @param crown_density Density of the crown of the mean tree (kg m-3). Advanced Input.
#' @param c_reg Regression coefficients of uprooting moment against stem weight (N m kg-1). Advanced Input.
#' @param n_drag N parameter of the drag coefficient formula (dimensionless). Advanced Input.
#' @param c_drag C parameter of the drag coefficient formula (dimensionless). Advanced Input.
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1). Advanced Input.
#' @param snow_depth Depth of layer of snow on tree crown (cm). Advanced Input.
#' @param snow_density Density of snow (kg m-3). Advanced Input.
#' @param ro Air density (kg m-3). Advanced Input.
#' @param x Spatial coordinate.
#' @param y Spatial coordinate.
#' @param z Spatial coordinate.
#' @param dams DAMS (Detailed Aspect Method of Scoring) value to describe the local wind climate (for UK conditions only).
#' @return If \code{full_output} = 1, a comprehensive list including: stand id; for breakage and overturning, the critical wind speeds (m s-1) at
#'  canopy top height and anemometer's height; zero plane displacement height (m), canopy surface roughness (m); canopy drag; ratio of critical wind speed at
#'  canopy top over friction velocity (uh / u*); drag per unit ground area; the deflection loading factor; the critical breaking moment and the critical
#'  overturning moment; the combined effect of edge, gap, and gustiness on the applied bending moment; the probabilities of damage; the mode of damage;
#'  a summary of the inputs. If \code{full_output} = 0, a much shorter list including: stand id; the critical wind speeds of breakage and overturning,
#'  and the associated probabilities.
fg_rou <- function(stand_id, date, species, mean_ht, mean_dbh, spacing, full_output = 1, weib_a = NA, weib_k = NA, top_ht = NA, mean_cr_width = NA,
                   mean_cr_depth = NA, soil_group = NA, rooting = NA, new_edge = NA, gap_size = NA, moe = NA, mor = NA, fknot = NA, stem_vol = NA,
                   crown_vol = NA, stem_density = NA, crown_density = NA, c_reg = NA, c_drag = NA, n_drag = NA, drag_upper_limit = NA, snow_depth = NA,
                   snow_density = NA, ro = NA, x = NA, y = NA, z = NA, dams = NA) {

  #1. Check essentials (species, mean_ht&top_ht, mean_dbh, spacing):
  essentials <- c(stand_id, species, mean_dbh, spacing)
  stopifnot(!anyNA(essentials))
  stopifnot(!(is.na(mean_ht) && is.na(top_ht)))
  spacing <- round(spacing, 1)

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
  #mean_cr_width <- ifelse (mean_cr_width > 2 * spacing, 2 * spacing, mean_cr_width)
  #mean_cr_width <- ifelse(mean_cr_width > mean_ht, mean_ht, mean_cr_width)
  mean_cr_width <- round(mean_cr_width, 2)

  param0_cr_depth <- species_parameters[species, "param0_cr_depth"] #get(paste0("param0_cr_depth_", species))
  param1_cr_depth <- species_parameters[species, "param1_cr_depth"] #get(paste0("param1_cr_depth_", species))
  #mean_cr_depth <- ifelse(is.na(mean_cr_depth), param0_cr_depth + param1_cr_depth * mean_ht, mean_cr_depth)
  mean_cr_depth <- ifelse(is.na(mean_cr_depth), canopy_depth_fun(param0_cr_depth, param1_cr_depth, mean_ht), mean_cr_depth)
  mean_cr_depth <- ifelse(mean_cr_depth > mean_ht, mean_ht, mean_cr_depth)
  #mean_cr_depth <- ifelse(mean_cr_depth < 1, 1, mean_cr_depth)
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
  dlf_used <- as.numeric(uh_b_results)[7]

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
  #add stems per hectare (sph):
  sph <- round(10000 / (spacing^2), 0)
  #give meaningful date output:
  date <- as.Date(date)

  fg_rou_out_full <- list(stand_id = stand_id, date = as.Date(date), species = species, u10_b = u10_b, uh_b = uh_b, prob_b = prob_b, zpd_b = zpd_b, z0_b = z0_b,
                          drag_b = drag_b, gammasolved_b = gammasolved_b, lambdacapital_b = lambdacapital_b, breaking_moment = breaking_moment,
                          u10_o = u10_o, uh_o = uh_o, prob_o = prob_o, zpd_o = zpd_o, z0_o = z0_o, drag_o = drag_o, gammasolved_o = gammasolved_o,
                          lambdacapital_o = lambdacapital_o, overturning_moment = overturning_moment,
                          mode_of_damage = mode_of_damage, u10_damage = u10_damage, u_damage = u_damage, prob_damage = prob_damage,
                          bm_rou = bm_rou, dlf_calc = dlf_calc, dlf_used = dlf_used, edge_gap_gust_factor = edge_gap_gust_factor,
                          mean_ht = mean_ht, top_ht = top_ht, mean_dbh = mean_dbh, spacing = spacing, sph = sph, mean_cr_width = mean_cr_width,
                          mean_cr_depth = mean_cr_depth, soil_group = soil_group, rooting = rooting, weib_a = weib_a, weib_k = weib_k,
                          new_edge = new_edge, gap_size = gap_size, moe = moe, mor = mor, fknot = fknot, stem_vol = stem_vol, crown_vol = crown_vol,
                          stem_density = stem_density, crown_density = crown_density, stem_weight = stem_weight, crown_weight = crown_weight,
                          c_reg = c_reg, c_drag = c_drag, n_drag = n_drag, drag_upper_limit = drag_upper_limit, snow_depth = snow_depth,
                          snow_density = snow_density, snow_vol = snow_vol, snow_weight = snow_weight, ro = ro, x = x, y = y, z = z, dams = dams,
                          Warning_Variables = default_warning, Max_Stem_Weight_Warning = max_stem_weight_warning)


  fg_rou_out_short <- list(stand_id = stand_id, date = date, species = species, u10_damage = u10_damage, mode_of_damage = mode_of_damage,
                           u10_b = u10_b, u10_o = u10_o, prob_damage = prob_damage, prob_b = prob_b, prob_o = prob_o,
                           Warning_Variables = default_warning, Max_Stem_Weight_Warning = max_stem_weight_warning)

  if(full_output == 1) {
    fg_rou_out <- fg_rou_out_full
  } else fg_rou_out <- fg_rou_out_short

  return(fg_rou_out)
}
