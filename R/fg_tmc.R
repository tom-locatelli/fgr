#' Wrapper for the single-tree (turning moment coefficient) method.
#' @title ForestGALES - Single-tree (TMC) method.
#' @param stand_id Stand identifier. Essential.
#' @param tree_id Tree identifier. Essential.
#' @param date Date of wind damage risk assessment. Optional (but useful!).
#' @param species Tree species under investigation. Essential.
#' @param tree_ht Individual tree height (m). Essential.
#' @param dbh Diameter of the stem at breast height, i.e. 1.3m above the ground (cm). Essential.
#' @param spacing_current Current mean spacing of trees in the stand (m). Essential.
#' @param predominant_species Predominant species in the stand. Defaults to \code{species}.
#' @param stand_mean_ht Arithmetic mean height of the trees in the stand (m). Essential.
#' @param stand_mean_dbh Mean dbh of all trees in the stand (cm) - arithmetic average. Dbh is diameter at breast height, measured at 1.3m above the ground. Essential.
#' @param stand_top_ht Height of the dominant tree(s) in the stand (m). Essential.
#' @param full_output Switch between full and basic outputs.
#' @param weib_a Scale parameter of the Weibull distribution for local wind speeds.
#' @param weib_k Shape parameter of the Weibull distribution for local wind speeds.
#' @param ci Competition Index (\code{BAL}, \code{Hegyi}, \code{None}) used.
#' @param ci_value Value of \code{ci}.
#' @param elev_ht The height at which the elevated critical wind speed is calculated (m). For the TMC method, this defaults to 1.05 * stand top height
#' @param cr_width Width of the tree crown (m).
#' @param cr_depth Length of the tree crown (m).
#' @param stand_cr_width Width of the crown of the mean tree in the stand (m).
#' @param stand_cr_depth Length of the crown of the mean tree in the stand (m).
#' @param soil_group Soil group identifier (1 = freely draining mineral soils; 2 = gleyed mineral soils; 3 = peaty mineral soils; 4 = deep peats).
#' @param rooting Rooting depth class (1 = Shallow, 2 = Deep, 3 = Medium).
#' @param dist_edge Distance of tree from the upwind edge (m).
#' @param gap_size Length of the upwind gap (m).
#' @param spacing_before Mean spacing of trees in the stand before any thinning (m).
#' @param years_since_thin Number of years since most recent thinning.
#' @param species_parameters Data.frame covering all species-specific parameters. Defaults to fgr Internal Data species paramters data.frame. If specified by the user, it can have as many or as few species (i.e. rows) as required. It is recommended that when working with user-defined species, stem volume is specified and provided as an input (see below in input list). When this is not possible, stem volume is calculated with Eq. 4 of Fonweban et al. 2012. In order to allow this, letter codes for User-defined species must begin with a capital "U" for the model to recognise that the Fonweban et al. formula needs applied to calculate stem volume.
#' @param fgr_constants Data.frame including all constants required for model execution. Defaults to fgr Internal Data fgr constants data.frame. these include entities that can be individually supplied to the model (e.g. ro, the air density, and snow density - see below in input list), or entities that qould not normally require modification (e.g. constants used in the calculation of the Deflection Loading Factor, the Annual Exceedance Probability, the Edge, Gap, and Gust Factors, aerodynamic quantities such as zpd, z0, and gammasolved - but even von Karman's and the gravitational constants can be modified!)
#' @param moe Modulus of Elasticity of green wood (MPa). Advanced Input.
#' @param mor Modulus of Rupture of green wood (MPa). Advanced Input.
#' @param fknot Knot factor (dimensionless). Advanced Input.
#' @param stem_vol Individual tree stem volume (m3). Advanced Input.
#' @param crown_vol Volume of the crown of individual trees (m3). Advanced Input.
#' @param stem_density Density of green wood of the stem (kg m-3). Advanced Input.
#' @param crown_density Density of the crown of individual trees (kg m-3). Advanced Input.
#' @param c_reg Regression coefficient of uprooting moment against stem weight (N m kg-1). Advanced Input.
#' @param c_drag C parameter of the drag coefficient formula (dimensionless). Advanced Input.
#' @param n_drag N parameter of the drag coefficient formula (dimensionless). Advanced Input.
#' @param drag_upper_limit Maximum wind speed used during the experiments from which \code{n_drag} and \code{c_drag} were derived (m*s-1). Advanced Input.
#' @param snow_depth Depth of layer of snow on tree crown (cm). Advanced Input.
#' @param snow_density Density of snow (kg m-3). Advanced Input.
#' @param ro Air density (kg m-3). Advanced Input.
#' @param x Spatial coordinate.
#' @param y Spatial coordinate.
#' @param z Spatial coordinate.
#' @param dams DAMS (Detailed Aspect Method of Scoring) value to describe the local wind climate (for UK conditions only).
#' @return If \code{full_output} = 1, a comprehensive list including: stand and tree id's; for breakage and overturning, the critical wind speeds (m s-1) at
#'  canopy top height and anemometer's height; zero plane displacement height (m), canopy surface roughness (m); canopy drag; ratio of critical wind speed at
#'  canopy top over friction velocity (uh / u*); drag per unit ground area; the turning moment coefficient; the turning moment ratio; the deflection loading
#'  factor; the critical breaking moment and the critical overturning moment; the combined effect of edge and gap on the applied bending moment; the probabilities
#'  of damage; the mode of damage; a summary of the inputs. If \code{full_output} = 0, a much shorter list including: stand id and tree id's; the critical
#'  wind speeds of breakage and overturning, and the associated probabilities.
fg_tmc <- function(stand_id, tree_id, date = NA, species, tree_ht, dbh, spacing_current, predominant_species = species, stand_mean_ht, stand_mean_dbh, stand_top_ht,
                   full_output = 1, weib_a = NA, weib_k = NA, ci = NA, ci_value = NA, elev_ht = NA, cr_width = NA, cr_depth = NA, stand_cr_width = NA, stand_cr_depth = NA,
                   soil_group = NA, rooting = NA, dist_edge = NA, gap_size = NA, spacing_before = NA, years_since_thin = NA, species_parameters = fgr:::species_parameters, fgr_constants = fgr:::fgr_constants, moe = NA, mor = NA, fknot = NA,
                   stem_vol = NA,crown_vol = NA, stem_density = NA, crown_density = NA, c_reg = NA, c_drag = NA, n_drag = NA, drag_upper_limit = NA,
                   snow_depth = NA, snow_density = NA, ro = NA, x = NA, y = NA, z = NA, dams = NA) { #

  #1. Check essentials (stand_id, tree_id, species, tree_ht, dbh, spacing_current, stand_mean_dbh + stand_mean_ht&stand_top_ht):
  essentials <- c(stand_id, tree_id, species, tree_ht, dbh, spacing_current, stand_mean_dbh, stand_top_ht)
  stopifnot(!anyNA(essentials))
  #stopifnot(!(is.na(stand_mean_ht) && is.na(stand_top_ht)))
  #if(is.na(stand_mean_ht) & is.na(stand_top_ht)) stop("Essentials missing")
  #ifelse(is.na(stand_mean_ht), ifelse(is.na(stand_top_ht), stop("Essentials missing")))
  spacing_current <- round(spacing_current, 1)
  #2. An "equivalent mean stand height" needs to be calculated from stand top height using a generic stand top height to equivalent mean stand height conversion.
  #This equivalent mean stand height represents the level in the stand responsible for most of the momentum absorption. For this reason, stand top height is an essential input.
  #In fact, stand top height is largely independent of the understorey. This conversion, based on average slope and intercept from the species in ForestGALES, is as below:
  equivalent_mean_ht <- eq_mean_ht_fun(stand_top_ht)


  #OLD_2. If stand_top_ht is not provided, calculate it from stand_mean_ht, and viceversa. This requires knowledge of the predominant_species, which if not specified is equal to species
  #stand_mean_ht is used in the calculations of z0 and zpd, while stand_top_ht# in the elevate function
  predominant_species <- ifelse(is.na(predominant_species), species, predominant_species) #still required to assign species-specific parameters
  #param0_standHt <- species_parameters[species, "param0_height"] #get(paste0("param0_height_", predominant_species))
  #param1_standHt <- species_parameters[species, "param1_height"] #get(paste0("param1_height_", predominant_species))
  #stand_mean_ht <- ifelse(is.na(stand_mean_ht), param0_standHt + param1_standHt * stand_top_ht, stand_mean_ht)
  #stand_mean_ht <- ifelse(is.na(stand_mean_ht), top_ht_to_mean_ht(param0_standHt, param1_standHt, stand_top_ht), stand_mean_ht)
  #stand_top_ht <- ifelse(is.na(stand_top_ht), (stand_mean_ht - param0_standHt) / param1_standHt, stand_top_ht)
  #stand_top_ht <- ifelse(is.na(stand_top_ht), mean_ht_to_top_ht(param0_standHt, param1_standHt, stand_mean_ht), stand_top_ht)

  #3. Check desirables (cr_width, cr_depth, stand_cr_width, stand_cr_depth, soil_group, rooting, gap_size, dist_edge, status) and initialise 'default_warning':
  desirables <- c(cr_width, cr_depth, stand_cr_width, stand_cr_depth, soil_group, rooting, gap_size, dist_edge)
  default_warning <- ifelse(anyNA(desirables), "defw", NA) #"Warning: some inputs were set to default values"
  #Single tree first:
  param0_cr_width <- species_parameters[species, "param0_cr_width"] #get(paste0("param0_crWidth_", species))
  param1_cr_width <- species_parameters[species, "param1_cr_width"] #get(paste0("param1_crWidth_", species))
  #cr_width <- ifelse(is.na(cr_width), param0_cr_width + param1_cr_width * dbh, cr_width)
  cr_width <- ifelse(is.na(cr_width), canopy_width_fun(param0_cr_width, param1_cr_width, dbh), cr_width)
  #cr_width <- ifelse (cr_width > 2 * spacing_current, 2 * spacing_current, cr_width)
  #cr_width <- ifelse(cr_width > tree_ht, tree_ht, cr_width)
  cr_width <- round(cr_width, 2)

  param0_cr_depth <- species_parameters[species, "param0_cr_depth"] #get(paste0("param0_cr_depth_", species))
  param1_cr_depth <- species_parameters[species, "param1_cr_depth"] #get(paste0("param1_cr_depth_", species))
  #cr_depth <- ifelse(is.na(cr_depth), param0_cr_depth + param1_cr_depth * tree_ht, cr_depth)
  cr_depth <- ifelse(is.na(cr_depth), canopy_depth_fun(param0_cr_depth, param1_cr_depth, tree_ht), cr_depth)
  cr_depth <- ifelse(cr_depth > tree_ht, tree_ht, cr_depth)
  #cr_depth <- ifelse(cr_depth < 1, 1, cr_depth)
  cr_depth <- round(cr_depth, 2)

  #stand "equivalent mean tree":
  param0_stand_cr_width <- species_parameters[predominant_species, "param0_cr_width"] #get(paste0("param0_crWidth_", predominant_species))
  param1_stand_cr_width <- species_parameters[predominant_species, "param1_cr_width"] #get(paste0("param1_crWidth_", predominant_species))
  #stand_cr_width <- ifelse(is.na(stand_cr_width), param0_stand_cr_width + param1_stand_cr_width * stand_mean_dbh, stand_cr_width)
  stand_cr_width <- ifelse(is.na(stand_cr_width), canopy_width_fun(param0_stand_cr_width, param1_stand_cr_width, stand_mean_dbh), stand_cr_width)
  #stand_cr_width <- ifelse (stand_cr_width > 2 * spacing_current, 2 * spacing_current, stand_cr_width)
  #stand_cr_width <- ifelse(stand_cr_width > stand_mean_ht, stand_mean_ht, stand_cr_width)
  stand_cr_width <- round(stand_cr_width, 2)

  param0_stand_cr_depth <- species_parameters[predominant_species, "param0_cr_depth"] #get(paste0("param0_crDepth_", predominant_species))
  param1_stand_cr_depth <- species_parameters[predominant_species, "param1_cr_depth"] #get(paste0("param1_crDepth_", predominant_species))
  #stand_cr_depth <- ifelse(is.na(stand_cr_depth), param0_stand_cr_depth + param1_stand_cr_depth * stand_mean_ht, stand_cr_depth)
  stand_cr_depth <- ifelse(is.na(stand_cr_depth), canopy_depth_fun(param0_stand_cr_depth, param1_stand_cr_depth, equivalent_mean_ht), stand_cr_depth)
  stand_cr_depth <- ifelse(stand_cr_depth > equivalent_mean_ht, equivalent_mean_ht, stand_cr_depth)
  #stand_cr_depth <- ifelse(stand_cr_depth < 1, 1, stand_cr_depth)
  stand_cr_depth <- round(stand_cr_depth, 2)

  soil_group <- ifelse(is.na(soil_group), 1, soil_group) #defaults to 1: Freely draining mineral soils - Soil Group A
  rooting <- ifelse(is.na(rooting), 3, rooting) #defaults to 3 to choose average of c_reg from combined shallow and deep rooting data

  dist_edge <- ifelse(is.na(dist_edge), fgr_constants$tree_heights_inside_forest*equivalent_mean_ht, dist_edge)
  gap_size <- ifelse(is.na(gap_size), 0, gap_size)

  #4. Check competition variables:
  #status <- ifelse(is.na(status), "co", status)
  ci <- ifelse(is.na(ci), "none", ci)
  ci_value <- ifelse(is.na(ci_value), NA, ci_value)

  #5. Check variables required for modelling effect of thinning:
  spacing_before <- round(ifelse(is.na(spacing_before), spacing_current, spacing_before), 1)
  years_since_thin <- ifelse(is.na(years_since_thin), 5, years_since_thin)
  years_since_thin <- ifelse(years_since_thin < 0, 0, years_since_thin)
  years_since_thin <- ifelse(years_since_thin > 5, 5, years_since_thin)

  #6. Check Advanced Inputs (elev_ht, moe, mor, fknot, stem_vol, stem_density, crown_vol, crown_density, c_reg, c_drag, n_drag, drag_upper_limit, snow_depth, snow_density, ro). Calculate volume of stem, crown, snow
  elev_ht <- ifelse(is.na(elev_ht), 1.05*stand_top_ht, elev_ht) #Default height to calculate elevated u_crit is mean_ht
  moe <- ifelse(is.na(moe), species_parameters[species, "moe"], moe) #get(paste0("moe_", species)), moe)
  mor <- ifelse(is.na(mor), species_parameters[species, "mor"], mor) #get(paste0("mor_", species)), mor)
  stem_density <- ifelse(is.na(stem_density), species_parameters[species, "stem_density"], stem_density) #get(paste0("stem_density_", species)), stem_density)
  crown_density <- ifelse(is.na(crown_density), species_parameters[species, "canopy_density"], crown_density) #get(paste0("canopy_density_", species)), crown_density)
  fknot <- ifelse(is.na(fknot), species_parameters[species, "fknot"], fknot) #get(paste0("fknot_", species)), fknot)
  #c_drag, n_drag, and drag_upper_limit are used in the calculations of the aerodynamic variables (drag, zpd, z0, lambdaCapital, gammaSolved). They are thus stand-level properties and therefore
  #require that the parameters required for their calculation belong to the predominant species in the stand.
  c_drag <- ifelse(is.na(c_drag), species_parameters[predominant_species, "c_drag"], c_drag) #get(paste0("c_drag_", predominant_species)), c_drag)
  n_drag <- ifelse(is.na(n_drag), species_parameters[predominant_species, "n_drag"], n_drag) #get(paste0("n_drag_", predominant_species)), n_drag)
  drag_upper_limit <- ifelse(is.na(drag_upper_limit), species_parameters[predominant_species, "drag_upper_limit"], drag_upper_limit) #get(paste0("drag_upper_limit_", predominant_species)), drag_upper_limit)
  rb <- species_parameters[predominant_species, "rb"] #get(paste0("RB_", species)) #Root Bending Term, from Neild and Wood
  snow_depth <- ifelse(is.na(snow_depth), 0, snow_depth)
  snow_density <- ifelse(is.na(snow_density), fgr_constants$snow_density_default, snow_density)
  ro <- ifelse(is.na(ro), fgr_constants$ro_default, ro)
  #Overturning Moment Multipliers (c_reg, n m/kg):
  #omm_dataframe <- get(paste0("omm_dataframe_", species))
  c_reg <- ifelse(is.na(c_reg), species_parameters[species, paste0("c_reg_soil_", soil_group, "_rd_", rooting)], c_reg) #get(paste0("c_reg_soil_", soil_group, "_rd_", rooting)), c_reg) #omm_dataframe[rooting, soil_group], c_reg) #'rooting' and 'soil_group' as rows and columns indices
  stem_vol <- ifelse(is.na(stem_vol), stem_vol_fun(species, dbh, tree_ht, species_parameters), stem_vol)
  crown_vol <- ifelse(is.na(crown_vol), 1/3 * pi * cr_depth * (cr_width/2)^2, crown_vol)
  snow_vol <- ifelse(is.na(snow_depth), NA, pi * snow_depth * (cr_width/2)^2)

  #7. Calculate stem weight and extract max stem weight to check confidence of overturning predictions. Calculate crown weight and snow weight
  stem_weight <- stem_vol * stem_density
  #msw_dataframe <- get(paste0("msw_dataframe_", species))
  msw <- species_parameters[species, paste0("msw_soil_", soil_group, "_rd_", rooting)] #msw_dataframe[rooting, soil_group]
  max_stem_weight_warning <- ifelse(stem_weight > msw, "msw", NA) #"Warning: the weight of the stem exceeds the data used to calculate the resistance to overturning"
  crown_weight <- crown_vol * crown_density
  snow_weight <- ifelse(is.na(snow_vol), NA, snow_vol * snow_density)

  #8. Calculate Critical Wind Speeds and CWS-related outputs: (uh_b, uh_o, tmc, tmr_simple, dlf_calc, breaking_moment, overturning_moment, edge_gap_factor).
      #Outputs are given as lists, from which they are then extracted.
  #BREAKAGE:
  uh_b_results <- uh_breakage_tmc(tree_ht, dbh, cr_depth, cr_width, spacing_current, spacing_before, years_since_thin, dist_edge, gap_size, moe, mor, fknot,
                                    stem_vol, stem_density, crown_density, snow_depth, snow_density, ci, ci_value, n_drag, c_drag, drag_upper_limit, equivalent_mean_ht, stand_cr_depth, stand_cr_width, fgr_constants) #output list contains (uh_b, tmc, tmr_full, dlf_calc, breaking_moment, edge_gap_factor)

#  uh_b_results_tmr_simple <- uh_breakage_tmc_tmr_simple(tree_ht, dbh, cr_depth, cr_width, spacing_current, spacing_before, years_since_thin, dist_edge, gap_size, equivalent_mean_ht, moe, mor, fknot,
#                                  stem_vol, stem_density, crown_density, snow_depth, snow_density, ci, ci_value) #output list contains (uh_b, tmc, tmr_simple, dlf_calc, breaking_moment, edge_gap_factor)

  #OVERTURNING:
  uh_o_results <- uh_overturning_tmc(tree_ht, dbh, cr_depth, cr_width, spacing_current, spacing_before, years_since_thin, dist_edge, gap_size, moe, c_reg,
                                       stem_vol, stem_density, crown_density, snow_depth, snow_density, ci, ci_value, n_drag, c_drag, drag_upper_limit, equivalent_mean_ht, stand_cr_depth, stand_cr_width, fgr_constants) #output list contains (uh_o, tmc, tmr_full, dlf_calc, overturning_moment, edge_gap_factor)

#  uh_o_results <- uh_overturning_tmc_tmr_simple(tree_ht, dbh, cr_depth, cr_width, spacing_current, spacing_before, years_since_thin, dist_edge, gap_size, equivalent_mean_ht, moe, c_reg,
#                                  stem_vol, stem_density, crown_density, snow_depth, snow_density, ci, ci_value) #output list contains (uh_o, tmc, tmr_simple, dlf_calc, overturning_moment, edge_gap_factor)


  #Extracting values:
  breaking_moment <- as.numeric(uh_b_results)[5]
  overturning_moment <- as.numeric(uh_o_results)[5]
  dlf_calc <- as.numeric(uh_b_results)[4] #Deflection Loading Factor
  edge_gap_factor <- as.numeric(uh_b_results)[6]
  tmc <- as.numeric(uh_b_results)[2]
  tmr_full <- as.numeric(uh_b_results)[3]
  dlf_used <- as.numeric(uh_b_results)[7]
  #tmr_simple <- as.numeric(uh_b_results)[3]
  #CWS:
  uh_b <- as.numeric(uh_b_results)[1] #critical wind speed for breakage at canopy top
  uh_o <- as.numeric(uh_o_results)[1] #critical wind speed for overturning at canopy top

  #9. Calculate zpd and z0 for the elevate function. Also calculate other streamlining parameters (drag, gammaSolved, LambdaCapital).
      #These outputs all depend on the CWS so they are given for breakage and overturning:
  zpd_b <- zpd_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_b, n_drag, c_drag, drag_upper_limit, equivalent_mean_ht, fgr_constants) #Zero plane displacement. This for breakage
  z0_b <- z0_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_b, n_drag, c_drag, drag_upper_limit, equivalent_mean_ht, fgr_constants) #Canopy roughness. This for breakage
  lambdacapital_b <- lambdacapital_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_b, n_drag, c_drag, drag_upper_limit) #Drag per unit ground area. This for breakage
  gammasolved_b <- gammasolved_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_b, n_drag, c_drag, drag_upper_limit, fgr_constants) #Ratio of critical wind speed at canopy top over friction velocity (uh / u*). This for breakage
  canopybreadth_b <- canopy_breadth_fun(stand_cr_width, uh_b, n_drag, c_drag, drag_upper_limit) #breadth of streamlined canopy for breakage
  drag_b <- drag_fun(uh_b, n_drag, c_drag, drag_upper_limit) #Drag (porosity) of crown. This for breakage
  zpd_o <- zpd_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_o, n_drag, c_drag, drag_upper_limit, equivalent_mean_ht, fgr_constants) #Zero plane displacement. This for overturning
  z0_o <- z0_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_o, n_drag, c_drag, drag_upper_limit, equivalent_mean_ht, fgr_constants) #Canopy roughness. This for overturning
  gammasolved_o <- gammasolved_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_o, n_drag, c_drag, drag_upper_limit, fgr_constants) #Ratio of critical wind speed at canopy top over friction velocity (uh / u*). This for overturning
  lambdacapital_o <- lambdacapital_fun(stand_cr_width, stand_cr_depth, spacing_current, uh_o, n_drag, c_drag, drag_upper_limit) #Drag per unit ground area. This for overturning
  canopybreadth_o <- canopy_breadth_fun(stand_cr_width, uh_o, n_drag, c_drag, drag_upper_limit) #breadth of streamlined canopy for breakage
  drag_o <- drag_fun(uh_o, n_drag, c_drag, drag_upper_limit) #Drag (porosity) of crown. This for overturning

  #10. Elevate uh to anemometer height (10m + zpd)
  u10_b <- elevate(uh_b, z0_b, zpd_b, elev_ht) #This for breakage
  u10_o <- elevate(uh_o, z0_o, zpd_o, elev_ht) #This for overturning

  #11. Select CWS and mode of damage
  u_damage <- ifelse(uh_b < uh_o, uh_b, uh_o)
  u10_damage <- ifelse(u10_b < u10_o, u10_b, u10_o)
  mode_of_damage <- ifelse(u10_b < u10_o, "Breakage", "Overturning")

  #12. Check Wind Climate variables: (dams, weib_a, weib_k)
  weib_k <- ifelse(is.na(weib_k), 1.85, weib_k)
  weib_a <- ifelse(is.na(dams) & is.na(weib_a), weib_a, ifelse(is.na(dams), weib_a, ifelse(is.na(weib_a), fgr_constants$dams_to_weibull_a1 + fgr_constants$dams_to_weibull_a2*dams, weib_a)))

  #13. Calculate Probability of (breakage, overturning, damage) and associated return periods
  prob_b <- ifelse(is.na(weib_a), NA, annual_exceedance_prob(u10_b, weib_a, weib_k, fgr_constants))
  prob_o <- ifelse(is.na(weib_a), NA, annual_exceedance_prob(u10_o, weib_a, weib_k, fgr_constants))
  prob_damage <- ifelse(is.na(weib_a), NA, annual_exceedance_prob(u10_damage, weib_a, weib_k, fgr_constants))

  #14. Outputs in long and short forms:
  #add stems per hectare (sph):
  sph_before <- round(10000 / (spacing_before^2), 0)
  sph_current <- round(10000 / (spacing_current^2), 0)
  fgr_warnings <- paste0(default_warning, "_", max_stem_weight_warning)

  fg_tmc_out_full <- list(stand_id = stand_id, tree_id = tree_id, date = date, species = species, u10_b = u10_b, uh_b = uh_b, prob_b = prob_b, zpd_b = zpd_b, z0_b = z0_b,
                          drag_b = drag_b, gammasolved_b = gammasolved_b, lambdacapital_b = lambdacapital_b, canopybreadth_b = canopybreadth_b,
                          breaking_moment = breaking_moment, u10_o = u10_o, uh_o = uh_o, prob_o = prob_o, zpd_o = zpd_o, z0_o = z0_o, drag_o = drag_o,
                          gammasolved_o = gammasolved_o,lambdacapital_o = lambdacapital_o, canopybreadth_o = canopybreadth_o, overturning_moment = overturning_moment, c_reg = c_reg,
                          mode_of_damage = mode_of_damage, u10_damage = u10_damage, u_damage = u_damage, prob_damage = prob_damage,
                          tmc = tmc, tmr_full = tmr_full, ci = ci, ci_value = ci_value,
                          tree_ht = tree_ht, dbh = dbh, cr_width = cr_width, cr_depth = cr_depth, dlf_calc = dlf_calc, dlf_used = dlf_used, stem_vol = stem_vol,
                          stem_density = stem_density, stem_weight = stem_weight, crown_vol = crown_vol, crown_density = crown_density,
                          crown_weight = crown_weight, snow_depth = snow_depth, snow_density = snow_density, snow_weight = snow_weight,
                          predominant_species = predominant_species, stand_mean_ht = stand_mean_ht, stand_top_ht = stand_top_ht, equivalent_mean_ht = equivalent_mean_ht,
                          stand_mean_dbh = stand_mean_dbh, stand_cr_width = stand_cr_width, stand_cr_depth = stand_cr_depth,
                          c_drag = c_drag, n_drag = n_drag, drag_upper_limit = drag_upper_limit, spacing_current = spacing_current,
                          spacing_before = spacing_before, sph_current = sph_current, sph_before = sph_before, years_since_thin = years_since_thin, soil_group = soil_group, rooting = rooting,
                          weib_a = weib_a, weib_k = weib_k, dist_edge = dist_edge, gap_size = gap_size, edge_gap_factor = edge_gap_factor, elev_ht = elev_ht,
                          moe = moe, mor = mor, fknot = fknot, ro = ro, x = x, y = y, z = z, dams = dams, Warnings = fgr_warnings)


  fg_tmc_out_short <-   list(stand_id = stand_id, tree_id = tree_id, date = date, species = species, u10_damage = u10_damage, mode_of_damage = mode_of_damage,
                             u10_b = u10_b, u10_o = u10_o, prob_damage = prob_damage, prob_b = prob_b, prob_o = prob_o, Warnings = fgr_warnings)



  if(full_output == 1) {
    fg_tmc_out <- fg_tmc_out_full
  } else fg_tmc_out <- fg_tmc_out_short

  return(fg_tmc_out)
}
