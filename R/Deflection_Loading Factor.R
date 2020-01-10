#' Calculate the Deflection Loading Factor to account for the additional moment provided by the weight of the stem, crown (and snow, when present)
#' @param bm Applied bending moment resulting from the wind load (N).
#' @param ht Tree height. Depending on the method used ('roughness' or 'TMC'), this can be either the mean tree in the stand, or each individual tree (m).
#' @param dbh Diameter of the stem at breast height, i.e. 1.3m above the ground (cm). Depending on the method used ('roughness' or 'TMC') this can be either the arithmetic average of the dbh of all the trees in the stand, or the dbh of an individual tree.
#' @param cr_depth Length of the tree crown (m).
#' @param cr_width Width of the tree crown (m).
#' @param moe Modulus of Elasticity of green wood (MPa).
#' @param stem_density Density of green wood of the stem (kg m-3).
#' @param crown_density Density of of the tree crown (kg m-3).
#' @param stem_density Density of green wood of the stem (kg m-3).
#' @param stem_vol Volume of the tree stem of the mean tree in the stand (m3). For the roughness method, this is stem volume of the mean tree. For the TMC method, this is individual tree stem voume.
#' @param snow_depth Depth of layer of snow on tree crown (cm).
#' @param snow_density Density of snow (kg m-3).
#' @param x Height along the tree stem where deflection is to be calculated (m).
#' @param lever_arm Length of the lever arm (m). Typically, it is equal tree height.
#' @param fow Applied wind loading in the \code{deflection_fun} function. It is a placeholder for the output of the \code{force_of_wind_fun} function.
#' @param pull_height Height along the tree stem where the wind loading is applied (m).
#' @name DFL_Functions
#' @title Deflection Loading Factor Functions
NULL

#' @rdname DFL_Functions
dlf_fun <- function (bm, ht, cr_depth, cr_width, stem_vol, dbh, moe, crown_density, stem_density, snow_depth, snow_density, fgr_constants) {
  DLF_calc <- 1 / (1 - ((deflection_fun(cr_depth/2, ht, force_of_wind_fun(bm, ht, cr_depth), dbh, ht, cr_depth, (ht - cr_depth/2), moe, fgr_constants) * #Displacement at mid-crown height
                           ((((pi * cr_depth * (cr_width/2)^2)/3) * crown_density) + (pi * (cr_width/2)^2 * snow_depth *snow_density)) * fgr_constants$grav) + #weight of crown and snow
                          deflection_fun(3/4 * ht, ht, force_of_wind_fun(bm, ht, cr_depth), dbh, ht, cr_depth, (ht - cr_depth/2), moe, fgr_constants) * #deflection ¾ of the way down the stem.  Assumption that centre of stem mass is at this height
                          stem_vol * stem_density * fgr_constants$grav) / #weight of stem
                     bm) #Note that BM is in both numerator and denominator, so the magnitude of the BM does not have any impact on DLF. It was decided to be kept in the equation for completeness.
  return(DLF_calc)
}

#' @rdname DFL_Functions
deflection_fun <- function(x, lever_arm, fow, dbh, ht, cr_depth, pull_height, moe, fgr_constants) { #Gardiner 1989 'Mechanical characteristics of Sitka Spruce' Eq. 5: deflection at distance x from top of tree. fow is ForceOfWind
  hh <- fgr_constants$an*fgr_constants$bn*fgr_constants$cn
  i_value <- i_fun(dbh, ht, cr_depth)
  deflection <- ((fow*lever_arm^3) / (moe* i_value *hh)) * (fgr_constants$an*(x/lever_arm)^fgr_constants$cn -
                                                              r_fun(lever_arm, pull_height) * fgr_constants$cn*(x/lever_arm)^fgr_constants$bn +
                                                              r_fun(lever_arm, pull_height) * fgr_constants$bn*fgr_constants$cn*(x/lever_arm) -
                                                              fgr_constants$an*fgr_constants$cn*(x/lever_arm) + fgr_constants$an*fgr_constants$bn -
                                                              r_fun(lever_arm, pull_height) *fgr_constants$an*fgr_constants$cn)
  return(deflection)
}

#' @rdname DFL_Functions
force_of_wind_fun <- function (bm, ht, cr_depth) {
  force_of_wind <- bm / (ht - cr_depth/2)
  return(force_of_wind)
}

#' @rdname DFL_Functions
r_fun <- function(lever_arm, pull_height) { #Ratio of distance from top of tree where force is applied to tree tree height (ht)
  r_value <- (lever_arm - pull_height) / lever_arm
  return(r_value)
}

#' @rdname DFL_Functions
i_fun <- function(dbh, ht, cr_depth) { #Second area moment of Inertia calculated at tree base:
  i_value <- pi * ((diam_base_fun(dbh/100, ht, cr_depth))^4 / 64) # dbase in m in this formula
  return(i_value)
}
