#' Calculate the Deflection Loading Factor to account for the additional moment provided by the weight of the stem, crown (and snow, when present)
#' @param bm The applied bending moment resulting from the wind load
#' @param ht The height (m) of a tree.
#' @param dbh The dbh (cm) of a tree.
#' @param cr_depth The length (m) of the tree crown.
#' @param cr_width The width (m) of the tree crown.
#' @param moe Modulus of Elasticity (MPa) of green wood.
#' @param stem_density Density (kg m-3) of green wood of the stem.
#' @param crown_density Density (kgm-3) of of the crown of the mean tree in the stand.
#' @param stem_density Density (kg m-3) of green wood of the stem.
#' @param stem_vol Volume (m3) of the stem of the mean tree in the stand.
#' @param snow_depth Depth (cm) of layer of snow on tree crown.
#' @param snow_density Density (kg m-3) of snow.
#' @param x Height (m) along the tree stem where deflection is to be calculated.
#' @param lever_arm Length (m) of the lever arm. Typically, it is equal tree height.
#' @param fow Applied wind loading. In the \code{deflection_fun} function, it is a placeholder for the output of the \code{force_of_wind_fun} function.
#' @param pull_height Height (m) along the tree stem where the wind loading is applied.
#' @name DFL_Functions
#' @title Deflection Loading Factor Functions
NULL

#' @rdname DFL_Functions
dlf_fun <- function (bm, ht, cr_depth, cr_width, stem_vol, dbh, moe, crown_density, stem_density, snow_depth, snow_density) {
  DLF_calc <- 1 / (1 - ((deflection_fun(cr_depth/2, ht, force_of_wind_fun(bm, ht, cr_depth), dbh, ht, cr_depth, (ht - cr_depth/2), moe) * #Displacement at mid-crown height
                           ((((pi * cr_depth * (cr_width/2)^2)/3) * crown_density) + (pi * (cr_width/2)^2 * snow_depth *snow_density)) * fgr_constants$grav) + #weight of crown and snow
                          deflection_fun(3/4 * ht, ht, force_of_wind_fun(bm, ht, cr_depth), dbh, ht, cr_depth, (ht - cr_depth/2), moe) * #deflection ¾ of the way down the stem.  Assumption that centre of stem mass is at this height
                          stem_vol * stem_density * fgr_constants$grav) / #weight of stem
                     bm) #Note that BM is in both numerator and denominator, so the magnitude of the BM does not have any impact on DLF. It was decided to be kept in the equation for completeness.
  return(DLF_calc)
}

#' @rdname DFL_Functions
deflection_fun <- function(x, lever_arm, fow, dbh, ht, cr_depth, pull_height, moe) { #Gardiner 1989 'Mechanical characteristics of Sitka Spruce' Eq. 5: deflection at distance x from top of tree. fow is ForceOfWind
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
